#![feature(adt_const_params)]
#![feature(const_heap)]
#![feature(const_mut_refs)]
#![feature(const_option)]
#![feature(const_ptr_read)]
#![feature(const_ptr_write)]
#![feature(const_slice_from_raw_parts_mut)]
#![feature(const_trait_impl)]
#![feature(const_type_id)]
#![feature(core_intrinsics)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]
#![feature(inline_const_pat)]

use core::any::TypeId;
use core::intrinsics::const_allocate;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ConstVar {
    key: u64,
    value_ty: u64,
    value_bytes: &'static [u8],
}

impl ConstVar {
    pub const fn new<Key, ValueTy>(value: ValueTy) -> Self
    where
        Key: 'static,
        ValueTy: 'static,
    {
        let key = unsafe { core::mem::transmute::<_, u64>(TypeId::of::<Key>()) };
        let value_ty = unsafe { core::mem::transmute::<_, u64>(TypeId::of::<ValueTy>()) };

        let value_bytes = unsafe {
            let ptr = const_allocate(
                core::mem::size_of::<ValueTy>(),
                core::mem::align_of::<ValueTy>(),
            );
            core::ptr::write(ptr.cast(), value);
            core::slice::from_raw_parts_mut(ptr.cast(), core::mem::size_of::<ValueTy>())
        };

        Self {
            key,
            value_ty,
            value_bytes,
        }
    }

    const fn into_inner<T>(self) -> T
    where
        T: 'static,
    {
        assert!(unsafe { core::mem::transmute::<_, u64>(TypeId::of::<T>()) == self.value_ty });
        unsafe { core::ptr::read(self.value_bytes.as_ptr().cast()) }
    }
}

#[derive(PartialEq, Eq)]
pub struct ConstVars(&'static [ConstVar]);

impl ConstVars {
    const fn slice_allocate(size: usize) -> &'static mut [ConstVar] {
        let ptr = unsafe {
            const_allocate(
                core::mem::size_of::<ConstVar>() * size,
                core::mem::align_of::<ConstVar>(),
            )
        };
        unsafe { core::slice::from_raw_parts_mut(ptr.cast(), size) }
    }

    const fn slice_find(vars: &'static [ConstVar], key: u64) -> Option<ConstVar> {
        let mut i = 0;
        while i < vars.len() {
            if vars[i].key == key {
                return Some(vars[i]);
            }
            i += 1;
        }
        None
    }

    const fn slice_push(vars: &'static [ConstVar], var: ConstVar) -> &'static [ConstVar] {
        let new = Self::slice_allocate(vars.len() + 1);

        let mut i = 0;
        while i < vars.len() {
            if vars[i].key == var.key {
                panic!("")
            }

            new[i] = vars[i];
            i += 1;
        }

        new[i] = var;
        new
    }

    const fn slice_reassign(vars: &'static [ConstVar], var: ConstVar) -> &'static [ConstVar] {
        let new = Self::slice_allocate(vars.len());

        let mut i = 0;
        while i < vars.len() {
            if vars[i].key == var.key {
                new[i] = var;
            } else {
                new[i] = vars[i];
            }

            i += 1;
        }

        new
    }

    const fn slice_assign(vars: &'static [ConstVar], var: ConstVar) -> &'static [ConstVar] {
        if Self::slice_find(vars, var.key).is_some() {
            Self::slice_reassign(vars, var)
        } else {
            Self::slice_push(vars, var)
        }
    }

    pub const fn empty() -> Self {
        Self(Self::slice_allocate(0))
    }

    pub const fn assign(self, var: ConstVar) -> Self {
        Self(Self::slice_assign(self.0, var))
    }

    pub const fn map<Key, Map>(self) -> Self
    where
        Key: 'static,
        Map: ~const ConstVarMap,
    {
        let value = self.get::<Key, Map::Input>();
        self.assign(ConstVar::new::<Key, Map::Output>(Map::map(value)))
    }

    pub const fn get<Key, ValueTy>(&self) -> ValueTy
    where
        Key: 'static,
        ValueTy: 'static,
    {
        let key = unsafe { core::mem::transmute::<_, u64>(TypeId::of::<Key>()) };
        Self::slice_find(self.0, key)
            .unwrap()
            .into_inner::<ValueTy>()
    }
}

pub struct ConstEnv<const VARS: ConstVars>;

impl ConstEnv<{ ConstVars::empty() }> {
    pub const fn empty() -> Self {
        Self
    }
}

impl<const VARS: ConstVars> ConstEnv<VARS> {
    pub const fn get<Key: 'static, ValueTy: 'static>(&self) -> ValueTy {
        VARS.get::<Key, ValueTy>()
    }

    pub const fn assign<const VAR: ConstVar>(self) -> ConstEnv<{ VARS.assign(VAR) }> {
        ConstEnv
    }

    pub const fn map<Key, Map>(&self) -> ConstEnv<{ VARS.map::<Key, Map>() }>
    where
        Key: 'static,
        Map: ~const ConstVarMap,
    {
        ConstEnv
    }
}

#[const_trait]
pub trait ConstVarMap {
    type Input: 'static;
    type Output: 'static;
    fn map(value: Self::Input) -> Self::Output;
}
