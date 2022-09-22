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
pub struct ConstValue {
    ty: u64,
    bytes: &'static [u8],
}

impl ConstValue {
    pub const fn new<T>(value: T) -> Self
    where
        T: 'static,
    {
        let ty = unsafe { core::mem::transmute::<_, u64>(TypeId::of::<T>()) };
        let bytes = unsafe {
            let ptr = const_allocate(core::mem::size_of::<T>(), core::mem::align_of::<T>());
            core::ptr::write(ptr.cast(), value);
            core::slice::from_raw_parts_mut(ptr.cast(), core::mem::size_of::<T>())
        };

        Self { ty, bytes }
    }

    const fn into_inner<T>(self) -> T
    where
        T: 'static,
    {
        assert!(unsafe { core::mem::transmute::<_, u64>(TypeId::of::<T>()) == self.ty });
        unsafe { core::ptr::read(self.bytes.as_ptr().cast()) }
    }
}

type ConstVariable = (u64, ConstValue);

#[derive(PartialEq, Eq)]
pub struct ConstVariables(&'static [ConstVariable]);

impl ConstVariables {
    const fn slice_allocate(size: usize) -> &'static mut [ConstVariable] {
        let ptr = unsafe {
            const_allocate(
                core::mem::size_of::<ConstVariable>() * size,
                core::mem::align_of::<ConstVariable>(),
            )
        };
        unsafe { core::slice::from_raw_parts_mut(ptr.cast(), size) }
    }

    const fn slice_find(vars: &'static [ConstVariable], key: u64) -> Option<ConstVariable> {
        let mut i = 0;
        while i < vars.len() {
            if vars[i].0 == key {
                return Some(vars[i]);
            }
            i += 1;
        }
        None
    }

    const fn slice_push(
        vars: &'static [ConstVariable],
        var: ConstVariable,
    ) -> &'static [ConstVariable] {
        let new = Self::slice_allocate(vars.len() + 1);

        let mut i = 0;
        while i < vars.len() {
            if vars[i].0 == var.0 {
                panic!("")
            }

            new[i] = vars[i];
            i += 1;
        }

        new[i] = var;
        new
    }

    const fn slice_reassign(
        vars: &'static [ConstVariable],
        var: ConstVariable,
    ) -> &'static [ConstVariable] {
        let new = Self::slice_allocate(vars.len());

        let mut i = 0;
        while i < vars.len() {
            if vars[i].0 == var.0 {
                new[i] = var;
            } else {
                new[i] = vars[i];
            }

            i += 1;
        }

        new
    }

    const fn slice_assign(
        vars: &'static [ConstVariable],
        var: ConstVariable,
    ) -> &'static [ConstVariable] {
        if Self::slice_find(vars, var.0).is_some() {
            Self::slice_reassign(vars, var)
        } else {
            Self::slice_push(vars, var)
        }
    }

    pub const fn empty() -> Self {
        Self(Self::slice_allocate(0))
    }

    pub const fn assign<Key>(self, value: ConstValue) -> Self
    where
        Key: 'static,
    {
        let key = unsafe { core::mem::transmute::<_, u64>(TypeId::of::<Key>()) };
        Self(Self::slice_assign(self.0, (key, value)))
    }

    pub const fn map<Key, Map>(self) -> Self
    where
        Key: 'static,
        Map: ~const ConstMap,
    {
        let value = self.get::<Key, Map::Input>();
        let value = Map::map(value);
        let value = ConstValue::new(value);
        self.assign::<Key>(value)
    }

    pub const fn get<Key, ValueTy>(&self) -> ValueTy
    where
        Key: 'static,
        ValueTy: 'static,
    {
        let key = unsafe { core::mem::transmute::<_, u64>(TypeId::of::<Key>()) };
        Self::slice_find(self.0, key)
            .unwrap()
            .1
            .into_inner::<ValueTy>()
    }
}

pub struct ConstEnv<const VARS: ConstVariables>;

impl ConstEnv<{ ConstVariables::empty() }> {
    pub const fn empty() -> Self {
        Self
    }
}

impl<const VARS: ConstVariables> ConstEnv<VARS> {
    pub const fn get<Key: 'static, ValueTy: 'static>(&self) -> ValueTy {
        VARS.get::<Key, ValueTy>()
    }

    pub const fn assign<Key: 'static, const VALUE: ConstValue>(
        self,
    ) -> ConstEnv<{ VARS.assign::<Key>(VALUE) }> {
        ConstEnv
    }

    pub const fn map<Key, Map>(&self) -> ConstEnv<{ VARS.map::<Key, Map>() }>
    where
        Key: 'static,
        Map: ~const ConstMap,
    {
        ConstEnv
    }
}

#[const_trait]
pub trait ConstMap {
    type Input: 'static;
    type Output: 'static;
    fn map(value: Self::Input) -> Self::Output;
}
