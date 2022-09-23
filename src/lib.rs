#![no_std]
#![feature(adt_const_params)]
#![feature(associated_type_defaults)]
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

use core::any::TypeId;
use core::intrinsics::{const_allocate, const_deallocate};

const fn eq_typeid(a: TypeId, b: TypeId) -> bool {
    unsafe { core::mem::transmute::<_, u64>(a) == core::mem::transmute::<_, u64>(b) }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ConstValue {
    ty: TypeId,
    bytes: &'static [u8],
}

impl ConstValue {
    pub const fn new<T>(value: T) -> Self
    where
        T: 'static,
        T: Eq,
    {
        let bytes = unsafe {
            let ptr = const_allocate(core::mem::size_of::<T>(), core::mem::align_of::<T>());
            core::ptr::write(ptr.cast(), value);
            core::slice::from_raw_parts_mut(ptr.cast(), core::mem::size_of::<T>())
        };

        Self {
            ty: TypeId::of::<T>(),
            bytes,
        }
    }

    pub const fn into_inner<T>(self) -> T
    where
        T: 'static,
    {
        assert!(eq_typeid(TypeId::of::<T>(), self.ty));
        unsafe { core::ptr::read(self.bytes.as_ptr().cast()) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ConstVariable {
    key: TypeId,
    val: ConstValue,
}

#[derive(PartialEq, Eq)]
pub struct ConstVariables(&'static [ConstVariable]);

impl ConstVariables {
    const fn slice_allocate(size: usize) -> &'static mut [ConstVariable] {
        unsafe {
            let ptr = const_allocate(
                core::mem::size_of::<ConstVariable>() * size,
                core::mem::align_of::<ConstVariable>(),
            );
            core::slice::from_raw_parts_mut(ptr.cast(), size)
        }
    }

    const fn slice_deallocate(vars: &'static [ConstVariable]) {
        unsafe {
            const_deallocate(
                vars.as_ptr().cast_mut().cast(),
                core::mem::size_of::<ConstVariable>() * vars.len(),
                core::mem::align_of::<ConstVariable>(),
            )
        };
    }

    const fn slice_find(vars: &'static [ConstVariable], key: TypeId) -> Option<ConstVariable> {
        let mut i = 0;
        while i < vars.len() {
            if eq_typeid(vars[i].key, key) {
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
            if eq_typeid(vars[i].key, var.key) {
                panic!("")
            }

            new[i] = vars[i];
            i += 1;
        }

        Self::slice_deallocate(vars);

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
            if eq_typeid(vars[i].key, var.key) {
                new[i] = var;
            } else {
                new[i] = vars[i];
            }

            i += 1;
        }

        Self::slice_deallocate(vars);

        new
    }

    const fn slice_assign(
        vars: &'static [ConstVariable],
        var: ConstVariable,
    ) -> &'static [ConstVariable] {
        if Self::slice_find(vars, var.key).is_some() {
            Self::slice_reassign(vars, var)
        } else {
            Self::slice_push(vars, var)
        }
    }

    pub const fn empty() -> Self {
        Self(&[])
    }

    pub const fn assign<Key>(self, value: ConstValue) -> Self
    where
        Key: 'static,
    {
        Self(Self::slice_assign(
            self.0,
            ConstVariable {
                key: TypeId::of::<Key>(),
                val: value,
            },
        ))
    }

    pub const fn map<Key, Map>(self) -> Self
    where
        Key: 'static,
        Map: ~const ConstVariableMapper,
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
        Self::slice_find(self.0, TypeId::of::<Key>())
            .unwrap()
            .val
            .into_inner::<ValueTy>()
    }
}

pub struct ConstContext<const VARS: ConstVariables>;

impl ConstContext<{ ConstVariables::empty() }> {
    pub const fn empty() -> Self {
        Self
    }
}

impl<const VARS: ConstVariables> ConstContext<VARS> {
    #[must_use]
    pub const fn get<Key: 'static, ValueTy: 'static>(&self) -> ValueTy {
        VARS.get::<Key, ValueTy>()
    }

    #[must_use]
    pub const fn assign<Key: 'static, const VALUE: ConstValue>(
        self,
    ) -> ConstContext<{ VARS.assign::<Key>(VALUE) }> {
        ConstContext
    }

    #[must_use]
    pub const fn map_var<Key, Map>(&self) -> ConstContext<{ VARS.map::<Key, Map>() }>
    where
        Key: 'static,
        Map: ~const ConstVariableMapper,
    {
        ConstContext
    }

    #[must_use]
    pub const fn map<Map>(&self) -> ConstContext<{ Map::map(VARS) }>
    where
        Map: ~const ConstContextMapper,
    {
        ConstContext
    }
}

#[const_trait]
pub trait ConstVariableMapper {
    type Input: 'static + Eq;
    type Output: 'static + Eq;
    fn map(value: Self::Input) -> Self::Output;
}

#[const_trait]
pub trait ConstContextMapper {
    fn map(vars: ConstVariables) -> ConstVariables;
}
