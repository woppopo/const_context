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
        Map: ~const ConstVarMap,
    {
        let value = self.get::<Key, Map::Input>();
        let value = Map::map_var(value);
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

pub struct ConstEnv<const VARS: ConstVariables>;

impl ConstEnv<{ ConstVariables::empty() }> {
    pub const fn empty() -> Self {
        Self
    }
}

impl<const VARS: ConstVariables> ConstEnv<VARS> {
    #[must_use]
    pub const fn get<Key: 'static, ValueTy: 'static>(&self) -> ValueTy {
        VARS.get::<Key, ValueTy>()
    }

    #[must_use]
    pub const fn assign<Key: 'static, const VALUE: ConstValue>(
        self,
    ) -> ConstEnv<{ VARS.assign::<Key>(VALUE) }> {
        ConstEnv
    }

    #[must_use]
    pub const fn map<Key, Map>(&self) -> ConstEnv<{ VARS.map::<Key, Map>() }>
    where
        Key: 'static,
        Map: ~const ConstVarMap,
    {
        ConstEnv
    }

    #[must_use]
    pub const fn map_env<Map>(&self) -> ConstEnv<{ Map::map_env(VARS) }>
    where
        Map: ~const ConstEnvMap,
    {
        ConstEnv
    }
}

#[const_trait]
pub trait ConstVarMap {
    type Input: 'static;
    type Output: 'static;
    fn map_var(value: Self::Input) -> Self::Output;
}

#[const_trait]
pub trait ConstEnvMap {
    fn map_env(vars: ConstVariables) -> ConstVariables;
}

/*
pub trait ConstEnvAbstract {
    const VARS: ConstVariables;

    type New<const NEW: ConstVariables>: ConstEnvAbstract;

    type Assign<Key, const VALUE: ConstValue>: ConstEnvAbstract = Self::New<{ Self::VARS.assign::<Key>(VALUE) }>
    where
        Key: 'static,
        Self::New<{ Self::VARS.assign::<Key>(VALUE) }>:;

    type Map<Key, Map>: ConstEnvAbstract = Self::New<{ Self::VARS.map::<Key, Map>() }>
    where
        Key: 'static,
        Map: ~const ConstVarMap,
        Self::New<{ Self::VARS.map::<Key, Map>() }>:;

    type MapEnv<Map>: ConstEnvAbstract = Self::New<{ Map::map_env(Self::VARS) }>
    where
        Map: ~const ConstEnvMap,
        Self::New<{ Map::map_env(Self::VARS) }>:;

    #[must_use]
    fn assign<Key: 'static, const VALUE: ConstValue>(self) -> Self::Assign<Key, VALUE>
    where
        Key: 'static,
        Self::New<{ Self::VARS.assign::<Key>(VALUE) }>:;

    #[must_use]
    fn map<Key, Map>(self) -> Self::Map<Key, Map>
    where
        Key: 'static,
        Map: ~const ConstVarMap,
        Self::New<{ Self::VARS.map::<Key, Map>() }>:;

    #[must_use]
    fn map_env<Map>(self) -> Self::MapEnv<Map>
    where
        Map: ~const ConstEnvMap,
        Self::New<{ Map::map_env(Self::VARS) }>:;
}

impl<const VARS: ConstVariables> const ConstEnvAbstract for ConstEnv<VARS> {
    const VARS: ConstVariables = VARS;
    type New<const NEW: ConstVariables> = ConstEnv<NEW>;

    fn assign<Key: 'static, const VALUE: ConstValue>(self) -> Self::Assign<Key, VALUE>
    where
        Key: 'static,
        Self::New<{ Self::VARS.assign::<Key>(VALUE) }>:,
    {
        ConstEnv
    }

    fn map<Key, Map>(self) -> Self::Map<Key, Map>
    where
        Key: 'static,
        Map: ~const ConstVarMap,
        Self::New<{ Self::VARS.map::<Key, Map>() }>:,
    {
        ConstEnv
    }

    fn map_env<Map>(self) -> Self::MapEnv<Map>
    where
        Map: ~const ConstEnvMap,
        Self::New<{ Map::map_env(Self::VARS) }>:,
    {
        ConstEnv
    }
}
*/
