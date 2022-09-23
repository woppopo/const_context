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

pub trait ConstVariable {
    type Key: 'static;
    type Value: 'static + Eq;
}

impl<K, V> ConstVariable for (K, V)
where
    K: 'static,
    V: 'static + Eq,
{
    type Key = K;
    type Value = V;
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
struct VarData {
    key: TypeId,
    val: ConstValue,
}

#[derive(PartialEq, Eq)]
pub struct ConstVariables(&'static [VarData]);

impl ConstVariables {
    const fn slice_allocate(size: usize) -> &'static mut [VarData] {
        unsafe {
            let ptr = const_allocate(
                core::mem::size_of::<VarData>() * size,
                core::mem::align_of::<VarData>(),
            );
            core::slice::from_raw_parts_mut(ptr.cast(), size)
        }
    }

    const fn slice_deallocate(vars: &'static [VarData]) {
        unsafe {
            const_deallocate(
                vars.as_ptr().cast_mut().cast(),
                core::mem::size_of::<VarData>() * vars.len(),
                core::mem::align_of::<VarData>(),
            )
        };
    }

    const fn slice_find(vars: &'static [VarData], key: TypeId) -> Option<VarData> {
        let mut i = 0;
        while i < vars.len() {
            if eq_typeid(vars[i].key, key) {
                return Some(vars[i]);
            }
            i += 1;
        }
        None
    }

    const fn slice_push(vars: &'static [VarData], var: VarData) -> &'static [VarData] {
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

    const fn slice_reassign(vars: &'static [VarData], var: VarData) -> &'static [VarData] {
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

    const fn slice_assign(vars: &'static [VarData], var: VarData) -> &'static [VarData] {
        if Self::slice_find(vars, var.key).is_some() {
            Self::slice_reassign(vars, var)
        } else {
            Self::slice_push(vars, var)
        }
    }

    pub const fn empty() -> Self {
        Self(&[])
    }

    pub const fn assign<Var>(self, value: ConstValue) -> Self
    where
        Var: ConstVariable,
    {
        assert!(eq_typeid(TypeId::of::<Var::Value>(), value.ty));

        Self(Self::slice_assign(
            self.0,
            VarData {
                key: TypeId::of::<Var::Key>(),
                val: value,
            },
        ))
    }

    pub const fn map<Var, Map>(self) -> Self
    where
        Var: ConstVariable,
        Map: ~const ConstVariableMapper<Var>,
    {
        let value = self.get::<Var>();
        let value = Map::map(value);
        let value = ConstValue::new(value);
        self.assign::<Var>(value)
    }

    pub const fn get<Var>(&self) -> Var::Value
    where
        Var: ConstVariable,
    {
        Self::slice_find(self.0, TypeId::of::<Var::Key>())
            .unwrap()
            .val
            .into_inner::<Var::Value>()
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
    pub const fn get<Var>(&self) -> Var::Value
    where
        Var: ConstVariable,
    {
        VARS.get::<Var>()
    }

    #[must_use]
    pub const fn assign<Var, const VALUE: ConstValue>(
        self,
    ) -> ConstContext<{ VARS.assign::<Var>(VALUE) }>
    where
        Var: ConstVariable,
    {
        ConstContext
    }

    #[must_use]
    pub const fn map_var<Var, Map>(&self) -> ConstContext<{ VARS.map::<Var, Map>() }>
    where
        Var: ConstVariable,
        Map: ~const ConstVariableMapper<Var>,
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
pub trait ConstVariableMapper<Var: ConstVariable> {
    fn map(value: Var::Value) -> Var::Value;
}

#[const_trait]
pub trait ConstContextMapper {
    fn map(vars: ConstVariables) -> ConstVariables;
}
