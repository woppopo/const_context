#![cfg_attr(not(test), no_std)]
#![feature(adt_const_params)]
#![feature(associated_type_defaults)]
#![feature(const_heap)]
#![feature(const_mut_refs)]
#![feature(const_option)]
#![feature(const_ptr_read)]
#![feature(const_ptr_write)]
#![feature(const_slice_from_raw_parts_mut)]
#![feature(const_type_id)]
#![feature(core_intrinsics)]
#![feature(decl_macro)]
#![feature(generic_const_exprs)]
#![feature(type_alias_impl_trait)]

use core::any::TypeId;
use core::intrinsics::{const_allocate, const_deallocate};
use core::marker::PhantomData;

macro const_for($i:ident in ($s:expr, $e:expr) $b:block) {{
    let mut $i = $s;
    while $i < $e {
        $b;
        $i += 1;
    }
}}

const fn eq_typeid(a: TypeId, b: TypeId) -> bool {
    unsafe { core::mem::transmute::<_, u64>(a) == core::mem::transmute::<_, u64>(b) }
}

const fn slice_allocate<T>(size: usize) -> &'static mut [T] {
    unsafe {
        let ptr = const_allocate(core::mem::size_of::<T>() * size, core::mem::align_of::<T>());
        core::slice::from_raw_parts_mut(ptr.cast(), size)
    }
}

const fn slice_deallocate<T>(vars: &'static [T]) {
    unsafe {
        const_deallocate(
            vars.as_ptr().cast_mut().cast(),
            core::mem::size_of::<T>() * vars.len(),
            core::mem::align_of::<T>(),
        )
    };
}

const fn into_bytes<T>(value: T) -> &'static mut [u8] {
    unsafe {
        let ptr = const_allocate(core::mem::size_of::<T>(), core::mem::align_of::<T>());
        core::ptr::write(ptr.cast(), value);
        core::slice::from_raw_parts_mut(ptr.cast(), core::mem::size_of::<T>())
    }
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
        Self {
            ty: TypeId::of::<T>(),
            bytes: into_bytes(value),
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
struct VarEntry {
    key: TypeId,
    val: ConstValue,
}

#[derive(PartialEq, Eq)]
pub struct ConstVariables(&'static [VarEntry]);

impl ConstVariables {
    pub const fn empty() -> Self {
        Self(&[])
    }

    const fn find(vars: &'static [VarEntry], key: TypeId) -> Option<VarEntry> {
        const_for!(i in (0, vars.len()) {
            if eq_typeid(vars[i].key, key) {
                return Some(vars[i]);
            }
        });
        None
    }

    const fn push(vars: &'static [VarEntry], var: VarEntry) -> &'static [VarEntry] {
        let new = slice_allocate(vars.len() + 1);

        const_for!(i in (0, vars.len()) {
            if eq_typeid(vars[i].key, var.key) {
                panic!("")
            }

            new[i] = vars[i];
        });

        slice_deallocate(vars);

        new[vars.len()] = var;
        new
    }

    const fn reassign(vars: &'static [VarEntry], var: VarEntry) -> &'static [VarEntry] {
        let new = slice_allocate(vars.len());

        const_for!(i in (0, vars.len()) {
            if eq_typeid(vars[i].key, var.key) {
                new[i] = var;
            } else {
                new[i] = vars[i];
            }
        });

        slice_deallocate(vars);

        new
    }

    pub const fn assign<Var>(self, value: ConstValue) -> Self
    where
        Var: ConstVariable,
    {
        assert!(eq_typeid(TypeId::of::<Var::Value>(), value.ty));

        let vars = self.0;
        let var = VarEntry {
            key: TypeId::of::<Var::Key>(),
            val: value,
        };

        Self(match Self::find(vars, var.key) {
            Some(_) => Self::reassign(vars, var),
            None => Self::push(vars, var),
        })
    }

    pub const fn get<Var>(&self) -> Var::Value
    where
        Var: ConstVariable,
    {
        Self::find(self.0, TypeId::of::<Var::Key>())
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
    pub const fn map<Map>(self) -> ConstContext<{ Map::OUTPUT }>
    where
        Map: ConstContextMap<VARS>,
    {
        ConstContext
    }
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

impl<V: ConstVariable> ConstVariable for &V {
    type Key = V::Key;
    type Value = V::Value;
}

pub trait ConstContextMap<const INPUT: ConstVariables> {
    const OUTPUT: ConstVariables;
}

pub struct ConstVariableGet<Var: ConstVariable>(PhantomData<Var>);

pub struct ConstVariableAssign<Var: ConstVariable, const VALUE: ConstValue>(PhantomData<Var>);

impl<Var: ConstVariable, const VALUE: ConstValue, const INPUT: ConstVariables>
    ConstContextMap<INPUT> for ConstVariableAssign<Var, VALUE>
{
    const OUTPUT: ConstVariables = INPUT.assign::<Var>(VALUE);
}

/*
trait Action<const INPUT: ConstVariables> {
    type Output;
    fn eval(self) -> Self::Output;
}

impl<const INPUT: ConstVariables> Action<INPUT> for () {
    type Output = ();
    fn eval(self) -> Self::Output {}
}

impl<const INPUT: ConstVariables, F, T> Action<INPUT> for F
where
    F: FnOnce() -> T,
{
    type Output = T;
    fn eval(self) -> Self::Output {
        self()
    }
}

impl<const INPUT: ConstVariables, Var: ConstVariable, const VALUE: ConstValue, F, Next>
    Action<INPUT> for (PhantomData<ConstVariableAssign<Var, VALUE>>, F)
where
    F: FnOnce() -> Next,
    Next: Action<{ <ConstVariableAssign<Var, VALUE> as ConstContextMap<INPUT>>::OUTPUT }>,
{
    type Output = Next::Output;
    fn eval(self) -> Self::Output {
        let next = (self.1)();
        next.eval()
    }
}

impl<const INPUT: ConstVariables, Var: ConstVariable, F, Next> Action<INPUT>
    for (PhantomData<ConstVariableGet<Var>>, F)
where
    F: FnOnce(Var::Value) -> Next,
    Next: Action<INPUT>,
{
    type Output = Next::Output;
    fn eval(self) -> Self::Output {
        let next = (self.1)(INPUT.get::<Var>());
        next.eval()
    }
}

macro_rules! ctx {
    (pure $e:expr) => { move || { $e } };
    ($var:ty) => { (PhantomData::<ConstVariableGet<$var>>, move |var| move || var) };
    (let $v:ident = $e:expr; $($rem:tt)*) => {{ let $v = $e; ctx!($($rem)*) }};
    ($var:ty = $e:expr; $($rem:tt)*) => { (PhantomData::<ConstVariableAssign<$var, { ConstValue::new($e as <$var as ConstVariable>::Value) }>>, move || { ctx!($($rem)*) }) };
    ($v:ident <= $var:ty; $($rem:tt)* ) => { (PhantomData::<ConstVariableGet<$var>>, move |$v| { ctx!($($rem)*) }) };
}

#[test]
#[cfg(test)]
fn test() {
    type Var1 = ((), u32);
    type Var2 = (u32, u32);
    type A = impl Action<{ ConstVariables::empty() }, Output = u32>;
    let action: A = ctx! {
        Var1 = 90;
        Var1
    };

    assert_eq!(action.eval(), 90);
}
*/
