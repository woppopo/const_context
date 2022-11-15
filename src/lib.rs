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
#![feature(inherent_associated_types)]

use core::any::TypeId;
use core::intrinsics::const_allocate;
use core::marker::PhantomData;

const fn eq_typeid(a: TypeId, b: TypeId) -> bool {
    unsafe { core::mem::transmute::<_, u64>(a) == core::mem::transmute::<_, u64>(b) }
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

pub struct VarListEnd;

pub struct VarList<Key, const VAL: ConstValue, Next>(PhantomData<(Key, Next)>);

pub trait Search<Key> {
    const FOUND: Option<ConstValue>;
}

impl<Key> Search<Key> for VarListEnd {
    const FOUND: Option<ConstValue> = None;
}

impl<Key, Hold, const VAL: ConstValue, Next: Search<Key>> Search<Key> for VarList<Hold, VAL, Next>
where
    Key: 'static,
    Hold: 'static,
{
    const FOUND: Option<ConstValue> = if eq_typeid(TypeId::of::<Key>(), TypeId::of::<Hold>()) {
        Some(VAL)
    } else {
        Next::FOUND
    };
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

pub struct ConstContext<Vars>(PhantomData<Vars>);

impl ConstContext<VarListEnd> {
    pub const fn empty() -> Self {
        Self(PhantomData)
    }
}

impl<Vars> ConstContext<Vars> {
    type Push<Var: ConstVariable, const VAL: ConstValue> =
        ConstContext<VarList<Var::Key, VAL, Vars>>;

    pub const fn into<NextVars>(self) -> ConstContext<NextVars> {
        ConstContext(PhantomData)
    }

    pub const fn get_from_type<Var>() -> Var::Value
    where
        Var: ConstVariable,
        Vars: Search<Var::Key>,
    {
        <Self as ConstContextGet<Var>>::OUTPUT
    }

    pub const fn get<Var>(&self) -> Var::Value
    where
        Var: ConstVariable,
        Vars: Search<Var::Key>,
    {
        <Self as ConstContextGet<Var>>::OUTPUT
    }

    pub const fn push<Var, const VAL: ConstValue>(
        self,
    ) -> <ConstContext<Vars> as ConstContextPush<Var, VAL>>::Output
    where
        Var: ConstVariable,
    {
        self.into()
    }
}

pub trait ConstContextGet<Var>
where
    Var: ConstVariable,
{
    const OUTPUT: Var::Value;
}

impl<Vars, Var> ConstContextGet<Var> for ConstContext<Vars>
where
    Var: ConstVariable,
    Vars: Search<Var::Key>,
{
    const OUTPUT: Var::Value = Vars::FOUND.unwrap().into_inner();
}

pub trait ConstContextPush<Var, const VAL: ConstValue>
where
    Var: ConstVariable,
{
    type Output;
}

impl<Vars, Var, const VAL: ConstValue> ConstContextPush<Var, VAL> for ConstContext<Vars>
where
    Var: ConstVariable,
{
    type Output = ConstContext<VarList<Var::Key, VAL, Vars>>;
}

pub trait Action<Vars> {
    type Output;
    fn eval(self) -> Self::Output;
}

impl<Input> Action<Input> for () {
    type Output = ();
    fn eval(self) -> Self::Output {}
}

impl<Input, F, T> Action<Input> for F
where
    F: FnOnce() -> T,
{
    type Output = T;
    fn eval(self) -> Self::Output {
        self()
    }
}

impl<Input, Output, F, T, C, Next> Action<Input> for (F, C)
where
    F: FnOnce(ConstContext<Input>) -> (ConstContext<Output>, T),
    C: FnOnce(T) -> Next,
    Next: Action<Output>,
{
    type Output = Next::Output;
    fn eval(self) -> Self::Output {
        let (_, arg) = self.0(ConstContext(PhantomData));
        let next = self.1(arg);
        next.eval()
    }
}

pub struct ConstVariableGet<Var: ConstVariable>(PhantomData<Var>);
pub struct ConstVariableAssign<Var: ConstVariable, const VALUE: ConstValue>(PhantomData<Var>);

impl<Input, Var: ConstVariable, const VALUE: ConstValue, F, Next> Action<Input>
    for (PhantomData<ConstVariableAssign<Var, VALUE>>, F)
where
    F: FnOnce() -> Next,
    Next: Action<VarList<Var::Key, VALUE, Input>>,
{
    type Output = Next::Output;
    fn eval(self) -> Self::Output {
        let next = (self.1)();
        next.eval()
    }
}

impl<Input, Var: ConstVariable, F, Next> Action<Input> for (PhantomData<ConstVariableGet<Var>>, F)
where
    F: FnOnce(Var::Value) -> Next,
    Next: Action<Input>,
    Input: Search<Var::Key>,
{
    type Output = Next::Output;
    fn eval(self) -> Self::Output {
        let next = (self.1)(<ConstContext<Input> as ConstContextGet<Var>>::OUTPUT);
        next.eval()
    }
}

#[macro_export]
macro_rules! ctx {
    () => {{
        || ()
    }};
    (pure $e:expr) => {{
        move || $e
    }};
    ($cvar:ty) => {{
        type __Get = ConstVariableGet<$cvar>;
        type __Value = <$cvar as ConstVariable>::Value;
        (PhantomData::<__Get>, move |var: __Value| move || var)
    }};
    (let $var:ident = $e:expr; $($rem:tt)*) => {{
        let $var = $e;
        ctx! { $($rem)* }
    }};
    ($cvar:ty = $e:expr; $($rem:tt)*) => {{
        type __Value = <$cvar as ConstVariable>::Value;
        type __Assign = ConstVariableAssign<$cvar, { ConstValue::new::<__Value>($e) }>;
        (PhantomData::<__Assign>, move || { ctx! { $($rem)* } })
    }};
    ($func:ident($($arg:expr)*); $($rem:tt)* ) => {{
        (|ctx| $func(ctx), move |_| { ctx!($($rem)*) })
    }};
    ($var:ident <= $func:ident($($arg:expr)*); $($rem:tt)* ) => {{
        (|ctx| $func(ctx), move |$var| { ctx!($($rem)*) })
    }};
    ($var:ident <= $cvar:ty; $($rem:tt)* ) => {{
        type __Get = ConstVariableGet<$cvar>;
        type __Value = <$cvar as ConstVariable>::Value;
        (PhantomData::<__Get>, move |$var: __Value| { ctx!($($rem)*) })
    }};
}

#[test]
#[cfg(test)]
fn test() {
    type Var1 = ((), u32);
    const fn f<Vars>(ctx: ConstContext<Vars>) -> (ConstContext<Vars>, u32) {
        (ctx, 42)
    }

    let action = ctx! {
        Var1 = 90;
        Var1
    };

    let action2 = ctx! {
        v <= f();
        pure v
    };

    let action3 = ctx! {
        Var1 = 90;
        v <= f();
        w <= Var1;
        pure (v + w)
    };

    assert_eq!(Action::<VarListEnd>::eval(action), 90);
    assert_eq!(Action::<VarListEnd>::eval(action2), 42);
    assert_eq!(Action::<VarListEnd>::eval(action3), 132);
}
