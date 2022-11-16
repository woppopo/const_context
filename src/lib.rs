#![cfg_attr(not(test), no_std)]
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
    pub const fn get_runtime<Var>(&self) -> Var::Value
    where
        Var: ConstVariable,
        Vars: Search<Var::Key>,
    {
        <Self as ConstContextGet<Var>>::GOT
    }
}

#[const_trait]
pub trait ConstContextAbstract {
    type Vars;
    type Push<Var: ConstVariable, const VAL: ConstValue>: ConstContextAbstract;
    fn push<Var: ConstVariable, const VAL: ConstValue>(self) -> Self::Push<Var, VAL>;
}

impl<Vars> const ConstContextAbstract for ConstContext<Vars> {
    type Vars = Vars;
    type Push<Var: ConstVariable, const VAL: ConstValue> =
        ConstContext<VarList<Var::Key, VAL, Vars>>;

    fn push<Var: ConstVariable, const VAL: ConstValue>(self) -> Self::Push<Var, VAL> {
        ConstContext(PhantomData)
    }
}

pub trait ConstContextGet<Var>
where
    Var: ConstVariable,
{
    const GOT: Var::Value;
}

impl<Vars, Var> ConstContextGet<Var> for ConstContext<Vars>
where
    Var: ConstVariable,
    Vars: Search<Var::Key>,
{
    const GOT: Var::Value = Vars::FOUND.unwrap().into_inner();
}

pub trait Action<Vars> {
    type OutputVars;
    type Output;
    fn eval(self) -> Self::Output;
}

pub trait StartEvaluation {
    type Output;
    fn start_eval(self) -> Self::Output;
}

impl<T: Action<VarListEnd>> StartEvaluation for T {
    type Output = T::Output;

    fn start_eval(self) -> Self::Output {
        self.eval()
    }
}

pub struct ConstContextBindAction<PreviousAction, ActionConstructor>(
    PreviousAction,
    ActionConstructor,
);

impl<Input, PreviousAction, ActionConstructor, NextAction> Action<Input>
    for ConstContextBindAction<PreviousAction, ActionConstructor>
where
    PreviousAction: Action<Input>,
    ActionConstructor: FnOnce(PreviousAction::Output) -> NextAction,
    NextAction: Action<PreviousAction::OutputVars>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;
    fn eval(self) -> Self::Output {
        let Self(action, constructor) = self;
        let output = action.eval();
        constructor(output).eval()
    }
}

pub struct ConstContextReturnAction<T>(T);

impl<Input, T> Action<Input> for ConstContextReturnAction<T> {
    type OutputVars = Input;
    type Output = T;
    fn eval(self) -> Self::Output {
        self.0
    }
}

pub struct ConstContextGetAction<Variable, ActionConstructor>(
    PhantomData<Variable>,
    ActionConstructor,
);

impl<Input, Variable, ActionConstructor, NextAction> Action<Input>
    for ConstContextGetAction<Variable, ActionConstructor>
where
    Input: Search<Variable::Key>,
    Variable: ConstVariable,
    ActionConstructor: FnOnce(Variable::Value) -> NextAction,
    NextAction: Action<Input>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;
    fn eval(self) -> Self::Output {
        let Self(_, constructor) = self;
        let got = <ConstContext<Input> as ConstContextGet<Variable>>::GOT;
        constructor(got).eval()
    }
}

pub struct ConstContextAssignAction<Variable, const VALUE: ConstValue, NextAction>(
    PhantomData<Variable>,
    NextAction,
);

impl<Input, Variable, const VALUE: ConstValue, NextAction> Action<Input>
    for ConstContextAssignAction<Variable, VALUE, NextAction>
where
    Variable: ConstVariable,
    NextAction: Action<VarList<Variable::Key, VALUE, Input>>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;
    fn eval(self) -> Self::Output {
        let Self(_, action) = self;
        action.eval()
    }
}

#[macro_export]
macro_rules! ctx {
    () => {{
        ConstContextReturnAction(())
    }};
    (pure $e:expr) => {{
        ConstContextReturnAction($e)
    }};
    (get $cvar:ty) => {{
        ConstContextGetAction(PhantomData::<$cvar>, ConstContextReturnAction)
    }};
    ($action:expr) => {{
        $action
    }};
    (let $var:ident = $e:expr; $($rem:tt)*) => {{
        let $var = $e;
        ctx! { $($rem)* }
    }};
    (const $cvar:ty = $e:expr; $($rem:tt)*) => {{
        type __Value = <$cvar as ConstVariable>::Value;
        ConstContextAssignAction::<_, { ConstValue::new::<__Value>($e) }, _>(PhantomData::<$cvar>, { ctx!($($rem)*) })
    }};
    ($var:ident <= $action:expr; $($rem:tt)* ) => {{
        ConstContextBindAction($action, move |$var| { ctx!($($rem)*) })
    }};
    ($var:ident <= get $cvar:ty; $($rem:tt)* ) => {{
        type __Value = <$cvar as ConstVariable>::Value;
        ConstContextGetAction(PhantomData::<$cvar>, move |$var: __Value| { ctx!($($rem)*) })
    }};
    ($action:expr; $($rem:tt)*) => {{
        ConstContextBindAction($action, move |_| { ctx!($($rem)*) })
    }};
}

#[test]
#[cfg(test)]
fn test() {
    type Var = ((), u32);

    let f = |n: u32| {
        ctx! {
            pure n
        }
    };

    let push90 = || {
        ctx! {
            const Var = 90;
        }
    };

    let action = ctx! {
        const Var = 90;
        get Var
    };

    let action2 = ctx! {
        v <= f(42);
        pure v
    };

    let action3 = ctx! {
        push90();
        v <= f(42);
        w <= get Var;
        pure (v + w)
    };

    assert_eq!(action.start_eval(), 90);
    assert_eq!(action2.start_eval(), 42);
    assert_eq!(action3.start_eval(), 132);

    let action = ctx! {
        f(42)
    };

    assert_eq!(action.start_eval(), 42);

    let action = ctx! {
        const Var = 90;
        get Var
    };

    let action2 = ctx! {
        action;
        get Var
    };

    assert_eq!(action2.start_eval(), 90);
}
