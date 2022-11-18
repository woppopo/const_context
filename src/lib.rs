#![cfg_attr(not(test), no_std)]
#![feature(adt_const_params)]
#![feature(const_heap)]
#![feature(const_mut_refs)]
#![feature(const_option)]
#![feature(const_ptr_read)]
#![feature(const_ptr_write)]
#![feature(const_slice_from_raw_parts_mut)]
#![feature(const_type_id)]
#![feature(const_type_name)]
#![feature(core_intrinsics)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]
#![feature(never_type)]

use core::any::TypeId;
use core::intrinsics::const_allocate;
use core::marker::PhantomData;

const fn type_eq<A: 'static, B: 'static>() -> bool {
    let a = TypeId::of::<A>();
    let b = TypeId::of::<B>();
    unsafe { core::mem::transmute::<_, u64>(a) == core::mem::transmute::<_, u64>(b) }
}

const fn into_bytes<T>(value: T) -> &'static mut [u8] {
    unsafe {
        let ptr = const_allocate(core::mem::size_of::<T>(), core::mem::align_of::<T>());
        core::ptr::write(ptr.cast(), value);
        core::slice::from_raw_parts_mut(ptr.cast(), core::mem::size_of::<T>())
    }
}

const fn str_concat(s1: &'static str, s2: &'static str) -> &'static str {
    let s1 = s1.as_bytes();
    let s2 = s2.as_bytes();
    let len = s1.len() + s2.len();

    unsafe {
        let ptr = const_allocate(
            core::mem::size_of::<u8>() * len,
            core::mem::align_of::<u8>(),
        );

        core::ptr::copy(s1.as_ptr(), ptr, s1.len());
        core::ptr::copy(s2.as_ptr(), ptr.add(s1.len()), s2.len());
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr.cast(), len))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ConstValue(&'static [u8]);

impl ConstValue {
    pub const fn new<T>(value: T) -> Self
    where
        T: 'static,
        T: Eq,
    {
        Self(into_bytes(value))
    }

    pub const fn with_type<T>(self) -> T
    where
        T: 'static,
    {
        let Self(bytes) = self;
        unsafe { core::ptr::read(bytes.as_ptr().cast()) }
    }
}

pub struct VariableListEnd;

pub struct VariableListHas<Key, Value, const VALUE: ConstValue, Next>(
    PhantomData<(Key, Value, Next)>,
);

pub struct VariableListRemoved<Key, Next>(PhantomData<(Key, Next)>);

pub trait VariableList: VariableListElement {
    type Next: VariableList;
}

pub trait VariableListElement {
    type Key: 'static;
    type Value: 'static;
    const VALUE: Option<ConstValue>;
    const END: bool;
}

impl VariableListElement for VariableListEnd {
    type Key = !;
    type Value = !;
    const VALUE: Option<ConstValue> = None;
    const END: bool = true;
}

impl VariableList for VariableListEnd {
    type Next = VariableListEnd;
}

impl<Key: 'static, Value: 'static, const VAL: ConstValue, Next: VariableList> VariableListElement
    for VariableListHas<Key, Value, VAL, Next>
{
    type Key = Key;
    type Value = Value;
    const VALUE: Option<ConstValue> = Some(VAL);
    const END: bool = false;
}

impl<Key: 'static, Value: 'static, const VAL: ConstValue, Next: VariableList> VariableList
    for VariableListHas<Key, Value, VAL, Next>
{
    type Next = Next;
}

impl<Key: 'static, Next: VariableList> VariableListElement for VariableListRemoved<Key, Next> {
    type Key = Key;
    type Value = !;
    const VALUE: Option<ConstValue> = None;
    const END: bool = false;
}

impl<Key: 'static, Next: VariableList> VariableList for VariableListRemoved<Key, Next> {
    type Next = Next;
}

const fn error_not_found<Key>() -> &'static str {
    let type_name = core::any::type_name::<Key>();
    str_concat(
        str_concat("The key `", type_name),
        "` is not found in current context.",
    )
}

const fn error_unexpected_type<Expected, Value>() -> &'static str {
    let type_name_expect = core::any::type_name::<Expected>();
    let type_name_value = core::any::type_name::<Value>();
    str_concat(
        str_concat("Mismatched types: expected `", type_name_expect),
        str_concat("`, found `", str_concat(type_name_value, "`.")),
    )
}

#[track_caller]
pub const fn find_variable<Key, Value, List: VariableList>() -> Value
where
    Key: 'static,
    Value: 'static,
{
    if List::END {
        panic!("{}", error_not_found::<Key>());
    }

    if type_eq::<Key, List::Key>() {
        let value = List::VALUE.expect(error_not_found::<Key>());
        assert!(
            type_eq::<Value, List::Value>(),
            "{}",
            error_unexpected_type::<Value, List::Value>()
        );
        value.with_type()
    } else {
        find_variable::<Key, Value, List::Next>()
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

pub trait Action<Input>
where
    Input: VariableList,
{
    type OutputVars: VariableList;
    type Output;
    fn eval(self) -> Self::Output;
}

pub trait StartEvaluation {
    type Output;
    fn start_eval(self) -> Self::Output;
}

impl<T: Action<VariableListEnd>> StartEvaluation for T {
    type Output = T::Output;

    #[inline(always)]
    fn start_eval(self) -> Self::Output {
        self.eval()
    }
}

pub struct BindAction<PreviousAction, ActionConstructor>(PreviousAction, ActionConstructor);

impl<PreviousAction, ActionConstructor> BindAction<PreviousAction, ActionConstructor> {
    #[inline(always)]
    pub const fn new(prev: PreviousAction, constructor: ActionConstructor) -> Self {
        Self(prev, constructor)
    }
}

impl<Input, PreviousAction, ActionConstructor, NextAction> Action<Input>
    for BindAction<PreviousAction, ActionConstructor>
where
    Input: VariableList,
    PreviousAction: Action<Input>,
    ActionConstructor: FnOnce(PreviousAction::Output) -> NextAction,
    NextAction: Action<PreviousAction::OutputVars>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        let Self(action, constructor) = self;
        let output = action.eval();
        constructor(output).eval()
    }
}

pub struct ClosureAction<F>(F);

impl<F> ClosureAction<F> {
    #[inline(always)]
    pub const fn new(closure: F) -> Self {
        Self(closure)
    }
}

impl<Input, F, NextAction> Action<Input> for ClosureAction<F>
where
    Input: VariableList,
    F: FnOnce() -> NextAction,
    NextAction: Action<Input>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        let Self(closure) = self;
        closure().eval()
    }
}

pub struct ReturnAction<T>(T);

impl<T> ReturnAction<T> {
    #[inline(always)]
    pub const fn new(value: T) -> Self {
        Self(value)
    }
}

impl<Input, T> Action<Input> for ReturnAction<T>
where
    Input: VariableList,
{
    type OutputVars = Input;
    type Output = T;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        self.0
    }
}

pub struct GetAction<Variable, ActionConstructor>(PhantomData<Variable>, ActionConstructor);

impl<Variable, ActionConstructor> GetAction<Variable, ActionConstructor> {
    #[inline(always)]
    pub const fn new(constructor: ActionConstructor) -> Self {
        Self(PhantomData, constructor)
    }
}

impl<Input, Variable, ActionConstructor, NextAction> Action<Input>
    for GetAction<Variable, ActionConstructor>
where
    Input: VariableList,
    Variable: ConstVariable,
    ActionConstructor: FnOnce(Variable::Value) -> NextAction,
    NextAction: Action<Input>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        let Self(_, constructor) = self;
        let got = const { find_variable::<Variable::Key, Variable::Value, Input>() };
        constructor(got).eval()
    }
}

pub struct AssignAction<Variable, const VALUE: ConstValue, NextAction>(
    PhantomData<Variable>,
    NextAction,
);

impl<Variable, const VALUE: ConstValue, NextAction> AssignAction<Variable, VALUE, NextAction> {
    #[inline(always)]
    pub const fn new(next: NextAction) -> Self {
        Self(PhantomData, next)
    }
}

impl<Input, Variable, const VALUE: ConstValue, NextAction> Action<Input>
    for AssignAction<Variable, VALUE, NextAction>
where
    Input: VariableList,
    Variable: ConstVariable,
    NextAction: Action<VariableListHas<Variable::Key, Variable::Value, VALUE, Input>>,
{
    type OutputVars = NextAction::OutputVars;
    type Output = NextAction::Output;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        let Self(_, action) = self;
        action.eval()
    }
}

#[macro_export]
macro_rules! ctx {
    () => {{
        $crate::ReturnAction::new(())
    }};
    (pure $e:expr) => {{
        $crate::ReturnAction::new($e)
    }};
    (get $cvar:ty) => {{
        $crate::GetAction::<$cvar, _>::new($crate::ReturnAction::new)
    }};
    ($action:expr) => {{
        $action
    }};
    (let _ = $e:expr; $($rem:tt)*) => {{
        $crate::ClosureAction::new(move || {
            let _ = $e;
            $crate::ctx! { $($rem)* }
        })
    }};
    (let $var:ident = $e:expr; $($rem:tt)*) => {{
        $crate::ClosureAction::new(move || {
            let $var = $e;
            $crate::ctx! { $($rem)* }
        })
    }};
    (const $cvar:ty = $e:expr; $($rem:tt)*) => {{
        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;
        $crate::AssignAction::<$cvar, { $crate::ConstValue::new::<__Value>($e) }, _>::new({ $crate::ctx!($($rem)*) })
    }};
    (const $cvar:ty = ($e:expr) where $($id:ident = $var:ty),*; $($rem:tt)*) => {{
        #[doc(hidden)]
        type __Key = <$cvar as $crate::ConstVariable>::Key;

        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;

        #[doc(hidden)]
        struct __CustomAssignAction<NextAction>(NextAction);

        #[doc(hidden)]
        const fn __construct_const_value<Input: $crate::VariableList>() -> $crate::ConstValue {
            $(let $id = $crate::find_variable::<
                <$var as $crate::ConstVariable>::Key,
                <$var as $crate::ConstVariable>::Value,
                Input>();)*
            $crate::ConstValue::new::<__Value>($e)
        }

        #[doc(hidden)]
        impl<Input, NextAction> $crate::Action<Input>
            for __CustomAssignAction<NextAction>
        where
            Input: $crate::VariableList,
            NextAction: $crate::Action<$crate::VariableListHas<__Key, __Value, { __construct_const_value::<Input>() }, Input>>,
        {
            type OutputVars = NextAction::OutputVars;
            type Output = NextAction::Output;

            #[inline(always)]
            fn eval(self) -> Self::Output {
                let Self(next) = self;
                next.eval()
            }
        }

        __CustomAssignAction({ $crate::ctx!($($rem)*) })
    }};
    (_ <= $action:expr; $($rem:tt)* ) => {{
        $crate::BindAction::new($action, move |_| { $crate::ctx!($($rem)*) })
    }};
    ($var:ident <= $action:expr; $($rem:tt)* ) => {{
        $crate::BindAction::new($action, move |$var| { $crate::ctx!($($rem)*) })
    }};
    ($var:ident <= get $cvar:ty; $($rem:tt)* ) => {{
        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;
        $crate::GetAction::<$cvar, _>::new(move |$var: __Value| { $crate::ctx!($($rem)*) })
    }};
    ($action:expr; $($rem:tt)*) => {{
        $crate::BindAction::new($action, move |_| { $crate::ctx!($($rem)*) })
    }};
}

#[test]
#[cfg(test)]
fn test() {
    type Var = ((), u32);

    fn f<Vars: VariableList>(n: u32) -> impl Action<Vars, Output = u32> {
        ctx! {
            pure n
        }
    }

    let push90 = || {
        ctx! {
            const Var = 90;
        }
    };

    let action = ctx! {
        const Var = 45;
        const Var = (a + b) where a = Var, b = Var;
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
