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
    let size = core::mem::size_of::<T>();
    let align = core::mem::align_of::<T>();

    unsafe {
        let ptr = const_allocate(size, align);
        core::ptr::write(ptr.cast(), value);
        core::slice::from_raw_parts_mut(ptr.cast(), size)
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

pub struct ReturnAction<Closure>(Closure);

impl<Closure> ReturnAction<Closure> {
    #[inline(always)]
    pub const fn new(closure: Closure) -> Self {
        Self(closure)
    }
}

impl<Input, Closure, Ret> Action<Input> for ReturnAction<Closure>
where
    Input: VariableList,
    Closure: FnOnce() -> Ret,
{
    type OutputVars = Input;
    type Output = Ret;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        let Self(closure) = self;
        closure()
    }
}

pub struct GetAction<Variable>(PhantomData<Variable>);

impl<Variable> GetAction<Variable> {
    #[inline(always)]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Input, Variable> Action<Input> for GetAction<Variable>
where
    Input: VariableList,
    Variable: ConstVariable,
{
    type OutputVars = Input;
    type Output = Variable::Value;

    #[inline(always)]
    fn eval(self) -> Self::Output {
        const { find_variable::<Variable::Key, Variable::Value, Input>() }
    }
}

pub struct AssignAction<Variable, const VALUE: ConstValue>(PhantomData<Variable>);

impl<Variable, const VALUE: ConstValue> AssignAction<Variable, VALUE> {
    #[inline(always)]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Input, Variable, const VALUE: ConstValue> Action<Input> for AssignAction<Variable, VALUE>
where
    Input: VariableList,
    Variable: ConstVariable,
{
    type OutputVars = VariableListHas<Variable::Key, Variable::Value, VALUE, Input>;
    type Output = ();

    #[inline(always)]
    fn eval(self) -> Self::Output {}
}

#[macro_export]
macro_rules! ctx {
    () => {{
        $crate::ReturnAction::new(move || ())
    }};
    (pure $e:expr) => {{
        $crate::ReturnAction::new(move || $e)
    }};
    (get $cvar:ty) => {{
        $crate::GetAction::<$cvar>::new()
    }};
    (_ <- $action:expr; $($rem:tt)* ) => {{
        $crate::BindAction::new(
            $action,
            move |_| { $crate::ctx!($($rem)*) },
        )
    }};
    ($var:ident <- $action:expr; $($rem:tt)* ) => {{
        $crate::BindAction::new(
            $action,
            move |$var| { $crate::ctx!($($rem)*) },
        )
    }};
    ($var:ident <- get $cvar:ty; $($rem:tt)* ) => {{
        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;
        $crate::BindAction::new(
            $crate::GetAction::<$cvar>::new(),
            move |$var: __Value| { $crate::ctx!($($rem)*) },
        )
    }};
    (let _ = $e:expr; $($rem:tt)*) => {{
        $crate::BindAction::new(
            $crate::ReturnAction::new(move || $e),
            move |_| { $crate::ctx!($($rem)*) },
        )
    }};
    (let $var:ident = $e:expr; $($rem:tt)*) => {{
        $crate::BindAction::new(
            $crate::ReturnAction::new(move || $e),
            move |$var| { $crate::ctx!($($rem)*) },
        )
    }};
    (const _: $t:ty = $e:expr; $($rem:tt)*) => {{
        const _: $t = $e;
        $crate::ctx!($($rem)*)
    }};
    (const $var:ident: $t:ty = $e:expr; $($rem:tt)*) => {{
        const $var: $t = $e;
        $crate::ctx!($($rem)*)
    }};
    (set $cvar:ty = $e:expr; $($rem:tt)*) => {{
        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;
        $crate::BindAction::new(
            $crate::AssignAction::<$cvar, { $crate::ConstValue::new::<__Value>($e) }>::new(),
            move |_| { $crate::ctx!($($rem)*) },
        )
    }};
    (set $cvar:ty = ($e:expr) where $($id:ident = $var:ty),*; $($rem:tt)*) => {{
        #[doc(hidden)]
        type __Key = <$cvar as $crate::ConstVariable>::Key;

        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;

        #[doc(hidden)]
        struct __CustomAssignAction;

        #[doc(hidden)]
        struct __CustomVariableList<Input: $crate::VariableList>(core::marker::PhantomData<Input>);

        #[doc(hidden)]
        impl<Input: $crate::VariableList> $crate::VariableList for __CustomVariableList<Input> {
            type Next = Input;
        }

        #[doc(hidden)]
        impl<Input: $crate::VariableList> $crate::VariableListElement for __CustomVariableList<Input> {
            type Key = __Key;
            type Value = __Value;
            const VALUE: Option<$crate::ConstValue> = Some({
                $(let $id = $crate::find_variable::<
                    <$var as $crate::ConstVariable>::Key,
                    <$var as $crate::ConstVariable>::Value,
                    Input>();)*
                $crate::ConstValue::new::<__Value>($e)
            });
            const END: bool = false;
        }

        #[doc(hidden)]
        impl<Input> $crate::Action<Input> for __CustomAssignAction
        where
            Input: $crate::VariableList,
        {
            type OutputVars = __CustomVariableList<Input>;
            type Output = ();

            #[inline(always)]
            fn eval(self) -> Self::Output {
                const { <Self::OutputVars as $crate::VariableListElement>::VALUE };
            }
        }

        $crate::BindAction::new(
            __CustomAssignAction,
            move |_| { $crate::ctx!($($rem)*) },
        )
    }};
    ($action:expr) => {{
        $action
    }};
    ($action:expr; $($rem:tt)*) => {{
        $crate::BindAction::new(
            $action,
            move |_| { $crate::ctx!($($rem)*) },
        )
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
            set Var = 90;
        }
    };

    let action = ctx! {
        set Var = 45;
        set Var = (a + b) where a = Var, b = Var;
        get Var
    };

    let action2 = ctx! {
        v <- f(42);
        pure v
    };

    let action3 = ctx! {
        push90();
        v <- f(42);
        w <- get Var;
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
        set Var = 90;
        get Var
    };

    let action2 = ctx! {
        action;
        get Var
    };

    assert_eq!(action2.start_eval(), 90);
}
