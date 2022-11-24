#![cfg_attr(not(test), no_std)]
#![cfg_attr(feature = "conditional", feature(associated_const_equality))]
#![cfg_attr(feature = "conditional", feature(specialization))]
#![feature(adt_const_params)]
#![feature(const_heap)]
#![feature(const_ptr_read)]
#![feature(const_ptr_write)]
#![feature(const_type_id)]
#![feature(const_type_name)]
#![feature(core_intrinsics)]
#![feature(inline_const)]

#[cfg(feature = "conditional")]
pub mod conditional;

use core::any::TypeId;
use core::intrinsics::const_allocate;
use core::marker::PhantomData;

const fn type_eq<A: 'static, B: 'static>() -> bool {
    let a = TypeId::of::<A>();
    let b = TypeId::of::<B>();
    unsafe { core::mem::transmute::<_, u64>(a) == core::mem::transmute::<_, u64>(b) }
}

const fn str_concat(s1: &str, s2: &str) -> &'static str {
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
    pub const fn new<T>(value: T) -> Self {
        let size = core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();

        let bytes = unsafe {
            let ptr = const_allocate(size, align);
            core::ptr::write(ptr.cast(), value);
            core::slice::from_raw_parts(ptr.cast(), size)
        };

        Self(bytes)
    }

    pub const fn with_type<T>(self) -> T {
        let Self(bytes) = self;
        unsafe { core::ptr::read(bytes.as_ptr().cast()) }
    }
}

pub struct VariableListEnd;

pub struct VariableListHas<Key, Value, const VALUE: ConstValue, Next>(
    PhantomData<(Key, Value, Next)>,
);

pub struct VariableListRemoved<Key, Next>(PhantomData<(Key, Next)>);

pub enum VariableListValue<T> {
    End,
    Has(T),
    Removed,
}

pub trait VariableList {
    type Next: VariableList;
    type Key: 'static;
    type Value: 'static;
    const VALUE: VariableListValue<ConstValue>;
}

impl VariableList for VariableListEnd {
    type Next = VariableListEnd;
    type Key = ();
    type Value = ();
    const VALUE: VariableListValue<ConstValue> = VariableListValue::End;
}

impl<Key: 'static, Value: 'static, const VAL: ConstValue, Next: VariableList> VariableList
    for VariableListHas<Key, Value, VAL, Next>
{
    type Next = Next;
    type Key = Key;
    type Value = Value;
    const VALUE: VariableListValue<ConstValue> = VariableListValue::Has(VAL);
}

impl<Key: 'static, Next: VariableList> VariableList for VariableListRemoved<Key, Next> {
    type Next = Next;
    type Key = Key;
    type Value = ();
    const VALUE: VariableListValue<ConstValue> = VariableListValue::Removed;
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
    match List::VALUE {
        VariableListValue::End => panic!("{}", error_not_found::<Key>()),
        VariableListValue::Removed if type_eq::<Key, List::Key>() => {
            panic!("{}", error_not_found::<Key>())
        }
        VariableListValue::Has(value) if type_eq::<Key, List::Key>() => {
            assert!(
                type_eq::<Value, List::Value>(),
                "{}",
                error_unexpected_type::<Value, List::Value>()
            );
            value.with_type()
        }
        _ => find_variable::<Key, Value, List::Next>(),
    }
}

pub trait ConstVariable {
    type Key: 'static;
    type Value: 'static;
}

impl ConstVariable for () {
    type Key = ();
    type Value = ();
}

impl<K, V> ConstVariable for (K, V)
where
    K: 'static,
    V: 'static,
{
    type Key = K;
    type Value = V;
}

pub trait Action: Sized {
    type Output;
    type Vars<Vars: VariableList>: VariableList;
    fn eval<Vars: VariableList>(self) -> Self::Output;

    #[inline(always)]
    fn start_eval(self) -> Self::Output {
        self.eval::<VariableListEnd>()
    }
}

pub struct BindAction<PreviousAction, ActionConstructor>(PreviousAction, ActionConstructor);

impl<PreviousAction, ActionConstructor> BindAction<PreviousAction, ActionConstructor> {
    #[inline(always)]
    pub const fn new<Ret>(prev: PreviousAction, constructor: ActionConstructor) -> Self
    where
        PreviousAction: Action,
        ActionConstructor: FnOnce(PreviousAction::Output) -> Ret,
    {
        Self(prev, constructor)
    }
}

impl<PreviousAction, ActionConstructor, NextAction> Action
    for BindAction<PreviousAction, ActionConstructor>
where
    PreviousAction: Action,
    ActionConstructor: FnOnce(PreviousAction::Output) -> NextAction,
    NextAction: Action,
{
    type Output = NextAction::Output;
    type Vars<Vars: VariableList> = NextAction::Vars<PreviousAction::Vars<Vars>>;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {
        let Self(action, constructor) = self;
        let output = action.eval::<Vars>();
        constructor(output).eval::<PreviousAction::Vars<Vars>>()
    }
}

pub struct PureAction<Closure>(Closure);

impl<Closure> PureAction<Closure> {
    #[inline(always)]
    pub const fn new(closure: Closure) -> Self {
        Self(closure)
    }
}

impl<Closure, Ret> Action for PureAction<Closure>
where
    Closure: FnOnce() -> Ret,
{
    type Output = Ret;
    type Vars<Vars: VariableList> = Vars;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {
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

impl<Variable> Action for GetAction<Variable>
where
    Variable: ConstVariable,
{
    type Output = Variable::Value;
    type Vars<Vars: VariableList> = Vars;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {
        const { find_variable::<Variable::Key, Variable::Value, Vars>() }
    }
}

pub struct UnsetAction<Variable>(PhantomData<Variable>);

impl<Variable> UnsetAction<Variable> {
    #[inline(always)]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Variable> Action for UnsetAction<Variable>
where
    Variable: ConstVariable,
{
    type Output = ();
    type Vars<Vars: VariableList> = VariableListRemoved<Variable::Key, Vars>;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {}
}

pub struct SetAction<Variable, const VALUE: ConstValue>(PhantomData<Variable>);

impl<Variable, const VALUE: ConstValue> SetAction<Variable, VALUE> {
    #[inline(always)]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<Variable, const VALUE: ConstValue> Action for SetAction<Variable, VALUE>
where
    Variable: ConstVariable,
{
    type Output = ();
    type Vars<Vars: VariableList> = VariableListHas<Variable::Key, Variable::Value, VALUE, Vars>;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {}
}

#[macro_export]
macro_rules! ctx {
    {} => {{
        $crate::PureAction::new(move || ())
    }};
    { pure $e:expr } => {{
        $crate::PureAction::new(move || $e)
    }};
    { get $cvar:ty } => {{
        $crate::GetAction::<$cvar>::new()
    }};
    { _ <- get $cvar:ty; $($rest:tt)*  } => {{
        $crate::ctx! {
            _ <- $crate::GetAction::<$cvar>::new();
            $($rest)*
        }
    }};
    { _ <- $action:expr; $($rest:tt)*  } => {{
        $crate::BindAction::new(
            $action,
            move |_| $crate::ctx! { $($rest)* },
        )
    }};
    { $var:ident <- get $cvar:ty; $($rest:tt)*  } => {{
        $crate::ctx! {
            $var <- $crate::GetAction::<$cvar>::new();
            $($rest)*
        }
    }};
    { $var:ident <- $action:expr; $($rest:tt)*  } => {{
        $crate::BindAction::new(
            $action,
            move |$var| $crate::ctx! { $($rest)* },
        )
    }};
    { let _ $(: $ty:ty)? = $e:expr; $($rest:tt)* } => {{
        $crate::BindAction::new(
            $crate::PureAction::new(move || $e),
            move |_ $(: $ty)?| $crate::ctx! { $($rest)* },
        )
    }};
    { let $var:ident $(: $ty:ty)? = $e:expr; $($rest:tt)* } => {{
        $crate::BindAction::new(
            $crate::PureAction::new(move || $e),
            move |$var $(: $ty)?| $crate::ctx! { $($rest)* },
        )
    }};
    { let mut $var:ident $(: $ty:ty)? = $e:expr; $($rest:tt)* } => {{
        $crate::BindAction::new(
            $crate::PureAction::new(move || $e),
            move |mut $var $(: $ty)?| $crate::ctx! { $($rest)* },
        )
    }};
    { let ref $var:ident $(: $ty:ty)? = $e:expr; $($rest:tt)* } => {{
        $crate::BindAction::new(
            $crate::PureAction::new(move || $e),
            move |ref $var $(: $ty)?| $crate::ctx! { $($rest)* },
        )
    }};
    { let ref mut $var:ident $(: $ty:ty)? = $e:expr; $($rest:tt)* } => {{
        $crate::BindAction::new(
            $crate::PureAction::new(move || $e),
            move |ref mut $var $(: $ty)?| $crate::ctx! { $($rest)* },
        )
    }};
    { const _: $ty:ty = $value:expr; $($rest:tt)* } => {{
        const _: $ty = $value;
        $crate::ctx! { $($rest)* }
    }};
    { const $name:ident: $ty:ty = $value:expr; $($rest:tt)* } => {{
        const $name: $ty = $value;
        $crate::ctx! { $($rest)* }
    }};
    { type $name:ident = $ty:ty; $($rest:tt)* } => {{
        type $name = $ty;
        $crate::ctx! { $($rest)* }
    }};
    { set $cvar:ty = $e:expr; $($rest:tt)* } => {{
        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;

        $crate::ctx! {
            $crate::SetAction::<$cvar, { $crate::ConstValue::new::<__Value>($e) }>::new();
            $($rest)*
        }
    }};
    { set $cvar:ty = $e:expr, where $($id:ident = $var:ty),+; $($rest:tt)* } => {{
        #[doc(hidden)]
        type __Key = <$cvar as $crate::ConstVariable>::Key;

        #[doc(hidden)]
        type __Value = <$cvar as $crate::ConstVariable>::Value;

        #[doc(hidden)]
        struct __CustomSetAction;

        #[doc(hidden)]
        struct __CustomVariableList<Input: $crate::VariableList>(::core::marker::PhantomData<Input>);

        #[doc(hidden)]
        impl<Input: $crate::VariableList> $crate::VariableList for __CustomVariableList<Input> {
            type Next = Input;
            type Key = __Key;
            type Value = __Value;
            const VALUE: $crate::VariableListValue<$crate::ConstValue> = $crate::VariableListValue::Has({
                $(let $id = $crate::find_variable::<
                    <$var as $crate::ConstVariable>::Key,
                    <$var as $crate::ConstVariable>::Value,
                    Input>();)*
                $crate::ConstValue::new::<__Value>($e)
            });
        }

        #[doc(hidden)]
        impl $crate::Action for __CustomSetAction {
            type Output = ();
            type Vars<Vars: $crate::VariableList> = __CustomVariableList<Vars>;

            #[inline(always)]
            fn eval<Vars: $crate::VariableList>(self) -> Self::Output {
                #[allow(path_statements)]
                const {
                    <Self::Vars<Vars> as $crate::VariableList>::VALUE;
                }
            }
        }

        $crate::ctx! {
            __CustomSetAction;
            $($rest)*
        }
    }};
    { unset $cvar:ty; $($rest:tt)* } => {{
        $crate::ctx! {
            $crate::UnsetAction::<$cvar>::new();
            $($rest)*
        }
    }};
    { $action:expr; $($rest:tt)* } => {{
        $crate::ctx! {
            _ <- $action;
            $($rest)*
        }
    }};
    { $action:expr } => {{
        $action
    }};
}

#[test]
#[cfg(test)]
fn test() {
    type Var = ((), u32);

    fn f(n: u32) -> impl Action<Output = u32> {
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
        set Var = a + b, where a = Var, b = Var;
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

    let action = ctx! {
        let _a = 0;
        let mut _a = 0;
        let ref _a = 0;
        let ref mut _a = 0;
        let _a: u32 = 0;
        let mut _a: u32 = 0;
        let ref _a: u32 = 0;
        let ref mut _a: u32 = 0;
        type Temp = (u64, u64);
        set Temp = 0;
        unset Temp;
    };

    assert_eq!(action.start_eval(), ());
}
