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

pub struct VariableListEnd;

pub struct VariableListHas<Key, const VALUE: ConstValue, Next>(PhantomData<(Key, Next)>);

pub trait VariableList: VariableListElement {
    type Next: VariableList;
}

pub trait VariableListElement {
    type Key: 'static;
    const VALUE: Option<ConstValue>;
}

impl VariableListElement for VariableListEnd {
    type Key = ();
    const VALUE: Option<ConstValue> = None;
}

impl VariableList for VariableListEnd {
    type Next = VariableListEnd;
}

impl<Key: 'static, const VAL: ConstValue, Next: VariableList> VariableListElement
    for VariableListHas<Key, VAL, Next>
{
    type Key = Key;
    const VALUE: Option<ConstValue> = Some(VAL);
}

impl<Key: 'static, const VAL: ConstValue, Next: VariableList> VariableList
    for VariableListHas<Key, VAL, Next>
{
    type Next = Next;
}

const fn error_not_found<Key>() -> &'static str {
    let msg1 = "The key `".as_bytes();
    let msg2 = "` is not found in current context.".as_bytes();
    let type_name = core::any::type_name::<Key>().as_bytes();

    unsafe {
        let len = msg1.len() + type_name.len() + msg2.len();
        let ptr = const_allocate(
            core::mem::size_of::<u8>() * len,
            core::mem::align_of::<u8>(),
        );

        let mut offset = 0;

        core::ptr::copy(msg1.as_ptr(), ptr.add(offset), msg1.len());
        offset += msg1.len();

        core::ptr::copy(type_name.as_ptr(), ptr.add(offset), type_name.len());
        offset += type_name.len();

        core::ptr::copy(msg2.as_ptr(), ptr.add(offset), msg2.len());
        offset += msg2.len();

        assert!(len == offset);
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr.cast(), offset))
    }
}

#[track_caller]
const fn find_variable<Key, Value, List: VariableList>() -> Value
where
    Key: 'static,
    Value: 'static,
{
    match List::VALUE {
        Some(value) => {
            if eq_typeid(TypeId::of::<Key>(), TypeId::of::<List::Key>()) {
                value.into_inner()
            } else {
                find_variable::<Key, Value, List::Next>()
            }
        }
        None => panic!("{}", error_not_found::<Key>()),
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
    NextAction: Action<VariableListHas<Variable::Key, VALUE, Input>>,
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
        struct __CustomAction<NextAction>(NextAction);

        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        const fn __construct_const_value<Input: $crate::VariableList, $($id : $crate::ConstVariable<Value = <$var as ConstVariable>::Value>,)*>() -> ConstValue {
            $(let $id: $id::Value = $crate::find_variable::<$id::Key, $id::Value, Input>();)*
            ConstValue::new::<__Value>($e)
        }

        #[doc(hidden)]
        impl<Input, NextAction> $crate::Action<Input>
            for __CustomAction<NextAction>
        where
            Input: $crate::VariableList,
            NextAction: $crate::Action<$crate::VariableListHas<__Key, { __construct_const_value::<Input, $($var,)*>() }, Input>>,
        {
            type OutputVars = NextAction::OutputVars;
            type Output = NextAction::Output;

            #[inline(always)]
            fn eval(self) -> Self::Output {
                let Self(next) = self;
                next.eval()
            }
        }

        __CustomAction({ $crate::ctx!($($rem)*) })
    }};
    (_ <= $action:expr; $($rem:tt)* ) => {{
        $crate::BindAction::new($action, move |_| { $crate::ctx!($($rem)*) })
    }};
    ($var:ident <= $action:expr; $($rem:tt)* ) => {{
        $crate::BindAction::new($action, move |$var| { $crate::ctx!($($rem)*) })
    }};
    ($var:ident <= get $cvar:ty; $($rem:tt)* ) => {{
        #[doc(hidden)]
        type __Value = <$cvar as ConstVariable>::Value;
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
