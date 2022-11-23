use core::marker::PhantomData;

use crate::{Action, VariableList};

pub trait IntoBoolFromVariableList {
    type Into<Vars: VariableList>: IntoBool;
}

pub trait IntoBool {
    const BOOL: bool;
}

pub trait BoolToTypeBool {
    type Into: TypeBool;
}

pub trait TypeBool {}

pub struct True;

pub struct False;

impl TypeBool for True {}
impl TypeBool for False {}

impl<Cond: IntoBool> BoolToTypeBool for Cond {
    default type Into = False;
}

impl<Cond: IntoBool<BOOL = true>> BoolToTypeBool for Cond {
    type Into = True;
}

pub struct DummyAction<Output>(PhantomData<Output>);

impl<Output> Action for DummyAction<Output> {
    type Output = Output;
    type Vars<Vars: VariableList> = Vars;
    fn eval<Vars: VariableList>(self) -> Self::Output {
        unreachable!()
    }
}

pub trait Select<Output> {
    type Action: Action<Output = Output>;
    fn selected(self) -> Self::Action;
}

pub struct SelectAction<A, B, Cond: TypeBool, Output>(A, B, PhantomData<(Cond, Output)>)
where
    A: Action<Output = Output>,
    B: Action<Output = Output>;

impl<A, B, Cond: TypeBool, Output> SelectAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    pub const fn new(a: A, b: B) -> Self {
        Self(a, b, PhantomData)
    }
}

impl<A, B, Cond: TypeBool, Output> Select<Output> for SelectAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    default type Action = DummyAction<Output>;
    default fn selected(self) -> Self::Action {
        unreachable!()
    }
}

impl<A, B, Output> Select<Output> for SelectAction<A, B, True, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    type Action = A;
    fn selected(self) -> Self::Action {
        self.0
    }
}

impl<A, B, Output> Select<Output> for SelectAction<A, B, False, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    type Action = B;
    fn selected(self) -> Self::Action {
        self.1
    }
}

pub struct IfAction<A, B, Cond, Output>(A, B, PhantomData<(Cond, Output)>);

impl<A, B, Cond: IntoBoolFromVariableList, Output> IfAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    pub const fn new(a: A, b: B) -> Self {
        Self(a, b, PhantomData)
    }
}

impl<A, B, Cond: IntoBoolFromVariableList, Output> Action for IfAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    type Output = Output;
    type Vars<Vars: VariableList> =
        <<SelectAction<A, B, <Cond::Into<Vars> as BoolToTypeBool>::Into, Output> as Select<
            Output,
        >>::Action as Action>::Vars<Vars>;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {
        SelectAction::<A, B, <Cond::Into<Vars> as BoolToTypeBool>::Into, Output>::new(
            self.0, self.1,
        )
        .selected()
        .eval::<Vars>()
    }
}

#[macro_export]
macro_rules! ctx_if {
    ($cond:expr, where $($id:ident = $var:ty),+; then { $a:expr } else { $b:expr }) => {{
        #[doc(hidden)]
        struct __Condition;

        #[doc(hidden)]
        impl $crate::conditional::IntoBoolFromVariableList for __Condition {
            type Into<Vars: $crate::VariableList> = __ConditionBool<Vars>;
        }

        #[doc(hidden)]
        struct __ConditionBool<Vars: $crate::VariableList>(::core::marker::PhantomData<Vars>);

        #[doc(hidden)]
        impl<Vars: $crate::VariableList> $crate::conditional::IntoBool for __ConditionBool<Vars> {
            const BOOL: bool = {
                $(let $id = $crate::find_variable::<
                    <$var as $crate::ConstVariable>::Key,
                    <$var as $crate::ConstVariable>::Value,
                    Vars>();)*
                $cond
            };
        }

        $crate::conditional::IfAction::<_, _, __Condition, _>::new($a, $b)
    }};
}

#[test]
#[cfg(test)]
fn test() {
    use crate::ctx;

    type Var = (u32, u32);

    let action = ctx! {
        set Var = 45;
        ctx_if!(
            a + b == 90, where a = Var, b = Var;
            then { ctx! { pure "==" } }
            else { ctx! { pure "!=" } }
        )
    };
    assert_eq!(action.start_eval(), "==");
}
