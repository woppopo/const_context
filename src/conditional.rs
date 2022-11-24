use core::marker::PhantomData;

use crate::{Action, VariableList};

pub trait IntoBoolFromVariableList {
    type From<Vars: VariableList>: IntoBool;
}

pub trait IntoBool {
    const BOOL: bool;
}

pub trait Select<Output> {
    type Action: Action<Output = Output>;
}

pub struct SelectAction<A, B, Cond: IntoBool, Output>(A, B, PhantomData<(Cond, Output)>)
where
    A: Action<Output = Output>,
    B: Action<Output = Output>;

impl<A, B, Cond: IntoBool, Output> SelectAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    #[inline(always)]
    pub const fn new(a: A, b: B) -> Self {
        Self(a, b, PhantomData)
    }
}

impl<A, B, Cond: IntoBool, Output> Select<Output> for SelectAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    default type Action = B;
}

impl<A, B, Cond: IntoBool<BOOL = true>, Output> Select<Output> for SelectAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    type Action = A;
}

pub struct IfAction<A, B, Cond, Output>(A, B, PhantomData<(Cond, Output)>);

impl<A, B, Cond: IntoBoolFromVariableList, Output> IfAction<A, B, Cond, Output>
where
    A: Action<Output = Output>,
    B: Action<Output = Output>,
{
    #[inline(always)]
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
    type Vars<Vars: VariableList> = <<SelectAction<A, B, Cond::From<Vars>, Output> as Select<
        Output,
    >>::Action as Action>::Vars<Vars>;

    #[inline(always)]
    fn eval<Vars: VariableList>(self) -> Self::Output {
        let Self(a, b, ..) = self;
        if const { Cond::From::<Vars>::BOOL } {
            a.eval::<Vars>()
        } else {
            b.eval::<Vars>()
        }
    }
}

#[macro_export]
macro_rules! ctx_if {
    ($cond:expr, where $($id:ident = $var:ty),+; then { $a:expr } else { $b:expr }) => {{
        #[doc(hidden)]
        struct __Condition;

        #[doc(hidden)]
        impl $crate::conditional::IntoBoolFromVariableList for __Condition {
            type From<Vars: $crate::VariableList> = __ConditionBool<Vars>;
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
    type Var2 = (u64, u64);

    let action = ctx! {
        set Var = 45;
        ctx_if!(
            a + b == 90, where a = Var, b = Var;
            then { ctx! { pure "==" } }
            else { ctx! { pure "!=" } }
        )
    };
    assert_eq!(action.start_eval(), "==");

    let action = ctx! {
        set Var = 45;
        ctx_if!(
            a + b == 90, where a = Var, b = Var;
            then { ctx! { set Var2 = 42; } }
            else { ctx! { } }
        );
        get Var2
    };
    assert_eq!(action.start_eval(), 42);
}
