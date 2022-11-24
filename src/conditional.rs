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
macro_rules! ctx_if_construct {
    {
        predicate = (set $var:ty)
        where = ()
        then = ($then:expr)
        else = ($else:expr)
    }=> {{
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
                $crate::is_variable_in::<
                    Vars,
                    <$var as $crate::ConstVariable>::Key,
                    <$var as $crate::ConstVariable>::Value>()
            };
        }

        $crate::conditional::IfAction::<_, _, __Condition, _>::new($then, $else)
    }};
    {
        predicate = ( $cond:expr )
        where = ($($id:ident = $var:ty),*)
        then = ($then:expr)
        else = ($else:expr)
    } => {{
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
                    Vars,
                    <$var as $crate::ConstVariable>::Key,
                    <$var as $crate::ConstVariable>::Value>();)*
                $cond
            };
        }

        $crate::conditional::IfAction::<_, _, __Condition, _>::new($then, $else)
    }}
}

#[macro_export]
macro_rules! ctx_if_else {
    {
        predicate = ($($predicate:tt)*)
        where = ($($binding:tt)*)
        then = ($($then:tt)*)
        else = ($($else:tt)*)
        rest = ()
    } => {
        $crate::ctx_if_construct! {
            predicate = ($($predicate)*)
            where = ($($binding)*)
            then = ($($then)*)
            else = ($($else)*)
        }
    };
    {
        predicate = ($($predicate:tt)*)
        where = ($($binding:tt)*)
        then = ($($then:tt)*)
        else = ($($else:tt)*)
        rest = ($e:tt $($rest:tt)*)
    } => {
        $crate::ctx_if_else! {
            predicate = ($($predicate)*)
            where = ($($binding)*)
            then = ($($then)*)
            else = ($($else)* $e)
            rest = ($($rest)*)
        }
    };
}

#[macro_export]
macro_rules! ctx_if_then {
    {
        predicate = ($($predicate:tt)*)
        where = ($($binding:tt)*)
        then = ($($then:tt)*)
        rest = (else $($rest:tt)*)
    } => {
        $crate::ctx_if_else! {
            predicate = ($($predicate)*)
            where = ($($binding)*)
            then = ($($then)*)
            else = ()
            rest = ($($rest)*)
        }
    };
    {
        predicate = ($($predicate:tt)*)
        where = ($($binding:tt)*)
        then = ($($then:tt)*)
        rest = ($e:tt $($rest:tt)*)
    } => {
        $crate::ctx_if_then! {
            predicate = ($($predicate)*)
            where = ($($binding)*)
            then = ($($then)* $e)
            rest = ($($rest)*)
        }
    };
}

#[macro_export]
macro_rules! ctx_if_where {
    {
        predicate = ($($predicate:tt)*)
        where = ($($binding:tt)*)
        rest = (then $($rest:tt)*)
    } => {
        $crate::ctx_if_then! {
            predicate = ($($predicate)*)
            where = ($($binding)*)
            then = ()
            rest = ($($rest)*)
        }
    };
    {
        predicate = ($($predicate:tt)*)
        where = ($($binding:tt)*)
        rest = ($where:tt $($rest:tt)*)
    } => {
        $crate::ctx_if_where! {
            predicate = ($($predicate)*)
            where = ($($binding)* $where)
            rest = ($($rest)*)
        }
    };
}

#[macro_export]
macro_rules! ctx_if_predicate {
    {
        predicate = ($($predicate:tt)*)
        rest = (where $($rest:tt)*)
    } => {
        $crate::ctx_if_where! {
            predicate = ($($predicate)*)
            where = ()
            rest = ($($rest)*)
        }
    };
    {
        predicate = ($($predicate:tt)*)
        rest = (then $($rest:tt)*)
    } => {
        $crate::ctx_if_then! {
            predicate = ($($predicate)*)
            where = ()
            then = ()
            rest = ($($rest)*)
        }
    };
    {
        predicate = ($($predicate:tt)*)
        rest = ($cond:tt $($rest:tt)*)
    } => {
        $crate::ctx_if_predicate! {
            predicate = ($($predicate)* $cond)
            rest = ($($rest)*)
        }
    };
}

#[macro_export]
macro_rules! ctx_if {
    (if set $($rest:tt)*) => {
        $crate::ctx_if_predicate! {
            predicate = (set)
            rest = ($($rest)*)
        }
    };
    (if $($rest:tt)*) => {
        $crate::ctx_if_predicate! {
            predicate = ()
            rest = ($($rest)*)
        }
    };
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
            if a + b == 90 where a = Var, b = Var then
                ctx! { pure "==" }
            else
                ctx! { pure "!=" }
        )
    };
    assert_eq!(action.start_eval(), "==");

    let action = ctx! {
        set Var = 45;
        ctx_if!(
            if a + b == 90 where a = Var, b = Var then
                ctx! { set Var2 = 42; }
            else
                ctx! { }
        );
        get Var2
    };
    assert_eq!(action.start_eval(), 42);

    let action = ctx! {
        set Var = 45;
        ctx_if!(
            if set Var then
                ctx! { set Var2 = 42; }
            else
                ctx! { }
        );
        get Var2
    };
    assert_eq!(action.start_eval(), 42);
}
