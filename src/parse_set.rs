#[macro_export]
macro_rules! ctx_set {
    {
        state = parse_dst
        rest = [ $dst:ty = $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_expr
            dst = [ $dst ]
            expr = []
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_expr
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        rest = []
    } => {
        $crate::ctx_set! {
            state = construct
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = []
            generic_names = []
            generic_bounds = []
            generic_params = []
            generic_const = []
            generic_const_params = []
        }
    };
    {
        state = parse_expr
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        rest = [ where $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_where
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = []
            generic_names = []
            generic_bounds = []
            generic_params = []
            generic_const = []
            generic_const_params = []
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_expr
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        rest = [ $other:tt $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_expr
            dst = [ $($dst)* ]
            expr = [ $($expr)* $other ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_where
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = []
    } => {
        $crate::ctx_set! {
            state = construct
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
        }
    };
    {
        state = parse_where
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ $name:ident <- get $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_bind_from
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* $name <- ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_where
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ const $name:ident : $ty:ty = $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_const_param
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* $name : $ty, ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_where
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ $name:ident $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_generic
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* $name, ]
            generic_bounds = [ $($generic_bound)* $name : ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_bind_from
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = []
    } => {
        $crate::ctx_set! {
            state = construct
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* , ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
        }
    };
    {
        state = parse_bind_from
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ , $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_where
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* , ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_bind_from
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ $other:tt $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_bind_from
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* $other ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_const_param
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = []
    } => {
        $crate::ctx_set! {
            state = construct
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* , ]
        }
    };
    {
        state = parse_const_param
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ , $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_where
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* , ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_const_param
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ $other:tt $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_const_param
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* $other ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_generic
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ = $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_generic_param
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* , ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_generic
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ : $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_generic_bound
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* 'static + ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_generic_bound
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ = $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_generic_param
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* , ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_generic_bound
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ $other:tt $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_generic_bound
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* $other ]
            generic_params = [ $($generic_param)* ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_generic_param
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = []
    } => {
        $crate::ctx_set! {
            state = construct
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* , ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
        }
    };
    {
        state = parse_generic_param
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ , $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_where
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* , ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = parse_generic_param
        dst = [ $($dst:tt)* ]
        expr = [ $($expr:tt)* ]
        binds = [ $($bind:tt)* ]
        generic_names = [ $($generic_name:tt)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:tt)* ]
        generic_const = [ $($generic_const:tt)* ]
        generic_const_params = [ $($generic_const_param:tt)* ]
        rest = [ $other:tt $($rest:tt)* ]
    } => {
        $crate::ctx_set! {
            state = parse_generic_param
            dst = [ $($dst)* ]
            expr = [ $($expr)* ]
            binds = [ $($bind)* ]
            generic_names = [ $($generic_name)* ]
            generic_bounds = [ $($generic_bound)* ]
            generic_params = [ $($generic_param)* $other ]
            generic_const = [ $($generic_const)* ]
            generic_const_params = [ $($generic_const_param)* ]
            rest = [ $($rest)* ]
        }
    };
    {
        state = construct
        dst = [ $dst:ty ]
        expr = [ $expr:expr ]
        binds = [ $($bind:ident <- $from:ty,)* ]
        generic_names = [ $($generic_name:ident,)* ]
        generic_bounds = [ $($generic_bound:tt)* ]
        generic_params = [ $($generic_param:ty,)* ]
        generic_const = [ $($generic_const:ident : $generic_const_type:ty,)* ]
        generic_const_params = [ $($generic_const_param:expr,)* ]
    } => {{
        #[doc(hidden)]
        #[allow(unused_parens)]
        struct __CustomSetAction
            <$($generic_name,)* $(const $generic_const : $generic_const_type,)*>
            (::core::marker::PhantomData<($($generic_name,)*)>);

        #[doc(hidden)]
        #[allow(unused_parens)]
        struct __CustomVariableList
            <Input: $crate::VariableList, $($generic_name,)* $(const $generic_const : $generic_const_type,)*>
            (::core::marker::PhantomData<(Input, $($generic_name,)*)>);

        #[doc(hidden)]
        impl<Input: $crate::VariableList, $($generic_name,)* $(const $generic_const : $generic_const_type,)*> $crate::VariableList
        for __CustomVariableList<Input, $($generic_name,)* $($generic_const,)*>
        where
            Input: $crate::VariableList,
            $($generic_bound)*
        {
            type Next = Input;
            type Key = <$dst as $crate::ConstVariable>::Key;
            type Value = <$dst as $crate::ConstVariable>::Value;
            const VALUE: $crate::VariableListValue<$crate::ConstValue> = $crate::VariableListValue::Has({
                $(let $bind = $crate::find_variable::<
                    Input,
                    <$from as $crate::ConstVariable>::Key,
                    <$from as $crate::ConstVariable>::Value>();)*
                $crate::ConstValue::new::<<$dst as $crate::ConstVariable>::Value>($expr)
            });
        }

        #[doc(hidden)]
        impl<$($generic_name,)* $(const $generic_const : $generic_const_type,)*> $crate::Action
        for __CustomSetAction<$($generic_name,)* $($generic_const,)*>
        where
            $($generic_bound)*
        {
            type Output = ();
            type Vars<Vars: $crate::VariableList> = __CustomVariableList<Vars, $($generic_name,)* $($generic_const,)*>;

            #[inline(always)]
            fn eval<Vars: $crate::VariableList>(self) -> Self::Output {
                #[allow(path_statements)]
                const {
                    <Self::Vars<Vars> as $crate::VariableList>::VALUE;
                }
            }
        }

        __CustomSetAction::<$($generic_param,)* $({ $generic_const_param },)*>(::core::marker::PhantomData)
    }};
}
