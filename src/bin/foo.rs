#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use core::marker::PhantomData;

use const_context::{
    ConstContext, ConstContextMapper, ConstValue, ConstVariable, ConstVariableMapper,
    ConstVariables,
};

struct Name<const NAME: &'static str>;
type Var1 = (Name<"value1">, u32);
type Var2 = (Name<"value2">, u32);
type Var3 = (Name<"value3">, u32);

struct Add42<Key>(PhantomData<Key>);

impl<Key: 'static> const ConstVariableMapper for Add42<Key> {
    type Var = (Key, u32);
    fn map(value: u32) -> u32 {
        value + 42
    }
}

struct MapContext;

impl const ConstContextMapper for MapContext {
    fn map(vars: ConstVariables) -> ConstVariables {
        let a = vars.get::<Var1>();
        let b = vars.get::<Var2>();
        vars.assign::<Var3>(ConstValue::new(a * b))
    }
}

fn _somefunc<const VARS: ConstVariables>(
    _: ConstContext<VARS>,
) -> ConstContext<{ MapContext::map(VARS) }> {
    ConstContext
}

fn main() {
    let value = const {
        let ctx = ConstContext::empty();
        let ctx = ctx.map::<<Var1 as ConstVariable>::Assign<{ ConstValue::new(42u32) }>>();
        let v1 = ctx.get::<Var1>();
        let ctx = ctx.map::<<Var1 as ConstVariable>::Assign<{ ConstValue::new(8u32) }>>();
        let v2 = ctx.get::<Var1>();
        let ctx = ctx.map::<Add42<<Var1 as ConstVariable>::Key>>();
        let v3 = ctx.get::<Var1>();
        v1 + v2 + v3
    };

    println!("{}", value);

    let value = const {
        let ctx = ConstContext::empty();
        let ctx = ctx.map::<<Var1 as ConstVariable>::Assign<{ ConstValue::new(6u32) }>>();
        let ctx = ctx.map::<<Var2 as ConstVariable>::Assign<{ ConstValue::new(7u32) }>>();
        let ctx = ctx.map::<MapContext>();
        ctx.get::<Var3>()
    };

    println!("{}", value);
}
