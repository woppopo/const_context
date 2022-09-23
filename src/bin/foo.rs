#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_context::{
    ConstContext, ConstContextMapper, ConstValue, ConstVariableMapper, ConstVariables,
};

struct Name<const NAME: &'static str>;

struct Add42;

impl<const NAME: &'static str> const ConstVariableMapper<(Name<NAME>, u32)> for Add42 {
    fn map(value: u32) -> u32 {
        value + 42
    }
}

struct MapContext;

impl const ConstContextMapper for MapContext {
    fn map(vars: ConstVariables) -> ConstVariables {
        type Var1 = (Name<"value1">, u32);
        type Var2 = (Name<"value2">, u32);
        type Var3 = (Name<"value2">, u32);

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
        type Var = (Name<"value">, u32);

        let ctx = ConstContext::empty();
        let ctx = ctx.assign::<Var, { ConstValue::new(42u32) }>();
        let v1 = ctx.get::<Var>();
        let ctx = ctx.assign::<Var, { ConstValue::new(8u32) }>();
        let v2 = ctx.get::<Var>();
        let ctx = ctx.map_var::<Var, Add42>();
        let v3 = ctx.get::<Var>();
        v1 + v2 + v3
    };

    println!("{}", value);

    let value = const {
        type Var1 = (Name<"value1">, u32);
        type Var2 = (Name<"value2">, u32);
        type Var3 = (Name<"value2">, u32);

        let ctx = ConstContext::empty();
        let ctx = ctx.assign::<Var1, { ConstValue::new(6u32) }>();
        let ctx = ctx.assign::<Var2, { ConstValue::new(7u32) }>();
        let ctx = ctx.map::<MapContext>();
        ctx.get::<Var3>()
    };

    println!("{}", value);
}
