#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_context::{
    ConstContext, ConstContextMapper, ConstValue, ConstVariableMapper, ConstVariables,
};

struct Name<const NAME: &'static str>;

struct Add42;

impl const ConstVariableMapper for Add42 {
    type Input = u32;
    type Output = u32;

    fn map(value: Self::Input) -> Self::Output {
        value + 42
    }
}

struct MapContext;

impl const ConstContextMapper for MapContext {
    fn map(vars: ConstVariables) -> ConstVariables {
        let a = vars.get::<Name<"value1">, u32>();
        let b = vars.get::<Name<"value2">, u32>();
        vars.assign::<Name<"value3">>(ConstValue::new(a * b))
    }
}

fn _somefunc<const VARS: ConstVariables>(
    _: ConstContext<VARS>,
) -> ConstContext<{ MapContext::map(VARS) }> {
    ConstContext
}

fn main() {
    let value = const {
        type Key = Name<"value">;

        let ctx = ConstContext::empty();
        let ctx = ctx.assign::<Key, { ConstValue::new(42u32) }>();
        let v1 = ctx.get::<Key, u32>();
        let ctx = ctx.assign::<Key, { ConstValue::new(8u32) }>();
        let v2 = ctx.get::<Key, u32>();
        let ctx = ctx.map_var::<Key, Add42>();
        let v3 = ctx.get::<Key, u32>();
        v1 + v2 + v3
    };

    println!("{}", value);

    let value = const {
        let ctx = ConstContext::empty();
        let ctx = ctx.assign::<Name<"value1">, { ConstValue::new(6u32) }>();
        let ctx = ctx.assign::<Name<"value2">, { ConstValue::new(7u32) }>();
        let ctx = ctx.map::<MapContext>();
        ctx.get::<Name<"value3">, u32>()
    };

    println!("{}", value);
}
