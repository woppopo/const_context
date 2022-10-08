#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use core::marker::PhantomData;

use const_context::{
    ConstContext, ConstContextMapper, ConstValue, ConstVariable, ConstVariableAssign as Assign,
    ConstVariables,
};

struct Name<const NAME: &'static str>;
type Var1 = (Name<"value1">, u32);
type Var2 = (Name<"value2">, u32);
type Var3 = (Name<"value3">, u32);

struct Add42<Key>(PhantomData<Key>);

impl<Key: 'static, const INPUT: ConstVariables> ConstContextMapper<INPUT> for Add42<Key> {
    const OUTPUT: ConstVariables = {
        let value = INPUT.get::<(Key, u32)>();
        INPUT.assign::<(Key, u32)>(ConstValue::new(value + 42))
    };
}

struct MapContext;

impl<const INPUT: ConstVariables> ConstContextMapper<INPUT> for MapContext {
    const OUTPUT: ConstVariables = {
        let a = INPUT.get::<Var1>();
        let b = INPUT.get::<Var2>();
        INPUT.assign::<Var3>(ConstValue::new(a * b))
    };
}

fn main() {
    let value = const {
        let ctx = ConstContext::empty();
        let ctx = ctx.map::<Assign<Var1, { ConstValue::new(42u32) }>>();
        let v1 = ctx.get::<Var1>();
        let ctx = ctx.map::<Assign<Var1, { ConstValue::new(8u32) }>>();
        let v2 = ctx.get::<Var1>();
        let ctx = ctx.map::<Add42<<Var1 as ConstVariable>::Key>>();
        let v3 = ctx.get::<Var1>();
        v1 + v2 + v3
    };

    println!("{}", value);

    let value = const {
        let ctx = ConstContext::empty();
        let ctx = ctx.map::<Assign<Var1, { ConstValue::new(6u32) }>>();
        let ctx = ctx.map::<Assign<Var2, { ConstValue::new(7u32) }>>();
        let ctx = ctx.map::<MapContext>();
        ctx.get::<Var3>()
    };

    println!("{}", value);
}
