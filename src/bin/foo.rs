#![feature(adt_const_params)]
#![feature(generic_const_exprs)]

use const_context::{
    ConstContext, ConstContextAbstract, ConstContextGet, ConstValue, ConstVariable,
};

struct Name<const NAME: &'static str>;
type Var1 = (Name<"value1">, u32);

fn add42<Key, Ctx: ConstContextAbstract + ConstContextGet<(Key, u32)>>(
    ctx: Ctx,
) -> Ctx::Push<(Key, u32), { ConstValue::new(Ctx::GOT + 42) }>
where
    Key: 'static,
{
    ctx.push()
}

fn main() {
    let value = {
        let ctx = ConstContext::empty();
        let ctx = ctx.push::<Var1, { ConstValue::new(42u32) }>();
        let v1 = ctx.get_runtime::<Var1>();
        let ctx = ctx.push::<Var1, { ConstValue::new(8u32) }>();
        let v2 = ctx.get_runtime::<Var1>();
        let ctx = add42::<<Var1 as ConstVariable>::Key, _>(ctx);
        let v3 = ctx.get_runtime::<Var1>();
        v1 + v2 + v3
    };

    println!("{}", value);
}
