#![feature(adt_const_params)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_context::{ConstContext, ConstContextPush, ConstValue, ConstVariable, Search};

struct Name<const NAME: &'static str>;
type Var1 = (Name<"value1">, u32);

type Push<Vars, Var, const VAL: ConstValue> =
    <ConstContext<Vars> as ConstContextPush<Var, VAL>>::Output;

const fn add42<Key, Vars>(
    ctx: ConstContext<Vars>,
) -> Push<
    Vars,
    (Key, u32),
    { ConstValue::new(ConstContext::<Vars>::get_from_type::<(Key, u32)>() + 42) },
>
where
    Key: 'static,
    Vars: Search<Key>,
{
    ctx.into()
}

fn main() {
    let value = const {
        let ctx = ConstContext::empty();
        let ctx = ctx.push::<Var1, { ConstValue::new(42u32) }>();
        let v1 = ctx.get::<Var1>();
        let ctx = ctx.push::<Var1, { ConstValue::new(8u32) }>();
        let v2 = ctx.get::<Var1>();
        let ctx = add42::<<Var1 as ConstVariable>::Key, _>(ctx);
        let v3 = ctx.get::<Var1>();
        v1 + v2 + v3
    };

    println!("{}", value);
}
