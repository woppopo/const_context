#![feature(adt_const_params)]
#![feature(generic_arg_infer)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstVars};

struct Name<const NAME: &'static str>;

const fn assign_u32<const NAME: &'static str, const VALUE: u32, const VARS: ConstVars>(
    _: ConstEnv<VARS>,
) -> ConstEnv<{ VARS.assign::<Name<NAME>, _>(VALUE) }> {
    ConstEnv
}

fn main() {
    let value = const {
        let env = ConstEnv::empty();
        let env = assign_u32::<"value", 42, _>(env);
        let v1 = env.get::<Name<"value">, u32>();
        let env = assign_u32::<"value", 8, _>(env);
        let v2 = env.get::<Name<"value">, u32>();
        v1 + v2
    };

    println!("{}", value);
}
