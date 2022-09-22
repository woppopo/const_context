#![feature(adt_const_params)]
#![feature(generic_arg_infer)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstVars};

struct Name<const NAME: &'static str>;

const fn modify(value: u32) -> u32 {
    value + 32
}

const fn assign_u32<const NAME: &'static str, const VALUE: u32, const VARS: ConstVars>(
    _: ConstEnv<VARS>,
) -> ConstEnv<{ VARS.assign::<Name<NAME>, _>(VALUE) }> {
    ConstEnv
}

const fn modify_u32<const NAME: &'static str, const VARS: ConstVars>(
    _: ConstEnv<VARS>,
) -> ConstEnv<{ VARS.assign::<Name<NAME>, _>(modify(VARS.get::<Name<NAME>, _>())) }> {
    ConstEnv
}

fn main() {
    let value = const {
        let env = ConstEnv::empty();
        let env = assign_u32::<"value", 42, _>(env);
        let v1 = env.get::<Name<"value">, u32>();
        let env = assign_u32::<"value", 8, _>(env);
        let v2 = env.get::<Name<"value">, u32>();
        let env = modify_u32::<"value", _>(env);
        let v3 = env.get::<Name<"value">, u32>();
        v1 + v2 + v3
    };

    println!("{}", value);
}
