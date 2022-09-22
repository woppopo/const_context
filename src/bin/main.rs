#![feature(adt_const_params)]
#![feature(generic_arg_infer)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstVars};

const fn assign<const VARS: ConstVars, const VALUE: u32>(
    _: ConstEnv<VARS>,
) -> ConstEnv<{ VARS.assign::<u32, _>(VALUE) }> {
    ConstEnv
}

const fn get<const VARS: ConstVars>(_: &ConstEnv<VARS>) -> u32 {
    VARS.get::<u32, u32>()
}

fn main() {
    let env = ConstEnv::empty();
    let env = assign::<_, 42>(env);
    let v1 = get(&env);
    let env = assign::<_, 8>(env);
    let v2 = get(&env);

    println!("{}", v1 + v2);
}
