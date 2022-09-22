#![feature(adt_const_params)]
#![feature(generic_arg_infer)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstVars};

struct Key;

type ValueType = u32;

const fn assign<const VARS: ConstVars, const VALUE: ValueType>(
    _: ConstEnv<VARS>,
) -> ConstEnv<{ VARS.assign::<Key, _>(VALUE) }> {
    ConstEnv
}

fn main() {
    let value = const {
        let env = ConstEnv::empty();
        let env = assign::<_, 42>(env);
        let v1 = env.get::<Key, ValueType>();
        let env = assign::<_, 8>(env);
        let v2 = env.get::<Key, ValueType>();
        v1 + v2
    };

    println!("{}", value);
}
