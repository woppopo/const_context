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

const fn get<const VARS: ConstVars>(_: &ConstEnv<VARS>) -> ValueType {
    VARS.get::<Key, ValueType>()
}

fn main() {
    let value = const {
        let env = ConstEnv::empty();
        let env = assign::<_, 42>(env);
        let v1 = get(&env);
        let env = assign::<_, 8>(env);
        let v2 = get(&env);
        v1 + v2
    };

    println!("{}", value);
}
