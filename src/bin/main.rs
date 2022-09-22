#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_arg_infer)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstVar, ConstVarMap};

struct Name<const NAME: &'static str>;

type Key = Name<"value">;

const fn var(value: u32) -> ConstVar {
    ConstVar::new::<Key, u32>(value)
}

struct Add42;

impl const ConstVarMap for Add42 {
    type Input = u32;
    type Output = u32;

    fn map(value: Self::Input) -> Self::Output {
        value + 42
    }
}

fn main() {
    let value = const {
        let env = ConstEnv::empty();
        let env = env.assign::<{ var(42) }>();
        let v1 = env.get::<Name<"value">, u32>();
        let env = env.assign::<{ var(8) }>();
        let v2 = env.get::<Name<"value">, u32>();
        let env = env.map::<Key, Add42>();
        let v3 = env.get::<Name<"value">, u32>();
        v1 + v2 + v3
    };

    println!("{}", value);
}
