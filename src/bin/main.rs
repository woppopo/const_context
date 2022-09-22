#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_arg_infer)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstMap, ConstValue};

struct Name<const NAME: &'static str>;

struct Add42;

impl const ConstMap for Add42 {
    type Input = u32;
    type Output = u32;

    fn map(value: Self::Input) -> Self::Output {
        value + 42
    }
}

fn main() {
    let value = const {
        type Key = Name<"value">;

        let env = ConstEnv::empty();
        let env = env.assign::<Key, { ConstValue::new(42u32) }>();
        let v1 = env.get::<Key, u32>();
        let env = env.assign::<Key, { ConstValue::new(42u32) }>();
        let v2 = env.get::<Key, u32>();
        let env = env.map::<Key, Add42>();
        let v3 = env.get::<Key, u32>();
        v1 + v2 + v3
    };

    println!("{}", value);
}
