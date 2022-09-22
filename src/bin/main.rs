#![feature(adt_const_params)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(inline_const)]

use const_env::{ConstEnv, ConstEnvAbstract, ConstEnvMap, ConstValue, ConstVarMap, ConstVariables};

struct Name<const NAME: &'static str>;

struct Add42;

impl const ConstVarMap for Add42 {
    type Input = u32;
    type Output = u32;

    fn map_var(value: Self::Input) -> Self::Output {
        value + 42
    }
}

struct MapEnv;

impl const ConstEnvMap for MapEnv {
    fn map_env(vars: ConstVariables) -> ConstVariables {
        let a = vars.get::<Name<"value1">, u32>();
        let b = vars.get::<Name<"value2">, u32>();
        vars.assign::<Name<"value3">>(ConstValue::new(a * b))
    }
}

fn _somefunc<Env: ConstEnvAbstract>(env: Env) -> impl ConstEnvAbstract
where
    Env::New<{ MapEnv::map_env(Env::VARS) }>:,
{
    env.map_env::<MapEnv>()
}

fn main() {
    let value = const {
        type Key = Name<"value">;

        let env = ConstEnv::empty();
        let env = env.assign::<Key, { ConstValue::new(42u32) }>();
        let v1 = env.get::<Key, u32>();
        let env = env.assign::<Key, { ConstValue::new(8u32) }>();
        let v2 = env.get::<Key, u32>();
        let env = env.map::<Key, Add42>();
        let v3 = env.get::<Key, u32>();
        v1 + v2 + v3
    };

    println!("{}", value);

    let value = const {
        let env = ConstEnv::empty();
        let env = env.assign::<Name<"value1">, { ConstValue::new(6u32) }>();
        let env = env.assign::<Name<"value2">, { ConstValue::new(7u32) }>();
        let env = env.map_env::<MapEnv>();
        env.get::<Name<"value3">, u32>()
    };

    println!("{}", value);
}
