#![feature(adt_const_params)]
#![feature(generic_const_exprs)]

use const_env::{ConstEnv, ConstValue, ConstVariables};

mod need_init {
    use super::*;

    static mut VALUE: Option<u32> = None;

    fn initialize_value() {
        unsafe { VALUE = Some(42) }
    }

    fn get_value() -> u32 {
        unsafe { VALUE.unwrap() }
    }

    #[derive(PartialEq, Eq)]
    pub struct Functions;

    impl Functions {
        pub const fn add_self(vars: ConstVariables) -> ConstVariables {
            vars.assign::<Self>(ConstValue::new(Self))
        }

        pub fn foo(&self) -> u32 {
            get_value()
        }
    }

    pub fn initialize<const VARS: ConstVariables>(
        _: ConstEnv<VARS>,
    ) -> ConstEnv<{ Functions::add_self(VARS) }> {
        initialize_value();
        ConstEnv
    }
}

fn main() {
    use need_init::Functions;

    let env = ConstEnv::empty();
    let env = need_init::initialize(env);
    let funcs = env.get::<Functions, Functions>();

    println!("{}", funcs.foo());
}
