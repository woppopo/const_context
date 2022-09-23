#![feature(adt_const_params)]
#![feature(generic_const_exprs)]

use const_context::{ConstContext, ConstValue, ConstVariable, ConstVariables};

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

    impl ConstVariable for Functions {
        type Key = Self;
        type Value = Self;
    }

    pub fn initialize<const VARS: ConstVariables>(
        _: ConstContext<VARS>,
    ) -> ConstContext<{ Functions::add_self(VARS) }> {
        initialize_value();
        ConstContext
    }
}

fn main() {
    use need_init::Functions;

    let ctx = ConstContext::empty();
    let ctx = need_init::initialize(ctx);
    let funcs = ctx.get::<Functions>();

    println!("{}", funcs.foo());
}
