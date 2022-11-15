#![feature(adt_const_params)]
#![feature(generic_const_exprs)]

use const_context::{ConstContext, ConstContextPush, ConstValue, ConstVariable};

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
        pub fn foo(&self) -> u32 {
            get_value()
        }
    }

    impl ConstVariable for Functions {
        type Key = Self;
        type Value = Self;
    }

    pub fn initialize<Vars>(
        ctx: ConstContext<Vars>,
    ) -> <ConstContext<Vars> as ConstContextPush<Functions, { ConstValue::new(Functions) }>>::Output
    {
        initialize_value();
        ctx.into()
    }
}

fn main() {
    use need_init::Functions;

    let ctx = ConstContext::empty();
    let ctx = need_init::initialize(ctx);
    let funcs = ctx.get::<Functions>();

    println!("{}", funcs.foo());
}
