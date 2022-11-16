#![feature(decl_macro)]

use const_context::{ctx, ConstVariable, StartEvaluation};

use crate::need_init::initialize;

mod need_init {
    use super::*;

    static mut VALUE: Option<u32> = None;

    // TODO: How to remove `pub` visibility?
    pub fn initialize_value() {
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

    pub macro initialize() {{
        ctx! {
            let _ = initialize_value();
            const Functions = Functions;
        }
    }}
}

fn main() {
    use need_init::Functions;

    let init = initialize!();
    let c = ctx! {
        init;
        funcs <= get Functions;
        let _ = println!("{}", funcs.foo());
    };

    c.start_eval();
}
