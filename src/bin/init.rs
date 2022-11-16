use const_context::{ConstContext, ConstValue, ConstVariable};

mod need_init {
    use const_context::ConstContextAbstract;

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

    pub fn initialize<Ctx: ConstContextAbstract>(
        ctx: Ctx,
    ) -> Ctx::Push<Functions, { ConstValue::new(Functions) }> {
        initialize_value();
        ctx.push()
    }
}

fn main() {
    use need_init::Functions;

    let ctx = ConstContext::empty();
    let ctx = need_init::initialize(ctx);
    let funcs = ctx.get_runtime::<Functions>();

    println!("{}", funcs.foo());
}
