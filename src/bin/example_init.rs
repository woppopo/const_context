use const_context::{ctx, ConstVariable, EvaluatableAction, StartEvaluation, VariableList};

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
    pub struct Functions(());

    impl Functions {
        pub fn foo(&self) -> u32 {
            get_value()
        }
    }

    impl ConstVariable for Functions {
        type Key = Self;
        type Value = Self;
    }

    pub fn initialize<Vars: VariableList>() -> impl EvaluatableAction<Vars, Output = ()> {
        ctx! {
            let _ = initialize_value();
            set Functions = Functions(());
        }
    }
}

fn main() {
    use need_init::Functions;

    // We cannot construct `Functions` ourself.
    //let functions = Functions(());

    let action = ctx! {
        need_init::initialize();
        funcs <- get Functions;
        let _ = println!("{}", funcs.foo());
    };

    action.start_eval();
}
