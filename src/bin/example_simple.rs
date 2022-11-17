#![feature(generic_const_exprs)]

use const_context::{ctx, StartEvaluation};

struct Id<const N: usize>;
type Var1 = (Id<1>, u32);
type Var2 = (Id<2>, u32);
type Var3 = (Id<3>, u32);

fn main() {
    let action = ctx! {
        const Var1 = 42;
        v1 <= get Var1;
        const Var1 = 8;
        v2 <= get Var1;
        const Var1 = (v + 42) where v = Var1;
        v3 <= get Var1;
        pure (v1 + v2 + v3)
    };

    println!("{}", action.start_eval()); // 100

    let action = ctx! {
        const Var1 = 6;
        const Var2 = 7;
        const Var3 = (a * b) where a = Var1, b = Var2;
        get Var3
    };

    println!("{}", action.start_eval()); // 42
}
