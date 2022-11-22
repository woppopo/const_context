#![feature(inline_const)]

use const_context::{ctx, EvaluatableAction, StartEvaluation, VariableList};

#[derive(PartialEq, Eq)]
enum Countdown {
    Three,
    Two,
    One,
    Go,
}

type Count = (Countdown, Countdown);

fn three<Vars: VariableList>() -> impl EvaluatableAction<Vars> {
    ctx! {
        let _ = println!("three.");
        set Count = Countdown::Three;
    }
}

fn two<Vars: VariableList>() -> impl EvaluatableAction<Vars> {
    ctx! {
        let _ = println!("two.");
        set Count = match a {
            Countdown::Three => Countdown::Two,
            _ => panic!(),
        }, where a = Count;
    }
}

fn one<Vars: VariableList>() -> impl EvaluatableAction<Vars> {
    ctx! {
        let _ = println!("one.");
        set Count = match a {
            Countdown::Two => Countdown::One,
            _ => panic!(),
        }, where a = Count;
    }
}

fn go<Vars: VariableList>() -> impl EvaluatableAction<Vars> {
    ctx! {
        let _ = println!("go!");
        set Count = match a {
            Countdown::One => Countdown::Go,
            _ => panic!(),
        }, where a = Count;
    }
}

fn main() {
    let action = ctx! {
        three();
        two();
        one();
        go();
    };

    action.start_eval();
}
