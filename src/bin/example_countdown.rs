#![feature(inline_const)]

use const_context::{ctx, Action};

#[derive(PartialEq, Eq)]
enum Countdown {
    Three,
    Two,
    One,
    Go,
}

type Count = (Countdown, Countdown);

fn three() -> impl Action {
    ctx! {
        let _ = println!("three.");
        set Count = Countdown::Three;
    }
}

fn two() -> impl Action {
    ctx! {
        let _ = println!("two.");
        set Count = match a {
            Countdown::Three => Countdown::Two,
            _ => panic!(),
        } where a <- get Count;
    }
}

fn one() -> impl Action {
    ctx! {
        let _ = println!("one.");
        set Count = match a {
            Countdown::Two => Countdown::One,
            _ => panic!(),
        } where a <- get Count;
    }
}

fn go() -> impl Action {
    ctx! {
        let _ = println!("go!");
        set Count = match a {
            Countdown::One => Countdown::Go,
            _ => panic!(),
        } where a <- get Count;
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
