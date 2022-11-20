# const_context

## Usage

```
[dependencies]
const_context = { git = "https://github.com/woppopo/const_context" }
```

```rust
#![feature(inline_const)]

use const_context::{ctx, Action, StartEvaluation, VariableList};

struct Key;
type IsInitialized = (Key, bool);

fn initialize<Vars: VariableList>() -> impl Action<Vars> {
    ctx! {
        set IsInitialized = true;
        let _ = println!("foo,");
    }
}

fn after_initialization<Vars: VariableList>() -> impl Action<Vars> {
    ctx! {
        set () = (assert!(is_initialized)) where is_initialized = IsInitialized;
        let _ = println!("bar.");
    }
}

fn main() {
    let action = ctx! {
        initialize();
        after_initialization();
    };
    action.start_eval();

    let action = ctx! {
        after_initialization(); // Emits compilation error.
        initialize();
    };
    action.start_eval();
}
```