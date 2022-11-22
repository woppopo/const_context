# const_context

## Usage

```
[dependencies]
const_context = { git = "https://github.com/woppopo/const_context" }
```

```rust
#![feature(inline_const)]

use const_context::{ctx, Action};

struct Key;
type IsInitialized = (Key, bool);

fn initialize() -> impl Action {
    ctx! {
        set IsInitialized = true;
        let _ = println!("foo,");
    }
}

fn after_initialization() -> impl Action {
    ctx! {
        set () = assert!(is_initialized),
        where
            is_initialized = IsInitialized;
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