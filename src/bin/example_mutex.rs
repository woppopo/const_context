#![feature(inline_const)]

use core::ops::{Deref, DerefMut};
use std::sync::{LockResult, Mutex, MutexGuard, PoisonError};

use const_context::{ctx, ctx_if, Action, ConstVariable};

pub type Id = usize;

pub struct Locked<const ID: Id>;

impl<const ID: Id> ConstVariable for Locked<ID> {
    type Key = Self;
    type Value = Self;
}

pub struct StaticMutex<const ID: Id, T>(Mutex<T>);

impl<const ID: Id, T> StaticMutex<ID, T> {
    pub const fn new(value: T) -> Self {
        Self(Mutex::new(value))
    }

    pub fn lock(&self) -> impl Action<Output = LockResult<StaticMutexGuard<ID, T>>> {
        ctx_if! {
            if set Locked<{ ID }> then
                ctx! { panic "Double locks" }
            else
                ctx! {
                    set Locked<{ ID }> = Locked where const ID: Id = ID;
                    pure match self.0.lock() {
                        Ok(guard) => Ok(StaticMutexGuard(guard)),
                        Err(poison) => Err(PoisonError::new(StaticMutexGuard(poison.into_inner()))),
                    }
                }
        }
    }

    pub fn unlock(_: StaticMutexGuard<'_, ID, T>) -> impl Action {
        ctx! {
            unset Locked<ID>;
        }
    }
}

pub struct StaticMutexGuard<'a, const ID: Id, T>(MutexGuard<'a, T>);

impl<'a, const ID: Id, T> Deref for StaticMutexGuard<'a, ID, T> {
    type Target = MutexGuard<'a, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, const ID: Id, T> DerefMut for StaticMutexGuard<'a, ID, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn main() {
    let ref a1 = StaticMutex::<1, _>::new(1);
    let ref a2 = StaticMutex::<2, _>::new(2);

    let action = ctx! {
        r1 <- a1.lock();
        r2 <- a2.lock();
        let _v1 = r1.unwrap();
        let _v2 = r2.unwrap();
    };

    action.start_eval();
}
