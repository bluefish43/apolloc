use super::CellInterface;
use core::{fmt::Debug, sync::atomic::{Ordering, AtomicPtr}};

#[cfg(feature = "no_std")]
extern crate alloc;
#[cfg(feature = "no_std")]
use alloc::boxed::Box;

/// A Cell that uses atomic operations.
pub struct AtomicCell<T> {
    inner_value: AtomicPtr<T>,
}

impl<T> AtomicCell<T> {
    /// Creates a new `AtomicCell`.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// let cell = AtomicCell::new(33i32);
    /// ```
    pub fn new(value: T) -> AtomicCell<T> {
        let pointer = AtomicPtr::new(Box::into_raw(Box::new(value)));
        Self {
            inner_value: pointer,
        }
    }

    fn inner(&self) -> *mut T {
        self.inner_value.load(Ordering::SeqCst)
    }

    fn set_inner(&self, value: T) {
        let pointer = Box::into_raw(Box::new(value));
        let original_pointer = self.inner_value.swap(pointer, Ordering::SeqCst);
        let _ = unsafe { Box::from_raw(original_pointer) };
    }
}

impl<T: Clone> Clone for AtomicCell<T> {
    fn clone(&self) -> Self {
        let pointer = Box::into_raw(Box::new(self.get().clone()));
        Self {
            inner_value: AtomicPtr::new(pointer),
        }
    }
}

impl<T: Debug> Debug for AtomicCell<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "AtomicCell({:?})", self.get())
    }
}

impl<'a, T: 'a> CellInterface<'a, T> for AtomicCell<T> {
    type Target = &'a T;

    fn get(&'a self) -> Self::Target {
        unsafe { self.inner().as_ref().unwrap() }
    }

    fn set(&'a self, value: T) {
        self.set_inner(value);
    }

    fn apply<F: Fn(&T) -> T>(&self, predicate: F) {
        let inner = self.get();
        let result = predicate(inner);
        self.set_inner(result);
    }

    fn apply_mut<F: Fn(&mut T)>(&self, predicate: F) {
        let inner = unsafe { self.inner().as_mut().unwrap() };
        predicate(inner);
    }

    fn unwrap(self) -> T {
        unsafe { *Box::from_raw(self.inner()) }
    }
}

impl<T> Drop for AtomicCell<T> {
    fn drop(&mut self) {
        let _ = unsafe { Box::from_raw(self.inner()) };
    }
}