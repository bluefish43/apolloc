//! # The Memcmc Library
//! This library has various utilities and safe interfaces for interior mutability.
//! 
//! Use it as you may! This library is under the MIT license.
//! ## Cell module
//! The cell module contains utilities for using interfaces such as `Cell`, `IoUCell` and others, that do not include synchronization primitives in them by default, so don't use these in multithreaded enviroments.
//! ### Safe module
//! The safe module contains utilities such as `SafeCell`, `SafeIoUCell` and `AtomicCell`, that do include synchronization primitives by default, and thus, are suited for multithreaded enviroments.

#![allow(unused_unsafe)]
#![cfg_attr(feature = "no_std", no_std)]

#[cfg(feature = "cell")]
pub mod cell;

#[cfg(feature = "prelude")]
pub mod prelude;

#[cfg(feature = "lazy")]
#[cfg(not(feature = "no_std"))]
pub mod lazy;

#[macro_export]
macro_rules! untracked_mut {
    ($ref:expr, $t:ty) => {
        {
            unsafe { ($ref as *const $t as *mut $t).as_mut().unwrap() }
        }
    };
}

/// This wraps the expression in an unsafe block.
/// # Examples
/// ```
/// // import everything
/// use memcmc::prelude::*;
/// 
/// let mut value: i32 = 0;
/// // get pointer to variable
/// let pointer: *mut i32 = &mut value as *mut _;
/// 
/// // dereference pointer
/// let original_value = unsafexpr!( *pointer );
/// assert_eq!(original_value, 0);
/// ```
#[macro_export]
macro_rules! unsafexpr {
    ($e:expr) => {
        unsafe{$e}
    }
}

/// Creates a new `SafeIoUCell` from a value.
/// # Examples
/// ```
/// use memcmc::prelude::*;
/// static CELL: SafeIoUCell<'static, usize> = safe_iou!(33);
/// ```
/// This is how not to use it.
/// ```ignore
/// use memcmc::prelude::*;
/// 
/// let variable: usize = 33;
/// 
/// static CELL: SafeIoUCell<'static, usize> = safe_iou!(variable); // this creates an error, because variable is not contant.
/// ```
/// When using this macro, be certain that you do not use/capture any non-constant variables, or use a pure expression.
#[macro_export]
macro_rules! safe_iou {
    ($iou:expr) => {
        {
            use memcmc::cell::safe::SafeIoUCell;
            SafeIoUCell::new(|| { $iou })
        }
    }
}

/// Creates a new `IoUCell` from a value.
/// # Examples
/// ```
/// use memcmc::prelude::*;
/// static CELL: IoUCell<'static, usize> = unsafexpr!(iou!(33));
/// ```
/// This is how not to use it.
/// ```ignore
/// use memcmc::prelude::*;
/// 
/// let variable: usize = 33;
/// 
/// static CELL: IoUCell<'static, usize> = unsafexpr!(iou!(variable)); // this creates an error, because variable is not contant.
/// ```
/// When using this macro, be certain that you do not use/capture any non-constant variables, or use a pure expression.
#[macro_export]
macro_rules! iou {
    ($iou:expr) => {
        {
            use $crate::cell::IoUCell;
            IoUCell::new(|| { $iou })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cell::{CellInterface, Cell, IoUCell};
    #[cfg(feature = "safe")]
    use crate::cell::safe::{SafeIoUCell, MutexCell, MutexCloneCell};

    #[test]
    fn test_cell() {
        let cell = Cell::new(33);
        assert_eq!(cell.get(), 33);
        cell.set(44);
        assert_eq!(cell.get(), 44);
        cell.apply(|v| v + 6);
        assert_eq!(cell.get(), 50);
    }

    #[cfg(feature = "safe")]
    #[tokio::test]
    async fn test_safe_cell() {
        use crate::prelude::*;

        let example = MutexCell::new(33);
        cell_set_49(&example).await;
        assert_eq!(example.get(), 49);

        cell_set_31(&example).await;
        assert_eq!(example.get(), 31);
    }

    #[cfg(feature = "safe")]
    async fn cell_set_49(cell: &MutexCell<'_, i32>) {
        cell.set(49);
    }

    #[cfg(feature = "safe")]
    async fn cell_set_31(cell: &MutexCell<'_, i32>) {
        cell.set(31);
    }

    #[test]
    fn test_lazy_cell() {
        static CELL: IoUCell<'_, i32> = unsafe { IoUCell::new(|| { 33i32 }) };
        assert_eq!(*CELL.get(), 33);
        CELL.set(49);
        assert_eq!(*CELL.get(), 49);
        CELL.apply(|cv| *cv + 1);
        assert_eq!(*CELL.get(), 50);
    }

    #[test]
    #[cfg(not(feature = "no_std"))]
    fn test_lazy_cell_non_copy() {
        static CELL: IoUCell<'_, String> = unsafe { IoUCell::new(|| { String::from("Hello, world!") }) };
        assert_eq!(*CELL.get(), format!("Hello, world!"));
        CELL.set(String::from("Oops!"));
        assert_eq!(*CELL.get(), String::from("Oops!"));
        CELL.apply(|cv| cv.to_owned() + "!");
        assert_eq!(*CELL.get(), String::from("Oops!!"));
    }

    #[cfg(feature = "safe")]
    #[tokio::test]
    async fn test_safe_lazy_cell() {
        static CELL: SafeIoUCell<'_, i32> = SafeIoUCell::new(|| { 33i32 });
        set_50(&CELL).await;
        assert_eq!(*CELL.get(), 50);
    }

    #[cfg(feature = "safe")]
    async fn set_50(cell: &SafeIoUCell<'_, i32>) {
        cell.set(50);
    }

    #[cfg(feature = "safe")]
    #[test]
    fn test_safe_clone_cell() {
        let cell: MutexCloneCell<i32> = MutexCloneCell::new(33);
        assert_eq!(cell.get(), 33);
        cell.set(99);
        assert_eq!(cell.get(), 99);
        cell.apply(|v| v + 1);
        assert_eq!(cell.get(), 100);
        cell.apply_mut(|v| *v += 10);
        assert_eq!(cell.get(), 110);
        cell.set(0);
        assert_eq!(cell.unwrap(), 0);
    }
}
