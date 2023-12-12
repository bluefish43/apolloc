//! # The Cell module
//! This module contains various interfaces for interior mutability suited for single-threaded enviroments.
//! 
//! If you are in a multi-threaded enviroment, use the utilities from the `safe` module instead

#[cfg(feature = "safe")]
pub mod safe;

pub mod atomic;

use crate::untracked_mut;
use core::{marker::PhantomData, sync::atomic::{AtomicBool, Ordering}, ptr::addr_of, mem::MaybeUninit, fmt::Debug};

/// An interface to unify multiple cell types as a single type trait.
pub trait CellInterface<'i, T> {
    type Target;

    /// Gets the underlying [`Self::Target`] of the Cell.
    fn get(&'i self) -> Self::Target
    where
        Self::Target: Clone;

    /// Sets the underlying Cell's value.
    fn set(&'i self, value: T);

    /// Applies a closure to the underlying Cell's value.
    fn apply<F: Fn(&T) -> T>(&'i self, predicate: F);

    /// Applies an in-place modification to the underlying Cell's value.
    /// 
    /// ## **WARNING**: Using this to reassign the underlying Cell's value can cause **undefined behaviour** and **memory leaks**.
    /// ## Below is an example of where this can happen:
    /// ```ignore
    /// use memcmc::prelude::*;
    /// 
    /// let cell = unsafexpr!(iou!("Hello!".to_owned()));
    /// 
    /// cell.apply_mut(|value| {
    ///     // as the value contained before is not dropped,
    ///     // this causes a memory leak.
    ///     *value = "Hi!".to_owned();
    /// });
    /// ```
    /// ## Use the `set` method to reassign the inner value instead of this. `set` correctly drops the value before setting the new one.
    fn apply_mut<F: Fn(&mut T)>(&'i self, predicate: F);

    /// Unwraps this Cell's inner value.
    fn unwrap(self) -> T
    where
        Self: 'i;
}

/// The `Cell` struct is used for interior mutability with types that are `Copy`.
/// # Multithreading
/// If you are in a multithreading enviroment, it is highly recomended to use its safe variant, `SafeCell`, instead.
/// # Interior mutability
/// This struct has an internal `PhantomData` with a `*mut ()` to attribute `!Send`, `!Sync` and `!Freeze` to this type, ensuring we don't try to write to read-only memory.
pub struct Cell<T: Copy> {
    /// Internal data field
    inner: T,
    /// PhantomData of pointer for traits `!Send`, `!Sync` and `!Freeze`
    _neg_freeze_phantom: PhantomData<*mut ()>,
}

impl<T: Copy> Cell<T> {
    /// This function returns a new `Cell`.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// let cell: Cell<i32> = Cell::new(-1);
    /// ```
    pub fn new(inner: T) -> Cell<T> {
        Self {
            inner,
            _neg_freeze_phantom: PhantomData,
        }
    }
}

impl<T: Copy + Debug> Debug for Cell<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Cell({:?})", self.inner)
    }
}

impl<T: Copy> Clone for Cell<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Copy> Copy for Cell<T> {}

impl<'i, T: Copy> CellInterface<'i, T> for Cell<T> {
    type Target = T;
    fn get(&self) -> T {
        self.inner
    }

    fn set(&self, value: T) {
        unsafe {
            let address = addr_of!(self.inner);
            (address as *mut T).write(value);
        }
    }

    fn apply<F: Fn(&T) -> T>(&self, predicate: F) {
        let value = self.get();
        let result = predicate(&value);
        self.set(result);
    }

    fn apply_mut<F: Fn(&mut T)>(&self, predicate: F) {
        let address = addr_of!(self.inner) as *mut T;
        let mut_reference = unsafe { address.as_mut().unwrap() };
        predicate(mut_reference);
    }

    fn unwrap(self) -> T {
        self.inner
    }
}

/// Initialize-on-Use cell for singlethreaded enviroments.
/// Using this struct in multithreaded enviroments is highly discouraged.
/// # Usage
/// You may use this struct where you need lazy initialization of global variables in a single-threaded enviroments.
/// 
/// If you are in a multithreaded enviroment, you may use `SafeIoUCell` instead.
/// # Examples
/// ```
/// use memcmc::prelude::*;
/// 
/// static CELL: IoUCell<'static, String> = unsafe { IoUCell::new(|| { "Hello, world!".to_string() }) };
/// 
/// let mut value: &str = CELL.get();
/// assert_eq!(value, "Hello, world!");
/// 
/// CELL.set("Oops!".to_string());
/// 
/// value = CELL.get();
/// assert_eq!(value, "Oops!");
/// ```
pub struct IoUCell<'a, T> {
    inner: MaybeUninit<T>,
    init: AtomicBool,
    _closure: fn() -> T,
    _phantom_lifetime: PhantomData<&'a ()>,
    _phantom_unfreeze: PhantomData<*mut ()>,
}

impl<'a, T> IoUCell<'a, T> {
    /// This function creates a new IoUCell<'a, T>.
    /// # Safety
    /// This function is marked as `unsafe` because it is not safe to use this variant on multithreaded enviroments, even though you CAN assign this to a static variable.
    /// If you are in a multithreaded enviroment, it is **MUCH** recomended to use this struct's safe version, `SafeIoUCell<'a, T>`.
    /// # Usage
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// static CELL: IoUCell<'static, String> = unsafe { IoUCell::new(|| { "Hello, world!".to_string() }) };
    /// ```
    pub const unsafe fn new(_closure: fn() -> T) -> Self {
        Self {
            inner: MaybeUninit::uninit(),
            init: AtomicBool::new(false),
            _closure,
            _phantom_lifetime: PhantomData,
            _phantom_unfreeze: PhantomData,
        }
    }

    /// Returns if the `IoUCell` inner value has been initialized yet.
    pub fn is_init(&self) -> bool {
        self.init.load(Ordering::SeqCst)
    }

    /// Initializes the inner `IoUCell` if it hasn't been initialized yet.
    #[allow(invalid_reference_casting)]
    fn init(&self) {
        if !self.is_init() {
            self.init.store(true, Ordering::SeqCst);
            unsafe { *(self.inner.assume_init_ref() as *const T as *mut T) = (self._closure)() }
        }
    }
}

impl<'a, T: Clone> Clone for IoUCell<'a, T> {
    fn clone(&self) -> Self {
        if self.is_init() {
            Self {
                inner: MaybeUninit::new(unsafe { self.inner.assume_init_ref() }.clone()),
                init: AtomicBool::new(true),
                _closure: self._closure,
                _phantom_lifetime: PhantomData,
                _phantom_unfreeze: PhantomData,
            }
        } else {
            Self {
                inner: MaybeUninit::uninit(),
                init: AtomicBool::new(false),
                _closure: self._closure,
                _phantom_lifetime: PhantomData,
                _phantom_unfreeze: PhantomData,
            }
        }
    }
}

impl<'a, T: Debug> Debug for IoUCell<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.init();
        write!(f, "IoUCell({:?})", unsafe { self.inner.assume_init_ref() })
    }
}

impl<'a, T: 'a> CellInterface<'a, T> for IoUCell<'a, T> {
    type Target = &'a T;

    fn get(&'a self) -> Self::Target {
        if self.is_init() {
            return unsafe { self.inner.assume_init_ref() }
        } else {
            self.init();
            return unsafe { self.inner.assume_init_ref() };
        }
    }

    fn set(&'a self, value: T) {
        self.init();
        unsafe { untracked_mut!(&self.inner, MaybeUninit<T>).assume_init_drop() };
        unsafe { (self.inner.assume_init_ref() as *const T as *mut T).write(value) }
    }

    fn apply<F: Fn(&T) -> T>(&self, predicate: F) {
        let reference = self.get();
        let result = predicate(reference);
        self.set(result);
    }

    fn apply_mut<F: Fn(&mut T)>(&self, predicate: F) {
        self.init();
        let address = addr_of!(self.inner) as *mut T;
        let mut_reference = unsafe { address.as_mut().unwrap() };
        predicate(mut_reference);
    }

    fn unwrap(self) -> T {
        self.init();
        unsafe { self.inner.assume_init_read() }
    }
}

impl<'a, T> Drop for IoUCell<'a, T> {
    fn drop(&mut self) {
        if self.is_init() {
            unsafe { self.inner.assume_init_drop() };
        }
    }
}

unsafe impl<'a, T> Send for IoUCell<'a, T> {}
unsafe impl<'a, T> Sync for IoUCell<'a, T> {}

/// The `CloneCell` struct is used for interior mutability.
/// # Multithreading
/// If you are in a multithreading enviroment, it is highly recomended to use its safe variant, `SafeCloneCell`, instead.
/// # Interior mutability
/// This struct has an internal `PhantomData` with a `*mut ()` to attribute `!Send`, `!Sync` and `!Freeze` to this type, ensuring we don't try to write to read-only memory.
pub struct CloneCell<T> {
    /// Internal data field
    inner: T,
    /// PhantomData of pointer for traits `!Send`, `!Sync` and `!Freeze`
    _neg_freeze_phantom: PhantomData<*mut ()>,
}

impl<T> CloneCell<T> {
    /// This function returns a new `CloneCell`.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// let cell: CloneCell<i32> = CloneCell::new(-1);
    /// ```
    pub fn new(inner: T) -> CloneCell<T> {
        Self {
            inner,
            _neg_freeze_phantom: PhantomData,
        }
    }
}

impl<T: Debug> Debug for CloneCell<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "CloneCell({:?})", self.inner)
    }
}

impl<T: Clone> Clone for CloneCell<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _neg_freeze_phantom: PhantomData,
        }
    }
}

impl<T: Copy> Copy for CloneCell<T> {}

impl<'i, T: Clone> CellInterface<'i, T> for CloneCell<T> {
    type Target = T;
    fn get(&self) -> T {
        self.inner.clone()
    }

    fn set(&self, value: T) {
        unsafe {
            let address = addr_of!(self.inner);
            (address as *mut T).write(value);
        }
    }

    fn apply<F: Fn(&T) -> T>(&self, predicate: F) {
        let value = self.get();
        let result = predicate(&value);
        self.set(result);
    }

    fn apply_mut<F: Fn(&mut T)>(&self, predicate: F) {
        let address = addr_of!(self.inner) as *mut T;
        let mut_reference = unsafe { address.as_mut().unwrap() };
        predicate(mut_reference);
    }

    fn unwrap(self) -> T {
        self.inner
    }
}
