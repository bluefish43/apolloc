//! # The Safe Module
//! Creates safe, multithreading-suited interfaces over interior mutability.

use crate::untracked_mut;
use core::{marker::PhantomData, mem::MaybeUninit, fmt::Debug, ops::{Deref, DerefMut}};
use parking_lot::{Mutex, MutexGuard};
use super::CellInterface;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(not(feature = "no_std"))] {
        use std::sync::Once;
    } else {
        use core::sync::atomic::{AtomicBool, Ordering};

        struct Once {
            state: AtomicBool,
        }

        impl Once {
            pub const fn new() -> Self {
                Self {
                    state: AtomicBool::new(false)
                }
            }
        
            pub fn call_once<F: FnOnce()>(&self, f: F) {
                if !self.state.load(Ordering::SeqCst) {
                    f();
                    self.state.store(true, Ordering::SeqCst);
                }
            }
        
            pub fn is_completed(&self) -> bool {
                self.state.load(Ordering::SeqCst)
            }
        }
    }
}

pub trait Synchronizer<'g, T, Guard: Deref<Target = T> + DerefMut> {
    fn create(value: T) -> Self;

    fn acquire(&'g self) -> Guard;

    fn data(&self) -> *mut T;

    fn inner(self) -> T;
}

impl<'g, T: 'g> Synchronizer<'g, T, MutexGuard<'g, T>> for Mutex<T> {
    fn create(value: T) -> Self {
        Self::new(value)
    }

    fn acquire(&'g self) -> MutexGuard<'g, T> {
        self.lock()
    }

    fn data(&self) -> *mut T {
        self.data_ptr()
    }

    fn inner(self) -> T {
        self.into_inner()
    }
}

/// Safe variation of the `cell::Cell`. This version is suitable for multi-threading.
pub struct SafeCell<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T: Copy> {
    inner: S,
    _phantom_data: PhantomData<&'g T>,
    _phantom_guard: PhantomData<Guard>,
}

/// `SafeCell` that uses a `parking_lot::Mutex` as `Synchronizer`.
pub type MutexCell<'g, T> = SafeCell<'g, MutexGuard<'g, T>, Mutex<T>, T>;

impl<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T: Copy> SafeCell<'g, Guard, S, T> {
    /// Creates a new `SafeCell`.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// let cell: MutexCell<'_, i128> = MutexCell::new(i128::MAX);
    /// ```
    pub fn new(inner: T) -> SafeCell<'g, Guard, S, T> {
        Self {
            inner: S::create(inner),
            _phantom_data: PhantomData,
            _phantom_guard: PhantomData,
        }
    }
}

impl<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T: Copy> Clone for SafeCell<'g, Guard, S, T> {
    fn clone(&self) -> Self {
        Self {
            inner: S::create(*(unsafe { *(S::acquire as *const fn(&S) -> Guard as *const fn(&Self) -> Guard) })(self)),
            _phantom_data: PhantomData,
            _phantom_guard: PhantomData,
        }
    }
}

impl<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T: Copy + Debug> Debug for SafeCell<'g, Guard, S, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "SafeCell({:?})", *(unsafe { *(S::acquire as *const fn(&S) -> Guard as *const fn(&Self) -> Guard) })(self))
    }
}

impl<'i, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'i, T, Guard> + 'i,  T: Copy> CellInterface<'i, T> for SafeCell<'i, Guard, S, T> {
    type Target = T;


    /// Gets the underlying [`Self::Target`] of the SafeCell.
    /// # Examples
    /// ```
    /// // import everything
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = MutexCell::new(34);
    /// 
    /// let value = cell.get();
    /// // this should always work
    /// assert_eq!(value, 34);
    /// ```
    fn get(&'i self) -> T {
        *self.inner.acquire()
    }

    /// Sets the underlying Cell's value.
    /// # Examples
    /// ```
    /// // import everything
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = SafeCell::new(34);
    /// // set its value
    /// cell.set(45);
    /// 
    /// // this should always work
    /// assert_eq!(cell.get(), 45);
    /// ```
    fn set(&'i self, value: T) {
        let _guard = self.inner.acquire();
        unsafe {
            let address = self.inner.data();
            address.write(value);
        }
    }

    /// Applies a closure to the underlying Cell's value.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = MutexCell::new(55);
    /// // apply
    /// cell.apply(|value| value + 5);
    /// 
    /// assert_eq!(cell.get(), 60);
    /// ```
    fn apply<F: Fn(&T) -> T>(&'i self, predicate: F) {
        let value = self.get();
        let result = predicate(&value);
        self.set(result);
    }

    /// Applies an in-place modification to the underlying Cell's value.
    /// 
    /// **WARNING**: Using this to reassign the underlying Cell's value can cause **undefined behaviour** and **memory leaks**.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = SafeCell::new(55);
    /// // apply
    /// cell.apply_mut(|value| *value += 5);
    /// 
    /// assert_eq!(cell.get(), 60);
    /// ```
    fn apply_mut<F: Fn(&mut T)>(&'i self, predicate: F) {
        let _guard = self.inner.acquire();
        let address = self.inner.data();
        let mut_reference = unsafe { address.as_mut().unwrap() };
        predicate(mut_reference);
    }

    fn unwrap(self) -> T
    where
        Self: 'i
    {
        self.inner.inner()
    }
}

/// Safe variant of `IoUCell`.
/// # Usage
/// This Cell is made when there is a need for an initialize-on-use variable on multithreaded enviroments.
/// 
/// Where you aren't in a multithreaded enviroment, using the single-threaded version of this, `IoUCell`, is recommended, because it does not use a `Mutex`, and thus it operates faster (generally).
pub struct SafeIoUCell<'a, T> {
    inner: Mutex<MaybeUninit<T>>,
    init: Once,
    _closure: fn() -> T,
    _phantom_lifetime: PhantomData<&'a ()>,
}

impl<'a, T> SafeIoUCell<'a, T> {
    /// Constructs a new `SafeIoUCell`.
    /// # Examples
    /// 
    pub const fn new(_closure: fn() -> T) -> Self {
        Self {
            inner: Mutex::new(MaybeUninit::uninit()),
            init: Once::new(),
            _closure,
            _phantom_lifetime: PhantomData,
        }
    }

    fn initialize(&self) {
        self.init.call_once(|| {
            let value = (self._closure)();
            *self.inner.lock() = MaybeUninit::new(value);
        });
    }

    fn initialize_or_dispose(&self) {
        if self.init.is_completed() {
            self.dispose_inner();
        } else {
            self.init.call_once(|| {
                let value = (self._closure)();
                *self.inner.lock() = MaybeUninit::new(value);
            });
            self.dispose_inner();
        }
    }

    pub fn get(&self) -> &T {
        self.initialize();

        unsafe { &*(*self.inner.lock()).as_ptr() }
    }

    fn dispose_inner(&self) {
        unsafe { untracked_mut!(&*self.inner.lock(), MaybeUninit<T>).assume_init_drop() };
    }
}

impl<'a, T: Clone> Clone for SafeIoUCell<'a, T> {
    fn clone(&self) -> Self {
        if self.init.is_completed() {
            let once = Once::new();
            once.call_once(|| {});
            Self {
                inner: Mutex::new(MaybeUninit::new(unsafe { self.inner.lock().assume_init_ref() }.clone())),
                init: once,
                _closure: self._closure,
                _phantom_lifetime: PhantomData,
            }
        } else {
            Self {
                inner: Mutex::new(MaybeUninit::uninit()),
                init: Once::new(),
                _closure: self._closure,
                _phantom_lifetime: PhantomData,
            }
        }
    }
}

impl<'a, T: Debug> Debug for SafeIoUCell<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "SafeIoUCell({:?})", unsafe { self.inner.lock().assume_init_ref() })
    }
}

impl<'a, T: 'a> CellInterface<'a, T> for SafeIoUCell<'a, T> {
    type Target = &'a T;
    
    fn get(&'a self) -> Self::Target {
        self.get()
    }

    fn set(&'a self, value: T) {
        self.initialize_or_dispose();
        unsafe { (self.inner.lock().assume_init_ref() as *const T as *mut T).write(value) }
    }

    fn apply<F: Fn(&T) -> T>(&self, predicate: F) {
        let value = self.get();
        let new_value = predicate(value);
        self.dispose_inner();
        self.set(new_value);
    }

    fn apply_mut<F: Fn(&mut T)>(&self, predicate: F) {
        self.initialize();
        let mut inner = self.inner.lock();
        let mut_reference = unsafe { inner.as_mut_ptr().as_mut().unwrap() };
        predicate(mut_reference);
    }

    fn unwrap(self) -> T {
        self.initialize();
        unsafe { self.inner.lock().assume_init_read() }
    }
}

/// Safe variation of the `cell::CloneCell`. This version is suitable for multi-threading.
pub struct SafeCloneCell<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T> {
    inner: S,
    _phantom_data: PhantomData<&'g T>,
    _phantom_guard: PhantomData<Guard>,
}

/// `SafeCloneCell` that uses a `parking_lot::Mutex` as `Synchronizer`.
pub type MutexCloneCell<'g, T> = SafeCloneCell<'g, MutexGuard<'g, T>, Mutex<T>, T>;

impl<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T> SafeCloneCell<'g, Guard, S, T> {
    /// Creates a new `SafeCloneCell`.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// let cell: MutexCell<'_, i128> = MutexCell::new(i128::MAX);
    /// ```
    pub fn new(inner: T) -> SafeCloneCell<'g, Guard, S, T> {
        Self {
            inner: S::create(inner),
            _phantom_data: PhantomData,
            _phantom_guard: PhantomData,
        }
    }
}

impl<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T: Clone> Clone for SafeCloneCell<'g, Guard, S, T> {
    fn clone(&self) -> Self {
        Self {
            inner: S::create((*(unsafe { *(S::acquire as *const fn(&S) -> Guard as *const fn(&Self) -> Guard) })(self)).clone()),
            _phantom_data: PhantomData,
            _phantom_guard: PhantomData,
        }
    }
}

impl<'g, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'g, T, Guard>, T: Debug> Debug for SafeCloneCell<'g, Guard, S, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "SafeCell({:?})", *(unsafe { *(S::acquire as *const fn(&S) -> Guard as *const fn(&Self) -> Guard) })(self))
    }
}

impl<'i, Guard: Deref<Target = T> + DerefMut, S: Synchronizer<'i, T, Guard> + 'i,  T> CellInterface<'i, T> for SafeCloneCell<'i, Guard, S, T> {
    type Target = T;


    /// Gets the underlying [`Self::Target`] of the SafeCloneCell.
    /// # Examples
    /// ```
    /// // import everything
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCloneCell<'_, usize> = MutexCloneCell::new(34);
    /// 
    /// let value = cell.get();
    /// // this should always work
    /// assert_eq!(value, 34);
    /// ```
    fn get(&'i self) -> T 
    where
        T: Clone
    {
        self.inner.acquire().clone()
    }

    /// Sets the underlying Cell's value.
    /// # Examples
    /// ```
    /// // import everything
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = SafeCell::new(34);
    /// // set its value
    /// cell.set(45);
    /// 
    /// // this should always work
    /// assert_eq!(cell.get(), 45);
    /// ```
    fn set(&'i self, value: T) {
        let _guard = self.inner.acquire();
        unsafe {
            let address = self.inner.data();
            address.write(value);
        }
    }

    /// Applies a closure to the underlying Cell's value.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = MutexCell::new(55);
    /// // apply
    /// cell.apply(|value| value + 5);
    /// 
    /// assert_eq!(cell.get(), 60);
    /// ```
    fn apply<F: Fn(&T) -> T>(&'i self, predicate: F) {
        let result = predicate(&*self.inner.acquire());
        self.set(result);
    }

    /// Applies an in-place modification to the underlying Cell's value.
    /// 
    /// **WARNING**: Using this to reassign the underlying Cell's value can cause **undefined behaviour** and **memory leaks**.
    /// # Examples
    /// ```
    /// use memcmc::prelude::*;
    /// 
    /// // create a new cell
    /// let cell: MutexCell<'_, usize> = SafeCell::new(55);
    /// // apply
    /// cell.apply_mut(|value| *value += 5);
    /// 
    /// assert_eq!(cell.get(), 60);
    /// ```
    fn apply_mut<F: Fn(&mut T)>(&'i self, predicate: F) {
        let _guard = self.inner.acquire();
        let address = self.inner.data();
        let mut_reference = unsafe { address.as_mut().unwrap() };
        predicate(mut_reference);
    }

    fn unwrap(self) -> T
    where
        Self: 'i
    {
        self.inner.inner()
    }
}
