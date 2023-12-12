// src/lazy.rs

//! # Lazy Module
//! This module contains utilies related to lazy-initialization of data, including data structures.

use std::{marker::PhantomData, ops::{Index, IndexMut}};

use crate::untracked_mut;

pub enum MaybeInitialized<T> {
    Init(T),
    Uninit(fn() ->  T),
}

impl<T> MaybeInitialized<T> {
    pub(crate) fn init(self) ->T {
        if let MaybeInitialized::Init(i) = self {
            i
        } else if let MaybeInitialized::Uninit(u) = self {
            u()
        } else {
            unreachable!()
        }
    }

    pub(crate) fn init_ref(&self) -> &T {
        if let MaybeInitialized::Init(i) = self {
            i
        } else if let MaybeInitialized::Uninit(u) = self {
            *untracked_mut!(self, Self) = MaybeInitialized::Init(u());
            self.init_ref()
        } else {
            unreachable!()
        }
    }

    pub(crate) fn init_mut(&mut self) -> &mut T {
        if let MaybeInitialized::Init(i) = self {
            i
        } else if let MaybeInitialized::Uninit(u) = self {
            *self = MaybeInitialized::Init(u());
            self.init_mut()
        } else {
            unreachable!()
        }
    }

    pub const fn is_init(&self) -> bool {
        matches!(self, MaybeInitialized::Init(_))
    }
}

/// A vector that can contain immediate and lazily-evaluated values.
/// # Examples
/// Push a value lazyly using `push_lazy`:
/// ```
/// use memcmc::lazy::LazyVec;
/// 
/// let mut lazyvec: LazyVec<String> = LazyVec::new();
/// 
/// lazyvec.push_lazy(|| { "Hello!".to_string() });
/// ```
/// Push an immediate value using `push_immediate`:
/// ```
/// use memcmc::lazy::LazyVec;
/// 
/// let mut lazyvec: LazyVec<String> = LazyVec::new();
/// 
/// lazyvec.push_immediate("Hello!".to_string());
/// ```
pub struct LazyVec<T> {
    /// Inner vector
    inner: Vec<MaybeInitialized<T>>,
    /// This is necessary so [`LazyVec`] is `!Freeze`, because [`MaybeInitialized`] uses interior mutability to initialize itself in `init_ref`.
    _phantom_ptr: PhantomData<*mut ()>
}

impl<T> LazyVec<T> {
    /// Returns a new [`LazyVec`].
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let lazyvec: LazyVec<String> = LazyVec::new();
    #[inline]
    pub const fn new() -> Self {
        Self {
            inner: Vec::new(),
            _phantom_ptr: PhantomData,
        }
    }

    /// Pushes a lazily-evaluated value into the [`LazyVec`].
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let mut lazyvec: LazyVec<i32> = LazyVec::new();
    /// 
    /// lazyvec.push_lazy(|| { 33i32 });
    #[inline]
    pub fn push_lazy(&mut self, _closure: fn() -> T) {
        self.inner.push(MaybeInitialized::Uninit(_closure));
    }

    /// Pushes an immediate value into the [`LazyVec`].
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let mut lazyvec: LazyVec<i32> = LazyVec::new();
    /// 
    /// lazyvec.push_immediate(33);
    #[inline]
    pub fn push_immediate(&mut self, value: T) {
        self.inner.push(MaybeInitialized::Init(value));
    }

    /// Returns a new [`LazyVec`] with the specified capacity.
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let lazyvec: LazyVec<String> = LazyVec::with_capacity(4);
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            inner: Vec::with_capacity(capacity),
            _phantom_ptr: PhantomData,
        }
    }

    /// Pops a value from the LazyVec and returns it.
    /// * Returns [`None`] if the vector is empty.
    /// * Returns the value if it is already initialized.
    /// * If the value was pushed lazily, it is evaluated at the time it is pushed.
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let mut lazyvec: LazyVec<String> = LazyVec::new();
    /// 
    /// lazyvec.push_lazy(|| { "Hello!".to_string() });
    /// 
    /// assert!(matches!(lazyvec.pop(), Some(_)));
    /// ```
    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop().map(|v| v.init())
    }

    /// Creates a `LazyVec<T>` directly from a pointer, a capacity, and a length.
    ///
    /// # Safety
    ///
    /// This is highly unsafe, due to the number of invariants that aren't
    /// checked:
    ///
    /// * `ptr` must have been allocated using the global allocator, such as via
    ///   the [`alloc::alloc`] function.
    /// * `T` needs to have the same alignment as what `ptr` was allocated with.
    ///   (`T` having a less strict alignment is not sufficient, the alignment really
    ///   needs to be equal to satisfy the [`dealloc`] requirement that memory must be
    ///   allocated and deallocated with the same layout.)
    /// * The size of `T` times the `capacity` (ie. the allocated size in bytes) needs
    ///   to be the same size as the pointer was allocated with. (Because similar to
    ///   alignment, [`dealloc`] must be called with the same layout `size`.)
    /// * `length` needs to be less than or equal to `capacity`.
    /// * The first `length` values must be properly initialized values of type `T`.
    /// * `capacity` needs to be the capacity that the pointer was allocated with.
    /// * The allocated size in bytes must be no larger than `isize::MAX`.
    ///   See the safety documentation of [`pointer::offset`].
    ///
    /// These requirements are always upheld by any `ptr` that has been allocated
    /// via `LazyVec<T>`. Other allocation sources are allowed if the invariants are
    /// upheld.
    ///
    /// Violating these may cause problems like corrupting the allocator's
    /// internal data structures. For example it is normally **not** safe
    /// to build a `LazyVec<u8>` from a pointer to a C `char` array with length
    /// `size_t`, doing so is only safe if the array was initially allocated by
    /// a `LazyVec` or `String`.
    /// It's also not safe to build one from a `LazyVec<u16>` and its length, because
    /// the allocator cares about the alignment, and these two types have different
    /// alignments. The buffer was allocated with alignment 2 (for `u16`), but after
    /// turning it into a `LazyVec<u8>` it'll be deallocated with alignment 1. To avoid
    /// these issues, it is often preferable to do casting/transmuting using
    /// [`slice::from_raw_parts`] instead.
    ///
    /// The ownership of `ptr` is effectively transferred to the
    /// `LazyVec<T>` which may then deallocate, reallocate or change the
    /// contents of memory pointed to by the pointer at will. Ensure
    /// that nothing else uses the pointer after calling this
    /// function.
    ///
    /// [`String`]: std::string::String
    /// [`alloc::alloc`]: std::alloc::alloc
    /// [`dealloc`]: std::alloc::GlobalAlloc::dealloc
    #[inline]
    pub unsafe fn from_raw_parts(ptr: *mut MaybeInitialized<T>, length: usize, capacity: usize) -> Self {
        Self {
            inner: unsafe { Vec::from_raw_parts(ptr, length, capacity) },
            _phantom_ptr: PhantomData,
        }
    }

    /// Gets shared reference to value from at specified index.
    /// * Returns [`None`] if the specified position is out-of-range.
    /// * Returns the value if it is already initialized.
    /// * If the value was pushed lazily, it is evaluated at the time it is pushed.
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let mut lazyvec: LazyVec<String> = LazyVec::new();
    /// 
    /// lazyvec.push_lazy(|| { "Hello!".to_string() });
    /// 
    /// assert!(matches!(lazyvec.pop(), Some(_)));
    /// ```
    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(index).map(|v| v.init_ref())
    }

    /// Gets a mutable reference to the value at the specified index.
    /// * Returns [`None`] if the specified position is out-of-range.
    /// * Returns the value if it is already initialized.
    /// * If the value was pushed lazily, it is evaluated at the time it is pushed.
    /// # Examples
    /// ```
    /// use memcmc::lazy::LazyVec;
    /// 
    /// let mut lazyvec: LazyVec<String> = LazyVec::new();
    /// 
    /// lazyvec.push_lazy(|| { "Hello!".to_string() });
    /// 
    /// assert!(matches!(lazyvec.pop(), Some(_)));
    /// ```
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.inner.get_mut(index).map(|v| v.init_mut())
    }

    pub fn reverse(&mut self) {
        self.inner.reverse();
    }

    pub fn reversed(mut self) -> Self {
        self.inner.reverse();
        self
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> LazyIterRef<'_, T> {
        LazyIterRef::from_lazy_vec_ref(self)
    }
}

impl<T> Index<usize> for LazyVec<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.inner[index].init_ref()
    }
}

impl<T> IndexMut<usize> for LazyVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.inner[index].init_mut()
    }
}

impl<T> IntoIterator for LazyVec<T> {
    type IntoIter = LazyIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        LazyIter::from_lazy_vec(self)
    }
}

pub struct LazyIter<T> {
    inner: LazyVec<T>,
}

impl<T> LazyIter<T> {
    pub(crate) fn from_lazy_vec(lazy_vec: LazyVec<T>) -> Self {
        Self {
            inner: lazy_vec.reversed()
        }
    }
}

impl<T> Iterator for LazyIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.pop()
    }
}

pub struct LazyIterRef<'a, T> {
    inner: &'a LazyVec<T>,
    pos: usize,
}

impl<'a, T> LazyIterRef<'a, T> {
    pub(crate) fn from_lazy_vec_ref(lazy_vec_ref: &'a LazyVec<T>) -> Self {
        Self {
            inner: lazy_vec_ref,
            pos: 0
        }
    }
}

impl<'a, T> Iterator for LazyIterRef<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let pos = self.pos;
        self.pos += 1;
        self.inner.get(pos)
    }
}
