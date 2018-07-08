//! Bus provides a lock-free, bounded, single-producer, multi-consumer, broadcast channel.
//!
//! It uses a circular buffer and atomic instructions to implement a lock-free single-producer,
//! multi-consumer channel. The interface is similar to that of the `std::sync::mpsc` channels,
//! except that multiple consumers (readers of the channel) can be produced, whereas only a single
//! sender can exist. Furthermore, in contrast to most multi-consumer FIFO queues, bus is
//! *broadcast*; every send goes to every consumer.
//!
//! I haven't seen this particular implementation in literature (some extra bookkeeping is
//! necessary to allow multiple consumers), but a lot of related reading can be found in Ross
//! Bencina's blog post ["Some notes on lock-free and wait-free
//! algorithms"](http://www.rossbencina.com/code/lockfree).
//!
//! Bus achieves broadcast by cloning the element in question, which is why `T` must implement
//! `Clone`. However, Bus is clever about only cloning when necessary. Specifically, the last
//! consumer to see a given value will move it instead of cloning, which means no cloning is
//! happening for the single-consumer case. For cases where cloning is expensive, `Arc` should be
//! used instead.
//!
//! In a single-producer, single-consumer setup (which is the only one that Bus and
//! `mpsc::sync_channel` both support), Bus gets ~2x the performance of `mpsc::sync_channel` on
//! my machine. YMMV. You can check your performance on Nightly using
//!
//! ```console
//! $ cargo bench --features bench
//! ```
//!
//! To see multi-consumer results, run the benchmark utility instead (should work on stable too)
//!
//! ```console
//! $ cargo build --bin bench --release
//! $ target/release/bench
//! ```
//!
//! # Examples
//!
//! Single-send, multi-consumer example
//!
//! ```rust
//! use bus::Bus;
//! let mut bus = Bus::new(10);
//! let mut rx1 = bus.add_rx();
//! let mut rx2 = bus.add_rx();
//!
//! bus.broadcast("Hello");
//! assert_eq!(rx1.recv(), Ok("Hello"));
//! assert_eq!(rx2.recv(), Ok("Hello"));
//! ```
//!
//! Multi-send, multi-consumer example
//!
//! ```rust
//! use bus::Bus;
//! use std::thread;
//!
//! let mut bus = Bus::new(10);
//! let mut rx1 = bus.add_rx();
//! let mut rx2 = bus.add_rx();
//!
//! // start a thread that sends 1..100
//! let j = thread::spawn(move || {
//!     for i in 1..100 {
//!         bus.broadcast(i);
//!     }
//! });
//!
//! // every value should be received by both receivers
//! for i in 1..100 {
//!     // rx1
//!     assert_eq!(rx1.recv(), Ok(i));
//!     // and rx2
//!     assert_eq!(rx2.recv(), Ok(i));
//! }
//!
//! j.join().unwrap();
//! ```
//!
//! Many-to-many channel using a dispatcher
//!
//! ```rust
//! use bus::Bus;
//!
//! use std::thread;
//! use std::sync::mpsc;
//!
//! // set up fan-in
//! let (tx1, mix_rx) = mpsc::sync_channel(100);
//! let tx2 = tx1.clone();
//! // set up fan-out
//! let mut mix_tx = Bus::new(100);
//! let mut rx1 = mix_tx.add_rx();
//! let mut rx2 = mix_tx.add_rx();
//! // start dispatcher
//! thread::spawn(move || {
//!     for m in mix_rx.iter() {
//!         mix_tx.broadcast(m);
//!     }
//! });
//!
//! // sends on tx1 are received ...
//! tx1.send("Hello").unwrap();
//!
//! // ... by both receiver rx1 ...
//! assert_eq!(rx1.recv(), Ok("Hello"));
//! // ... and receiver rx2
//! assert_eq!(rx2.recv(), Ok("Hello"));
//!
//! // same with sends on tx2
//! tx2.send("world").unwrap();
//! assert_eq!(rx1.recv(), Ok("world"));
//! assert_eq!(rx2.recv(), Ok("world"));
//! ```

#![deny(missing_docs)]
#![cfg_attr(feature = "bench", feature(test))]

extern crate atomic_option;
use atomic_option::AtomicOption;

extern crate futures;
use futures::prelude::*;
use futures::sync::mpsc;
use futures::task;

#[macro_use]
extern crate log;

extern crate void;

#[cfg(feature = "bench")]
extern crate test;

use std::sync::atomic;

use std::cell::UnsafeCell;
use std::ops::Deref;
use std::sync::Arc;

struct SeatState<T> {
    max: usize,
    val: Option<T>,
}

struct MutSeatState<T>(UnsafeCell<SeatState<T>>);
unsafe impl<T> Sync for MutSeatState<T> {}
impl<T> Deref for MutSeatState<T> {
    type Target = UnsafeCell<SeatState<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A Seat is a single location in the circular buffer.
/// Each Seat knows how many readers are expected to access it, as well as how many have. The
/// producer will never modify a seat's state unless all readers for a particular seat have either
/// called `.take()` on it, or have left (see `Bus.rleft`).
///
/// The producer walks the seats of the ring in order, and will always only modify the seat at
/// `tail + 1` once all readers have finished with the seat at `head + 2`. A reader will never
/// access a seat unless it is between the reader's `head` and the producer's `tail`. Together,
/// these properties ensure that a Seat is either accessed only by readers, or by only the
/// producer.
///
/// The `read` attribute is used to ensure that readers see the most recent write to the seat when
/// they access it. This is done using `atomic::Ordering::Acquire` and `atomic::Ordering::Release`.
struct Seat<T> {
    read: atomic::AtomicUsize,
    state: MutSeatState<T>,

    // is the writer waiting for this seat to be emptied? needs to be atomic since both the last
    // reader and the writer might be accessing it at the same time.
    waiting: AtomicOption<task::Task>,
}

impl<T: Clone + Sync> Seat<T> {
    /// take is used by a reader to extract a copy of the value stored on this seat. only readers
    /// that were created strictly before the time this seat was last written to by the producer
    /// are allowed to call this method, and they may each only call it once.
    fn take(&self) -> T {
        let read = self.read.load(atomic::Ordering::Acquire);

        // the writer will only modify this element when .read hits .max - writer.rleft[i]. we can
        // be sure that this is not currently the case (which means it's safe for us to read)
        // because:
        //
        //  - .max is set to the number of readers at the time when the write happens
        //  - any joining readers will start at a later seat
        //  - so, at most .max readers will call .take() on this seat this time around the buffer
        //  - a reader must leave either *before* or *after* a call to recv. there are two cases:
        //
        //    - it leaves before, rleft is decremented, but .take is not called
        //    - it leaves after, .take is called, but head has been incremented, so rleft will be
        //      decremented for the *next* seat, not this one
        //
        //    so, either .take is called, and .read is incremented, or writer.rleft is incremented.
        //    thus, for a writer to modify this element, *all* readers at the time of the previous
        //    write to this seat must have either called .take or have left.
        //  - since we are one of those readers, this cannot be true, so it's safe for us to assume
        //    that there is no concurrent writer for this seat
        let state = unsafe { &*self.state.get() };
        assert!(
            read < state.max,
            "reader hit seat with exhausted reader count"
        );

        let mut waiting = None;

        // NOTE
        // we must extract the value *before* we decrement the number of remaining items otherwise,
        // the object might be replaced by the time we read it!
        let v = if read + 1 == state.max {
            // we're the last reader, so we may need to notify the writer there's space in the buf.
            // can be relaxed, since the acquire at the top already guarantees that we'll see
            // updates.
            waiting = self.waiting.take(atomic::Ordering::Relaxed);

            // since we're the last reader, no-one else will be cloning this value, so we can
            // safely take a mutable reference, and just take the val instead of cloning it.
            unsafe { &mut *self.state.get() }.val.take().unwrap()
        } else {
            state
                .val
                .clone()
                .expect("seat that should be occupied was empty")
        };

        // let writer know that we no longer need this item.
        // state is no longer safe to access.
        #[cfg_attr(feature = "cargo-clippy", allow(drop_ref))]
        drop(state);
        self.read.fetch_add(1, atomic::Ordering::AcqRel);

        if let Some(t) = waiting {
            // writer was waiting for us to finish with this
            t.notify();
        }

        v
    }
}

impl<T> Default for Seat<T> {
    fn default() -> Self {
        Seat {
            read: atomic::AtomicUsize::new(0),
            waiting: AtomicOption::empty(),
            state: MutSeatState(UnsafeCell::new(SeatState { max: 0, val: None })),
        }
    }
}

/// `BusInner` encapsulates data that both the writer and the readers need to access. The tail is
/// only ever modified by the producer, and read by the consumers. The length of the bus is
/// instantiated when the bus is created, and is never modified.
struct BusInner<T> {
    ring: Vec<Seat<T>>,
    len: usize,
    tail: atomic::AtomicUsize,
    closed: atomic::AtomicBool,
}

struct WaitingReader {
    task: Option<task::Task>,
    at: usize,
}

impl WaitingReader {
    fn current(at: usize) -> Self {
        WaitingReader { task: Some(task::current()), at }
    }

    fn notify(self) {
        // do nothing but drop self
    }
}

impl Drop for WaitingReader {
    fn drop(&mut self) {
        if let Some(t) = self.task.take() {
            t.notify()
        }
    }
}

/// `Bus` is the main interconnect for broadcast messages. It can be used to send broadcast
/// messages, or to connect additional consumers. When the `Bus` is dropped, receivers will
/// continue receiving any outstanding broadcast messages they would have received if the bus were
/// not dropped. After all those messages have been received, any subsequent receive call on a
/// receiver will return a disconnected error.
pub struct Bus<T> {
    state: Arc<BusInner<T>>,

    // current number of readers
    readers: usize,

    // rleft keeps track of readers that should be skipped for each index. we must do this because
    // .read will be < max for those indices, even though all active readers have received them.
    rleft: Vec<usize>,

    // leaving is used by receivers to signal that they are done
    leaving: (mpsc::UnboundedSender<usize>, mpsc::UnboundedReceiver<usize>),

    // waiting is used by receivers to signal that they are waiting for new entries, and where they
    // are waiting
    waiting: (
        mpsc::UnboundedSender<WaitingReader>,
        mpsc::UnboundedReceiver<WaitingReader>,
    ),

    // cache used to keep track of threads waiting for next write.
    // this is only here to avoid allocating one on every broadcast()
    cache: Vec<WaitingReader>,
}

struct UnboundedReceiverIterator<'a, T: 'a>(&'a mut mpsc::UnboundedReceiver<T>);

impl<'a, T> Iterator for UnboundedReceiverIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match self.0.poll() {
            Ok(Async::Ready(Some(x))) => Some(x),
            Ok(Async::Ready(None)) |
            Ok(Async::NotReady) => None,
            Err(..) => unreachable!(),
        }
    }
}

impl<T> Bus<T> {
    /// Allocates a new `Bus`.
    ///
    /// The provided length should be sufficient to absorb temporary peaks in the data flow, and is
    /// thus workflow-dependent. Bus performance degrades somewhat when the queue is full, so it is
    /// generally better to set this high than low unless you are pressed for memory.
    pub fn new(mut len: usize) -> Bus<T> {
        use std::iter;

        // ring buffer must have room for one padding element
        len += 1;

        let inner = Arc::new(BusInner {
            ring: (0..len).map(|_| Seat::default()).collect(),
            tail: atomic::AtomicUsize::new(0),
            closed: atomic::AtomicBool::new(false),
            len: len,
        });

        Bus {
            state: inner,
            readers: 0,
            rleft: iter::repeat(0).take(len).collect(),
            leaving: mpsc::unbounded(),
            waiting: mpsc::unbounded(),

            cache: Vec::new(),
        }
    }

    /// Get the expected number of reads for the given seat. This number will always be
    /// conservative, in that fewer reads may be fine. Specifically, `.rleft` may not be
    /// sufficiently up-to-date to account for all readers that have left.
    #[inline]
    fn expected(&mut self, at: usize) -> usize {
        // since only the producer will modify the ring, and &mut self guarantees that *we* are the
        // producer, no-one is modifying the ring. Multiple read-only borrows are safe, and so the
        // cast below is safe.
        unsafe { &*self.state.ring[at].state.get() }.max - self.rleft[at]
    }

    /// Attempts to place the given value on the bus.
    ///
    /// If the bus is full, the behavior depends on `block`. If false, the value given is returned
    /// in an `Err()`. Otherwise, the current thread will be parked until there is space in the bus
    /// again, and the broadcast will be tried again until it succeeds.
    ///
    /// Note that broadcasts will succeed even if there are no consumers!
    fn broadcast_inner(&mut self, val: T) -> Result<(), T> {
        let tail = self.state.tail.load(atomic::Ordering::Relaxed);

        // we want to check if the next element over is free to ensure that we always leave one
        // empty space between the head and the tail. This is necessary so that readers can
        // distinguish between an empty and a full list. If the fence seat is free, the seat at
        // tail must also be free, which is simple enough to show by induction (exercise for the
        // reader).
        let fence = (tail + 1) % self.state.len;

        loop {
            // async note: this .poll() must always happen, or the writer won't be woken on a reader leaving

            // no!
            // let's check if any readers have left, which might increment self.rleft[tail].
            for mut left in UnboundedReceiverIterator(&mut self.leaving.1) {
                // a reader has left! this means that every seat between `left` and `tail-1`
                // has max set one too high. we track the number of such "missing" reads that
                // should be ignored in self.rleft, and compensate for them when looking at
                // seat.read above.
                self.readers -= 1;
                while left != tail {
                    self.rleft[left] += 1;
                    left = (left + 1) % self.state.len
                }
            }

            let fence_read = self.state.ring[fence].read.load(atomic::Ordering::Acquire);
            // is the fence block now free?
            if fence_read == self.expected(fence) {
                // yes! go ahead and write!
                break;
            } else {
                // no, so block by parking and telling readers to notify on last read
                self.state.ring[fence]
                    .waiting
                    .replace(Some(Box::new(task::current())), atomic::Ordering::Relaxed);

                // need the atomic fetch_add to ensure reader threads will see the new .waiting
                self.state.ring[fence]
                    .read
                    .fetch_add(0, atomic::Ordering::Release);

                self.poll_waiting(tail);
                // no, and blocking isn't allowed, so return an error
                return Err(val);
            }
        }

        // next one over is free, we have a free seat!
        let readers = self.readers;
        {
            let next = &self.state.ring[tail];
            // we are the only writer, so no-one else can be writing. however, since we're
            // mutating state, we also need for there to be no readers for this to be safe. the
            // argument for why this is the case is roughly an inverse of the argument for why
            // the unsafe block in Seat.take() is safe.  basically, since
            //
            //   .read + .rleft == .max
            //
            // we know all readers at the time of the seat's previous write have accessed this
            // seat. we also know that no other readers will access that seat (they must have
            // started at later seats). thus, we are the only thread accessing this seat, and
            // so we can safely access it as mutable.
            let state = unsafe { &mut *next.state.get() };
            state.max = readers;
            state.val = Some(val);
            next.waiting.replace(None, atomic::Ordering::Relaxed);
            next.read.store(0, atomic::Ordering::Release);
        }
        self.rleft[tail] = 0;
        // now tell readers that they can read
        let tail = (tail + 1) % self.state.len;
        self.state.tail.store(tail, atomic::Ordering::Release);

        self.poll_waiting(tail);
        Ok(())
    }

    fn poll_waiting(&mut self, tail: usize) {
        // unblock any blocked receivers
        for waiting in UnboundedReceiverIterator(&mut self.waiting.1) {
            // the only readers we can't unblock are those that have already absorbed the
            // broadcast we just made, since they are blocking on the *next* broadcast
            if waiting.at == tail {
                self.cache.push(waiting);
            } else {
                waiting.notify();
            }
        }
        for w in self.cache.drain(..) {
            // fine to do here because it is guaranteed not to block
            // async note: self.waiting.1 is still open, so this can't fail
            self.waiting.0.unbounded_send(w).unwrap();
        }
    }

    /// Add a new consumer to this bus.
    ///
    /// The new consumer will receive all *future* broadcasts on this bus.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use bus::Bus;
    /// use std::sync::mpsc::TryRecvError;
    ///
    /// let mut bus = Bus::new(10);
    /// let mut rx1 = bus.add_rx();
    ///
    /// bus.broadcast("Hello");
    ///
    /// // consumer present during broadcast sees update
    /// assert_eq!(rx1.recv(), Ok("Hello"));
    ///
    /// // new consumer does *not* see broadcast
    /// let mut rx2 = bus.add_rx();
    /// assert_eq!(rx2.try_recv(), Err(TryRecvError::Empty));
    ///
    /// // both consumers see new broadcast
    /// bus.broadcast("world");
    /// assert_eq!(rx1.recv(), Ok("world"));
    /// assert_eq!(rx2.recv(), Ok("world"));
    /// ```
    pub fn add_rx(&mut self) -> BusReader<T> {
        self.readers += 1;

        BusReader {
            bus: Arc::clone(&self.state),
            head: self.state.tail.load(atomic::Ordering::Relaxed),
            leaving: self.leaving.0.clone(),
            waiting: self.waiting.0.clone(),
            closed: false,
        }
    }

    fn try_close(&mut self) -> Poll<(), void::Void> {
        let was_closed = self.state.closed.swap(true, atomic::Ordering::Relaxed);
        if !was_closed {
            // Acquire/Release .tail to ensure other threads see new .closed
            self.state.tail.fetch_add(0, atomic::Ordering::AcqRel);

            self.leaving.1.close();
            self.waiting.1.close();
        }

        loop {
            match self.waiting.1.poll() {
                Ok(Async::Ready(Some(t))) => t.notify(),
                Ok(Async::Ready(None)) => break,
                Ok(Async::NotReady) => return Ok(Async::NotReady),
                Err(..) => unreachable!(),
            }
        }

        Ok(Async::Ready(()))
    }
}

impl<T> Drop for Bus<T> {
    fn drop(&mut self) {
        match self.try_close() {
            Ok(Async::Ready(())) => (),
            Ok(Async::NotReady) =>
                warn!("Bus writer {:p} was not closed before drop; some BusReaders might not wake", self),
            Err(..) => unreachable!(),
        }
    }
}

/// A `BusReader` is a single consumer of `Bus` broadcasts. It will see every new value that is
/// passed to `.broadcast()` (or successful calls to `.try_broadcast()`) on the `Bus` that it was
/// created from.
///
/// Dropping a `BusReader` is perfectly safe, and will unblock the writer if it was waiting for
/// that read to see a particular update.
///
/// ```rust
/// use bus::Bus;
/// let mut tx = Bus::new(1);
/// let mut r1 = tx.add_rx();
/// let r2 = tx.add_rx();
/// assert_eq!(tx.try_broadcast(true), Ok(()));
/// assert_eq!(r1.recv(), Ok(true));
///
/// // the bus does not have room for another broadcast
/// // since it knows r2 has not yet read the first broadcast
/// assert_eq!(tx.try_broadcast(true), Err(true));
///
/// // dropping r2 tells the producer that there is a free slot
/// // (i.e., it has been read by everyone)
/// drop(r2);
/// assert_eq!(tx.try_broadcast(true), Ok(()));
/// ```
pub struct BusReader<T> {
    bus: Arc<BusInner<T>>,
    head: usize,
    leaving: mpsc::UnboundedSender<usize>,
    waiting: mpsc::UnboundedSender<WaitingReader>,
    closed: bool,
}

impl<T: Clone + Sync> BusReader<T> {
    /// Attempts to read a broadcast from the bus.
    ///
    /// If the bus is empty, the behavior depends on `block`. If false,
    /// `Err(mpsc::RecvTimeoutError::Timeout)` is returned. Otherwise, the current thread will be
    /// parked until there is another broadcast on the bus, at which point the receive will be
    /// performed.
    fn recv_inner(&mut self) -> Poll<Option<T>, void::Void> {
        if self.closed {
            return Ok(Async::Ready(None));
        }

        let mut was_closed = false;
        loop {
            let tail = self.bus.tail.load(atomic::Ordering::Acquire);
            if tail != self.head {
                break;
            }

            // buffer is empty, check whether it's closed.
            // relaxed is fine since Bus.drop does an acquire/release on tail
            if self.bus.closed.load(atomic::Ordering::Relaxed) {
                // we need to check again that there's nothing in the bus, otherwise we might have
                // missed a write between when we did the read of .tail above and when we read
                // .closed here
                if !was_closed {
                    was_closed = true;
                    continue;
                }

                // the bus is closed, and we didn't miss anything!
                self.closed = true;
                return Ok(Async::Ready(None));
            }

            // async note: WaitingReader's Drop guarantees that if the writer has gone away, or is about to go away, this task will be notified again to drain itself.
            let _ = self.waiting.unbounded_send(WaitingReader::current(self.head));
            return Ok(Async::NotReady);
        }

        let head = self.head;
        let ret = self.bus.ring[head].take();

        // safe because len is read-only
        self.head = (head + 1) % self.bus.len;
        Ok(Async::Ready(Some(ret)))
    }
}

impl<T> Drop for BusReader<T> {
    fn drop(&mut self) {
        // we allow not checking the result here because the writer might have gone away, which
        // would result in an error, but is okay nonetheless.
        let _ = self.leaving.unbounded_send(self.head);
    }
}

impl<T: Clone + Sync> Sink for Bus<T> {
    type SinkItem = T;
    type SinkError = void::Void;

    fn start_send(&mut self, item: Self::SinkItem) -> StartSend<Self::SinkItem, Self::SinkError> {
        match self.broadcast_inner(item) {
            Ok(()) => Ok(AsyncSink::Ready),
            Err(item) => Ok(AsyncSink::NotReady(item)),
        }
    }

    fn poll_complete(&mut self) -> Poll<(), Self::SinkError> {
        Ok(Async::Ready(()))
    }

    fn close(&mut self) -> Poll<(), Self::SinkError> {
        self.try_close()
    }
}

impl<T: Clone + Sync> Stream for BusReader<T> {
    type Item = T;
    type Error = void::Void;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        self.recv_inner()
    }
}

#[cfg(feature = "bench")]
#[bench]
fn bench_bus_one_to_one(b: &mut test::Bencher) {
    let mut c = Bus::new(100);
    let mut rx = c.add_rx();
    let j = thread::spawn(move || loop {
        match rx.recv() {
            Ok(exit) if exit => break,
            Err(..) => break,
            _ => (),
        }
    });
    b.iter(|| c.broadcast(false));
    c.broadcast(true);
    j.join().unwrap();
}

#[cfg(feature = "bench")]
#[bench]
fn bench_syncch_one_to_one(b: &mut test::Bencher) {
    let (tx, rx) = mpsc::sync_channel(100);
    let j = thread::spawn(move || loop {
        match rx.recv() {
            Ok(exit) if exit => break,
            Err(..) => break,
            _ => (),
        }
    });
    b.iter(|| tx.send(false).unwrap());
    tx.send(true).unwrap();
    j.join().unwrap();
}
