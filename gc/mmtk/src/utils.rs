use std::sync::atomic::{AtomicUsize, Ordering};

use atomic_refcell::AtomicRefCell;
use mmtk::scheduler::{GCWork, GCWorker, WorkBucketStage};

use sysinfo::System;
use crate::Ruby;

pub struct ChunkedVecCollector<T> {
    vecs: Vec<Vec<T>>,
    current_vec: Vec<T>,
    chunk_size: usize,
}

impl<T> ChunkedVecCollector<T> {
    pub fn new(chunk_size: usize) -> Self {
        Self {
            vecs: vec![],
            current_vec: Vec::with_capacity(chunk_size),
            chunk_size,
        }
    }

    pub fn add(&mut self, item: T) {
        self.current_vec.push(item);
        if self.current_vec.len() == self.chunk_size {
            self.flush();
        }
    }

    fn flush(&mut self) {
        let new_vec = Vec::with_capacity(self.chunk_size);
        let old_vec = std::mem::replace(&mut self.current_vec, new_vec);
        self.vecs.push(old_vec);
    }

    pub fn into_vecs(mut self) -> Vec<Vec<T>> {
        if !self.current_vec.is_empty() {
            self.flush();
        }
        self.vecs
    }
}

impl<A> Extend<A> for ChunkedVecCollector<A> {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        for item in iter {
            self.add(item);
        }
    }
}

pub struct AfterAll {
    counter: AtomicUsize,
    stage: WorkBucketStage,
    packets: AtomicRefCell<Vec<Box<dyn GCWork<Ruby>>>>,
}

unsafe impl Sync for AfterAll {}

impl AfterAll {
    pub fn new(stage: WorkBucketStage) -> Self {
        Self {
            counter: AtomicUsize::new(0),
            stage,
            packets: AtomicRefCell::new(vec![]),
        }
    }

    pub fn add_packets(&self, mut packets: Vec<Box<dyn GCWork<Ruby>>>) {
        let mut borrow = self.packets.borrow_mut();
        borrow.append(&mut packets);
    }

    pub fn count_up(&self, n: usize) {
        self.counter.fetch_add(n, Ordering::SeqCst);
    }

    pub fn count_down(&self, worker: &mut GCWorker<Ruby>) {
        let old = self.counter.fetch_sub(1, Ordering::SeqCst);
        if old == 1 {
            let packets = {
                let mut borrow = self.packets.borrow_mut();
                std::mem::take(borrow.as_mut())
            };
            worker.scheduler().work_buckets[self.stage].bulk_add(packets);
        }
    }
}

pub fn default_heap_max() -> usize {
    let mut s = System::new();
    s.refresh_memory();
    s.total_memory()
        .checked_mul(80)
        .and_then(|v| v.checked_div(100))
        .expect("Invalid Memory size") as usize
}

pub fn parse_capacity(input: String) -> usize {
    let trimmed = input.trim();

    const KIBIBYTE: usize = 1024;
    const MEBIBYTE: usize = 1024 * KIBIBYTE;
    const GIBIBYTE: usize = 1024 * MEBIBYTE;

    let (val, suffix) = if let Some(pos) = trimmed.find(|c: char| !c.is_numeric()) {
        (&trimmed[..pos], &trimmed[pos..])
    } else {
        (trimmed, "")
    };

    // 1MiB is the default heap size
    match (val, suffix) {
        (number, "GiB") => number.parse::<usize>().unwrap() * GIBIBYTE,
        (number, "MiB") => number.parse::<usize>().unwrap() * MEBIBYTE,
        (number, "KiB") => number.parse::<usize>().unwrap() * KIBIBYTE,
        (number, suffix) if suffix.is_empty() => number.parse::<usize>().unwrap(),
        (_, _) => default_heap_max()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_capacity_parses_bare_bytes() {
        assert_eq!(1234, parse_capacity(String::from("1234")));
    }

    #[test]
    fn test_parse_capacity_parses_kibibytes() {
        assert_eq!(10240, parse_capacity(String::from("10KiB")))
    }

    #[test]
    fn test_parse_capacity_parses_mebibytes() {
        assert_eq!(10485760, parse_capacity(String::from("10MiB")))
    }

    #[test]
    fn test_parse_capacity_parses_gibibytes() {
        assert_eq!(10737418240, parse_capacity(String::from("10GiB")))
    }

    #[test]
    fn test_parses_nonsense_value_as_default_max() {
        let mut s = System::new();
        s.refresh_memory();
        let eighty_percent_mem = (s.total_memory() / 100 * 80) as usize;

        assert_eq!(eighty_percent_mem, parse_capacity(String::from("notanumber")));
        assert_eq!(eighty_percent_mem, parse_capacity(String::from("5tartswithanumber")));
        assert_eq!(eighty_percent_mem, parse_capacity(String::from("number1nthemiddle")));
        assert_eq!(eighty_percent_mem, parse_capacity(String::from("numberattheend111")));
        assert_eq!(eighty_percent_mem, parse_capacity(String::from("mult1pl3numb3r5")));
    }
}
