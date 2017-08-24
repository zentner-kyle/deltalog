use rand::Rng;
use std::collections::VecDeque;

#[allow(dead_code)]
pub struct FakeRng {
    u32_buffer: VecDeque<u32>,
    u64_buffer: VecDeque<u64>,
    f32_buffer: VecDeque<f32>,
    f64_buffer: VecDeque<f64>,
    byte_buffer: VecDeque<Vec<u8>>,
}

impl FakeRng {
    #[allow(dead_code)]
    pub fn new() -> Self {
        FakeRng {
            u32_buffer: VecDeque::new(),
            u64_buffer: VecDeque::new(),
            f32_buffer: VecDeque::new(),
            f64_buffer: VecDeque::new(),
            byte_buffer: VecDeque::new(),
        }
    }

    #[allow(dead_code)]
    pub fn push_u32(&mut self, v: u32) {
        self.u32_buffer.push_back(v);
    }

    #[allow(dead_code)]
    pub fn push_u64(&mut self, v: u64) {
        self.u64_buffer.push_back(v);
    }

    #[allow(dead_code)]
    pub fn push_f32(&mut self, v: f32) {
        self.f32_buffer.push_back(v);
    }

    #[allow(dead_code)]
    pub fn push_f64(&mut self, v: f64) {
        self.f64_buffer.push_back(v);
    }

    #[allow(dead_code)]
    pub fn push_bytes(&mut self, v: Vec<u8>) {
        self.byte_buffer.push_back(v);
    }
}

impl Rng for FakeRng {
    fn next_u32(&mut self) -> u32 {
        self.u32_buffer
            .pop_front()
            .expect("FakeRng needs more u32")
    }

    fn next_u64(&mut self) -> u64 {
        self.u64_buffer
            .pop_front()
            .expect("FakeRng needs more u64")
    }

    fn next_f32(&mut self) -> f32 {
        self.f32_buffer
            .pop_front()
            .expect("FakeRng needs more f32")
    }

    fn next_f64(&mut self) -> f64 {
        self.f64_buffer
            .pop_front()
            .expect("FakeRng needs more f32")
    }

    fn fill_bytes(&mut self, dest: &mut [u8]) {
        let source = self.byte_buffer
            .pop_front()
            .expect("FakeRng needs more bytes");
        if source.len() != dest.len() {
            panic!(format!("FakeRng has byte buffer len {}, \
                            but was asked to fill buffer of len {}.",
                           source.len(),
                           dest.len()));
        }
        for (tgt, src) in dest.iter_mut().zip(source.into_iter()) {
            *tgt = src;
        }
    }
}
