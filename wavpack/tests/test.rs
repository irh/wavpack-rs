use std::{
    cell::RefCell,
    io::{self, Cursor, Seek},
    rc::Rc,
};
use wavpack::*;

fn make_wave(frames: usize, channels: usize, frequency: f64) -> Vec<i32> {
    use std::f64::consts::TAU;
    use std::iter::repeat;

    (0..frames)
        .flat_map(|i| {
            let theta = i as f64 * TAU / 44100.0 * frequency;
            let sample = (theta.sin() * 32767.) as i32;
            repeat(sample).take(channels)
        })
        .collect()
}

fn seq(channels: usize) -> Vec<i32> {
    [make_wave(44100, channels, 220.0)]
        .into_iter()
        .flatten()
        .collect()
}

fn run_write_read_test(channels: usize, channel_mask: i32) {
    let mut input = seq(channels);

    let buffer = TestBuffer::default();

    // write
    {
        let mut writer = WavpackWriter::with_writer(buffer.clone())
            .add_bytes_per_sample(2)
            .add_bits_per_sample(16)
            .add_num_channels(channels as i32)
            .add_channel_mask(channel_mask)
            .add_sample_rate(44100)
            .build()
            .unwrap();

        writer.pack_samples(&mut input).unwrap();
    }

    // read
    {
        buffer.rewind();

        let mut reader = WavpackReader::with_reader(buffer.clone()).build().unwrap();
        let mut read_buffer = vec![0; reader.get_num_samples64().unwrap() as usize * channels];
        let unpacked_frame_count = reader.unpack_samples(&mut read_buffer).unwrap();

        // test
        assert_eq!(input.len(), unpacked_frame_count as usize * channels);
        for (v1, v2) in input.iter().zip(read_buffer.iter()) {
            assert_eq!(v1, v2);
        }
    }
}

#[test]
fn write_read_mono() {
    run_write_read_test(1, 4);
}

#[test]
fn write_read_stereo() {
    run_write_read_test(2, 3);
}

#[test]
fn write_read_4c() {
    run_write_read_test(4, 15);
}

#[derive(Clone, Default)]
struct TestBuffer {
    cursor: Rc<RefCell<Cursor<Vec<u8>>>>,
}

impl TestBuffer {
    fn rewind(&self) {
        self.cursor.borrow_mut().rewind().unwrap();
    }
}

impl io::Read for TestBuffer {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.cursor.borrow_mut().read(buf)
    }
}

impl io::Write for TestBuffer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.cursor.borrow_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.cursor.borrow_mut().flush()
    }
}

impl io::Seek for TestBuffer {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.cursor.borrow_mut().seek(pos)
    }
}

impl WavpackRead for TestBuffer {
    fn stream_length(&mut self) -> Option<u64> {
        Some(self.cursor.borrow().get_ref().len() as u64)
    }
}

impl WavpackWrite for TestBuffer {}
