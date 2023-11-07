use std::fs::remove_file;
use std::fs::File;
use std::path::PathBuf;
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
    [
        make_wave(44100, channels, 277.183),
        make_wave(44100, channels, 293.665),
        make_wave(44100, channels, 329.628),
        make_wave(44100, channels, 349.228),
        make_wave(44100, channels, 391.995),
        make_wave(44100, channels, 440.),
        make_wave(44100, channels, 493.883),
        make_wave(44100, channels, 523.251),
    ]
    .into_iter()
    .flatten()
    .collect()
}

fn run_write_read_test(channels: usize, channel_mask: i32, file_name: &str) {
    let mut input = seq(channels);

    // write
    {
        let file = File::create(file_name).unwrap();
        let mut wc = WriteBuilder::new(file)
            .add_bytes_per_sample(2)
            .add_bits_per_sample(16)
            .add_num_channels(channels as i32)
            .add_channel_mask(channel_mask)
            .add_sample_rate(44100)
            .build(-1)
            .unwrap();

        wc.pack_samples(&mut input).unwrap();
    }

    // read
    let path = PathBuf::from(file_name);
    let mut context = ContextBuilder::new(&path).build().unwrap();
    let unpacked = context.unpack(0, 75 * 8).unwrap();

    // test
    assert_eq!(input.len(), unpacked.len());
    for (v1, v2) in input.iter().zip(unpacked.iter()) {
        assert_eq!(v1, v2);
    }

    // clean
    remove_file(file_name).unwrap();
}

#[test]
fn write_read_mono() {
    run_write_read_test(1, 4, "mono.wv");
}

#[test]
fn write_read_stereo() {
    run_write_read_test(2, 3, "stereo.wv");
}

#[test]
fn write_read_4c() {
    run_write_read_test(4, 15, "4c.wv");
}
