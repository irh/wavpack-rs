use std::fs::remove_file;
use std::fs::File;
use std::path::PathBuf;
use wavpack::*;

fn make_wave(samples: usize, frequency: f64) -> Vec<i32> {
    let mut v = vec![0; samples];
    for (i, v) in v.iter_mut().enumerate() {
        let theta = i as f64 * std::f64::consts::TAU / 44100. * frequency;
        *v = (theta.sin() * 32767.) as i32;
    }
    v
}

fn seq() -> Vec<i32> {
    vec![
        make_wave(44100, 277.183),
        make_wave(44100, 293.665),
        make_wave(44100, 329.628),
        make_wave(44100, 349.228),
        make_wave(44100, 391.995),
        make_wave(44100, 440.),
        make_wave(44100, 493.883),
        make_wave(44100, 523.251),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
}

fn drop<T>(_: T) {}

#[test]
fn write_read() {
    // write
    let file = File::create("test.wv").unwrap();
    let mut wc = WriteBuilder::new(file)
        .add_bytes_per_sample(2)
        .add_bits_per_sample(16)
        .add_channel_mask(4)
        .add_num_channels(1)
        .add_sample_rate(44100)
        .build(44100 * 8)
        .unwrap();
    let mut data1 = seq();
    wc.pack_samples(&mut data1).unwrap();
    drop(wc);
    // read
    let path = PathBuf::from("test.wv");
    let mut context = ContextBuilder::new(&path).build().unwrap();
    let data2 = context.unpack(0, 75 * 8).unwrap();
    // test
    assert_eq!(data1.len(), data2.len());
    for (v1, v2) in data1.iter().zip(data2.iter()) {
        assert_eq!(v1, v2);
    }
    // clean
    remove_file("test.wv").unwrap();
}
