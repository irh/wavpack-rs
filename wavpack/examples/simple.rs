use anyhow::Result;
use wavpack::*;

fn main() -> Result<()> {
    let version = get_library_version_string();
    println!("WavPack version = {version}");
    let mut context = WavpackReader::builder("a.wv").tags().build()?;
    let mode = context.get_mode()?;
    dbg!(mode);
    let tags = context.get_all_tag_items()?;
    dbg!(&tags);
    if let TagData::Text(x) = &tags["Cuesheet"] {
        println!("{x}");
    }
    let context = WavpackReader::builder("not_a_file.wv").build();
    assert_eq!(context.err().unwrap().to_string(), "can't open file");
    Ok(())
}
