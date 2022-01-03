use byteorder::{BigEndian, ReadBytesExt};
use rust;
use rust::util;
use rust::vm;
use std::{fs::File, io::BufReader};

fn main() {
    println!("Hello, world!");
    let mut vm = vm::vm::new();
    let f = File::open("../2048.obj").expect("couldn't open or find file");
    let mut f = BufReader::new(f);
    let origin = f.read_u16::<BigEndian>().expect("error");
    let instructions = util::read_u16(f).unwrap();
    vm.from_file(instructions, origin);
    loop {
        vm.run();
    }
}
