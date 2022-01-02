extern crate rust;

fn main() {
    println!("Hello, world!");
    let mut vm = rust::vm::vm::new();
    loop {
        let raw_inst = 0;
        vm.handle_inst(rust::inst::parse(raw_inst));
    }
}
