#[unsafe(no_mangle)]
pub extern "C" fn print_int(n: i64) {
    println!("{}", n);
}
