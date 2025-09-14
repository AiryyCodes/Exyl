use std::ffi::{CStr, c_char};

#[unsafe(no_mangle)]
pub extern "C" fn print_string(s: *const c_char) {
    if s.is_null() {
        println!("(null)");
        return;
    }

    // Convert C string to Rust &str
    unsafe {
        let c_str = CStr::from_ptr(s);
        if let Ok(str_slice) = c_str.to_str() {
            println!("{}", str_slice);
        } else {
            println!("Invalid UTF-8 string");
        }
    }
}
