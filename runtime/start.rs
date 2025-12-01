use std::env;

#[link(name = "our_code")]
extern "C" {
    #[link_name="\x01our_code_starts_here"]
    fn our_code_starts_here(env: *mut i64) -> i64;
}

#[no_mangle]
pub extern "C" fn snek_print(i: i64) -> i64 {
    match i & 7 {
        0b111 => {
            match (i >> 3) & 7 {
                0 => eprintln!("Invalid arguments to one or more functions."),
                1 => eprintln!("Integer overflow."),
                2 => eprintln!("Bad cast."),
                _ => eprintln!("Unknown error")
            }
        },
        0b011 => println!("true"),
        0b001 => println!("false"),
        _ if i & 1 == 0 => {
            println!("{}", i >> 1)
        },
        _ => eprintln!("Unknown error")
    }
    return i;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut fn_args: i64 = 0;
    let mut arg_ptr: *mut i64 = &mut fn_args;
    if args.len() > 1 {
        fn_args = match &*args[1] {
            "true" => 3,
            "false" => 1,
            e => {
                let name = e.parse::<i64>();
                match e.parse::<i64>() {
                    Ok(i) => i << 1,
                    Err(_) => 0
                }
            }
        }
    }
    let i : i64 = unsafe {
        our_code_starts_here(arg_ptr)
    };
    let _ = snek_print(i);
}
