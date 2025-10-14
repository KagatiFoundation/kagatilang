#[macro_export]
macro_rules! bug {
    ($($arg:tt)*) => ({
        eprintln!("KAGC internal error: {}! Aborting...", format!($($arg)*));
        std::process::exit(1);
    });
}