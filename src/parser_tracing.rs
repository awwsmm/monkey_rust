use std::sync::RwLock;

static TRACE_LEVEL: RwLock<usize> = RwLock::new(0);

fn ident_level() -> String {
    let trace_level = TRACE_LEVEL.read().unwrap();
    "\t".repeat(*trace_level - 1)
}

fn trace_print(fs: String) {
    print!("{}{}\n", ident_level(), fs)
}

fn inc_ident() {
    let mut trace_level = TRACE_LEVEL.write().unwrap();
    *trace_level += 1;
}

fn dec_ident() {
    let mut trace_level = TRACE_LEVEL.write().unwrap();
    *trace_level -= 1;
}

pub(crate) fn trace(msg: &str) -> &str {
    inc_ident();
    trace_print(format!("BEGIN {}", msg));
    msg
}

pub(crate) fn untrace(msg: &str) {
    trace_print(format!("END {}", msg));
    dec_ident()
}