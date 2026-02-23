//! CPU profiling support (requires `profile` feature).
//!
//! Use `pascal run --profile` to generate a flamegraph.

#[cfg(feature = "profile")]
use anyhow::Result;
#[cfg(feature = "profile")]
use std::fs::File;
#[cfg(feature = "profile")]
use std::path::Path;

/// Run a closure under CPU profiling and write a flamegraph to the given path.
///
/// Build with `cargo build --features profile` to enable.
#[cfg(feature = "profile")]
pub fn run_profiled<F, T>(output_path: &Path, f: F) -> Result<T>
where
    F: FnOnce() -> T,
{
    let guard = pprof::ProfilerGuardBuilder::default()
        .frequency(1000)
        .blocklist(&["libc", "libgcc", "pthread", "vdso"])
        .build()
        .map_err(|e| anyhow::anyhow!("Profiler init failed: {}", e))?;

    let result = f();

    if let Ok(report) = guard.report().build() {
        let mut file = File::create(output_path)?;
        report.flamegraph(&mut file)?;
    }

    Ok(result)
}
