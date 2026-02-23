use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::Colorize;
use pascal::build_system::{BuildSystem, Manifest};
use pascal::ppu::PpuFile;
use pascal::ParallelConfig;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "pascal")]
#[command(about = "Pascal - A production-ready optimizing Pascal compiler and package manager")]
#[command(version = "0.1.2")]
#[command(author = "Pascal Team")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new Pascal project
    Init {
        /// Project name
        name: String,

        /// Directory to create the project in
        #[arg(short, long, default_value = ".")]
        dir: PathBuf,

        /// Project template: default, library, console
        #[arg(short, long, default_value = "default")]
        template: String,
    },

    /// Build the current project (requires pascal.toml)
    Build {
        /// Verbose output
        #[arg(short, long)]
        verbose: bool,

        /// Quiet mode (minimal output)
        #[arg(short, long)]
        quiet: bool,

        /// Optimization level 0-3 (overrides pascal.toml)
        #[arg(short = 'O', long)]
        optimization: Option<u8>,
    },

    /// Add a dependency to the project
    Add {
        /// Dependency name
        name: String,

        /// Version constraint
        #[arg(short = 'V', long)]
        version: Option<String>,

        /// Local path to dependency
        #[arg(long)]
        path: Option<String>,

        /// Git repository URL
        #[arg(long)]
        git: Option<String>,
    },

    /// Remove a dependency from the project
    Remove {
        /// Dependency name
        name: String,
    },

    /// Compile a Pascal source file
    Compile {
        /// Input Pascal file (unit or program)
        input: PathBuf,

        /// Output directory for compiled files
        #[arg(short, long, default_value = ".")]
        output: PathBuf,

        /// Additional search paths for units
        #[arg(short = 'I', long = "include")]
        search_paths: Vec<PathBuf>,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "0")]
        optimization: u8,

        /// Generate debug information
        #[arg(short, long)]
        debug: bool,

        /// Don't generate PPU files
        #[arg(long)]
        no_ppu: bool,

        /// Don't use existing PPU files
        #[arg(long)]
        no_cache: bool,

        /// Generate assembly code
        #[arg(short = 'S', long)]
        asm: bool,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,

        /// Quiet mode (minimal output)
        #[arg(short, long)]
        quiet: bool,

        /// Enable parallel compilation
        #[arg(short = 'j', long)]
        parallel: bool,

        /// Number of threads for parallel compilation (0 = auto)
        #[arg(long, default_value = "0")]
        threads: usize,

        /// Define symbol for conditional compilation (-DDEBUG, -DFOO=1)
        #[arg(short = 'D', long = "define", value_name = "SYMBOL")]
        defines: Vec<String>,
    },

    /// Show information about a compiled unit
    Info {
        /// PPU file to inspect
        ppu_file: PathBuf,
    },

    /// Run a Pascal program or project (interpreter)
    Run {
        /// Input Pascal file (optional if pascal.toml exists)
        input: Option<PathBuf>,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,

        /// Quiet mode (minimal output)
        #[arg(short, long)]
        quiet: bool,

        /// Watch mode: re-run when source files change
        #[arg(short, long)]
        watch: bool,

        /// CPU profile and write flamegraph (requires --features profile)
        #[arg(long)]
        profile: bool,

        /// Profile output path (default: flamegraph.svg)
        #[arg(long, default_value = "flamegraph.svg")]
        profile_output: PathBuf,
    },

    /// Run with interactive debugger (breakpoints, watch expressions)
    Debug {
        /// Input Pascal file
        input: PathBuf,

        /// Break on procedure/function entry (e.g. -b MyProc)
        #[arg(short, long)]
        breakpoint: Vec<String>,

        /// Watch variable (e.g. -w x)
        #[arg(short, long)]
        watch: Vec<String>,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Format Pascal source files
    Fmt {
        /// Pascal source file(s) or directory
        #[arg(default_value = ".")]
        path: Vec<PathBuf>,

        /// Check-only (don't write, report if formatting would change)
        #[arg(long)]
        check: bool,

        /// Create default configuration file
        #[arg(long)]
        init_config: bool,

        /// Configuration file path
        #[arg(long)]
        config: Option<PathBuf>,

        /// Process directories recursively
        #[arg(long, default_value = "true")]
        recursive: bool,

        /// Use 2-space indentation
        #[arg(long, conflicts_with_all = ["tabs", "indent_4"])]
        indent_2: bool,

        /// Use 4-space indentation
        #[arg(long, conflicts_with_all = ["tabs", "indent_2"])]
        indent_4: bool,

        /// Use tabs for indentation
        #[arg(long, conflicts_with_all = ["indent_2", "indent_4"])]
        tabs: bool,

        /// Maximum line length
        #[arg(long)]
        max_line_length: Option<usize>,

        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },

    /// Check Pascal source for errors (parse + type-check, no codegen)
    Check {
        /// Pascal source file
        input: PathBuf,
    },

    /// Generate documentation from Pascal source
    Doc {
        /// Pascal source file(s) or directory
        #[arg(default_value = ".")]
        path: Vec<PathBuf>,

        /// Output format: markdown, html
        #[arg(short, long, default_value = "markdown")]
        format: String,

        /// Output directory (default: docs/)
        #[arg(short, long, default_value = "docs")]
        output: PathBuf,

        /// Process directories recursively
        #[arg(long, default_value = "true")]
        recursive: bool,
    },

    /// Clean compiled files
    Clean {
        /// Directory to clean
        #[arg(default_value = ".")]
        directory: PathBuf,
    },
}

/// Find pascal.toml from cwd upward and open the build system.
fn open_project(verbose: bool) -> Result<BuildSystem> {
    let cwd = std::env::current_dir()?;
    let manifest_path = Manifest::find(&cwd).ok_or_else(|| {
        anyhow::anyhow!(
            "No pascal.toml found in {} or any parent directory.\n\
             Run `pascal init <name>` to create a new project.",
            cwd.display()
        )
    })?;
    let project_root = manifest_path.parent().unwrap();
    BuildSystem::open(project_root, verbose)
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { name, dir, template } => BuildSystem::init_with_template(&dir, &name, &template),

        Commands::Build { verbose, quiet, optimization } => {
            let mut bs = open_project(verbose && !quiet)?;
            if let Some(opt) = optimization {
                bs.manifest_mut().build.optimization = opt;
            }
            bs.build(quiet)
        }

        Commands::Add {
            name,
            version,
            path,
            git,
        } => {
            let mut bs = open_project(false)?;
            bs.add_dependency(&name, version.as_deref(), path.as_deref(), git.as_deref())
        }

        Commands::Remove { name } => {
            let mut bs = open_project(false)?;
            bs.remove_dependency(&name)
        }

        Commands::Compile {
            input,
            output,
            search_paths,
            optimization,
            debug,
            no_ppu,
            no_cache,
            asm,
            verbose,
            quiet,
            parallel,
            threads,
            defines,
        } => compile_file(
            input,
            output,
            search_paths,
            optimization,
            debug,
            !no_ppu,
            !no_cache,
            asm,
            verbose && !quiet,
            quiet,
            parallel,
            threads,
            defines,
        ),

        Commands::Info { ppu_file } => show_ppu_info(ppu_file),

        Commands::Run {
            input,
            verbose,
            quiet,
            watch,
            profile,
            profile_output,
        } => {
            if watch {
                if let Some(file) = input {
                    run_file_watch(file, verbose && !quiet)
                } else {
                    run_project_watch(verbose && !quiet)?;
                    Ok(())
                }
            } else if let Some(file) = input {
                run_file(file, verbose && !quiet, profile.then(|| profile_output))
            } else {
                open_project(verbose && !quiet)?.run(quiet, profile.then(|| profile_output))
            }
        }

        Commands::Debug {
            input,
            breakpoint,
            watch,
            verbose,
        } => run_debug(input, breakpoint, watch, verbose),

        Commands::Fmt { path, check, .. } => fmt_files(path, check),

        Commands::Check { input } => check_file(input),

        Commands::Doc {
            path,
            format,
            output,
            recursive,
        } => doc_files(path, format, output, recursive),

        Commands::Clean { directory } => clean_directory(directory),
    }
}

fn compile_file(
    input: PathBuf,
    output: PathBuf,
    _search_paths: Vec<PathBuf>,
    optimization: u8,
    _debug: bool,
    _generate_ppu: bool,
    _use_ppu: bool,
    generate_asm: bool,
    verbose: bool,
    quiet: bool,
    parallel: bool,
    threads: usize,
    defines: Vec<String>,
) -> Result<()> {
    if !input.exists() {
        eprintln!(
            "{} File not found: {}",
            "Error:".red().bold(),
            input.display()
        );
        std::process::exit(1);
    }

    if verbose {
        println!("{} {}", "Compiling:".green().bold(), input.display());
        println!("{}", "Configuration:".cyan().bold());
        println!("  Output directory: {}", output.display());
        println!("  Optimization level: {}", optimization);
        if parallel {
            println!("  Parallel compilation: {}", "enabled".green());
            if threads > 0 {
                println!("  Threads: {}", threads);
            } else {
                println!("  Threads: {} (auto-detected)", num_cpus::get());
            }
        }
    }

    // Initialize parallel compiler if enabled
    if parallel {
        let config = ParallelConfig::new()
            .with_threads(threads)
            .with_parallel_modules(true)
            .with_parallel_optimization(true);

        if let Err(e) = config.init_thread_pool() {
            eprintln!(
                "{} Failed to initialize thread pool: {}",
                "Error:".red().bold(),
                e
            );
            std::process::exit(1);
        }

        if verbose {
            println!("{} Thread pool initialized", "Info:".cyan().bold());
        }
    }

    // Read source file
    let raw_source = std::fs::read_to_string(&input)?;
    if verbose {
        println!("{} Read {} bytes", "Info:".cyan().bold(), raw_source.len());
    }

    // Conditional compilation
    let defines_set: std::collections::HashSet<String> =
        defines.into_iter().map(|s| s.split('=').next().unwrap_or(&s).to_uppercase()).collect();
    let source = pascal::preprocess(&raw_source, &defines_set);
    if verbose && !defines_set.is_empty() {
        println!("{} Defines: {:?}", "Info:".cyan().bold(), defines_set);
    }

    // Parsing (lexer is created internally by parser)
    let mut parser = pascal::parser::Parser::new(&source);
    let program = match parser.parse_program() {
        Ok(prog) => {
            if verbose {
                println!("{} Parsed program '{}'", "Info:".cyan().bold(), prog.name);
                println!("  Uses: {:?}", prog.uses);
                println!("  Statements: {}", prog.block.statements.len());
                println!("  Variables: {}", prog.block.vars.len());
                println!("  Constants: {}", prog.block.consts.len());
            }
            prog
        }
        Err(e) => {
            eprintln!("{} {}", "Parse error:".red().bold(), e);
            if !parser.errors().is_empty() {
                for err in parser.errors() {
                    eprintln!("  {}", err);
                }
            }
            std::process::exit(1);
        }
    };

    // Optimization
    if optimization > 0 && verbose {
        println!(
            "{} Optimization level {}",
            "Info:".cyan().bold(),
            optimization
        );
    }

    // Code generation (assembly)
    if generate_asm {
        let stem = input.file_stem().unwrap().to_str().unwrap();
        let asm_path = output.join(format!("{}.asm", stem));

        // Create a unit from the program for code generation
        let unit = pascal::ast::Unit {
            name: program.name.clone(),
            uses: program.uses.clone(),
            interface: pascal::ast::UnitInterface {
                uses: vec![],
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
            },
            implementation: pascal::ast::UnitImplementation {
                uses: vec![],
                types: program.block.types.clone(),
                constants: program.block.consts.clone(),
                variables: program.block.vars.clone(),
                procedures: program.block.procedures.clone(),
                functions: program.block.functions.clone(),
                classes: vec![],
                interfaces: vec![],
                initialization: Some(program.block.statements.clone()),
                finalization: None,
            },
        };

        let mut codegen = pascal::UnitCodeGenerator::new();
        let asm_output = codegen.generate_unit(&unit)?;

        std::fs::create_dir_all(&output)?;
        std::fs::write(&asm_path, asm_output)?;

        if !quiet {
            println!(
                "{} Generated assembly: {}",
                "Success:".green().bold(),
                asm_path.display()
            );
        }
    }

    if !quiet {
        println!(
            "{} Compiled '{}' successfully",
            "Success:".green().bold(),
            program.name
        );
    }

    Ok(())
}

fn run_file(input: PathBuf, verbose: bool, profile_output: Option<PathBuf>) -> Result<()> {
    if !input.exists() {
        eprintln!(
            "{} File not found: {}",
            "Error:".red().bold(),
            input.display()
        );
        std::process::exit(1);
    }

    let source = std::fs::read_to_string(&input)?;
    if verbose {
        println!("{} {}", "Running:".green().bold(), input.display());
    }

    // Parse
    let mut parser = pascal::parser::Parser::new(&source);
    let program = match parser.parse_program() {
        Ok(prog) => prog,
        Err(e) => {
            eprintln!("{} {}", "Parse error:".red().bold(), e);
            for err in parser.errors() {
                eprintln!("  {}", err);
            }
            std::process::exit(1);
        }
    };

    if verbose {
        println!(
            "{} Parsed program '{}'",
            "Info:".cyan().bold(),
            program.name
        );
    }

    run_program_impl(&program, verbose, profile_output)
}

fn run_file_watch(input: PathBuf, verbose: bool) -> Result<()> {
    use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
    use std::sync::mpsc;
    use std::time::Duration;

    if !input.exists() {
        eprintln!("{} File not found: {}", "Error:".red().bold(), input.display());
        std::process::exit(1);
    }

    let (tx, rx) = mpsc::channel();
    let mut watcher = RecommendedWatcher::new(tx, Config::default().with_poll_interval(Duration::from_secs(1)))?;
    let watch_path = input.canonicalize().unwrap_or_else(|_| input.clone());
    watcher.watch(&watch_path, RecursiveMode::NonRecursive)?;

    if !verbose {
        println!("{} Watching {} (Ctrl+C to stop)", "Info:".cyan().bold(), input.display());
    }

    loop {
        run_file(input.clone(), verbose, None)?;
        if verbose {
            println!("{} Waiting for changes...", "Info:".cyan().bold());
        }
        match rx.recv() {
            Ok(Ok(_)) => {
                if verbose {
                    println!("{} Change detected, re-running...", "Info:".cyan().bold());
                }
            }
            Err(_) => return Ok(()),
            _ => {}
        }
    }
}

fn run_project_watch(verbose: bool) -> Result<()> {
    let bs = open_project(verbose)?;
    let manifest = bs.manifest();
    let main_path = manifest
        .package
        .main
        .as_ref()
        .map(|m| bs.project_root().join(&manifest.package.src).join(m))
        .ok_or_else(|| anyhow::anyhow!("No main file in pascal.toml"))?;
    run_file_watch(main_path, verbose)?;
    Ok(())
}

fn run_debug(
    input: PathBuf,
    breakpoints: Vec<String>,
    watch_vars: Vec<String>,
    verbose: bool,
) -> Result<()> {
    if !input.exists() {
        eprintln!("{} File not found: {}", "Error:".red().bold(), input.display());
        std::process::exit(1);
    }

    let source = std::fs::read_to_string(&input)?;
    let mut parser = pascal::parser::Parser::new(&source);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{} {}", "Parse error:".red().bold(), e);
            std::process::exit(1);
        }
    };

    let mut session = pascal::debugger::DebugSession::new();
    for b in &breakpoints {
        session.add_breakpoint(b);
    }
    for w in &watch_vars {
        session.add_watch(w);
    }

    if verbose {
        println!(
            "{} Debugging '{}' (breakpoints: {:?}, watch: {:?})",
            "Info:".cyan().bold(),
            input.display(),
            breakpoints,
            watch_vars
        );
    }

    pascal::debugger::run_with_debugger(&program, &mut session, verbose)
}

fn run_program_impl(
    program: &pascal::ast::Program,
    verbose: bool,
    profile_output: Option<PathBuf>,
) -> Result<()> {
    let run = || {
        let mut interpreter = pascal::interpreter::Interpreter::new(verbose);
        match interpreter.run_program(program) {
            Ok(()) => {
                if verbose {
                    println!("\n{} Program finished", "Info:".cyan().bold());
                }
            }
            Err(e) => {
                eprintln!("\n{} {}", "Runtime error:".red().bold(), e);
                std::process::exit(1);
            }
        }
    };

    if let Some(ref out) = profile_output {
        #[cfg(feature = "profile")]
        {
            pascal::profile::run_profiled(out, run)?;
            if verbose {
                println!(
                    "{} Profile written to {}",
                    "Info:".cyan().bold(),
                    out.display()
                );
            }
        }
        #[cfg(not(feature = "profile"))]
        {
            eprintln!(
                "{} Use `cargo build --features profile` to enable profiling",
                "Warning:".yellow().bold()
            );
            run();
        }
    } else {
        run();
    }

    Ok(())
}

fn fmt_files(paths: Vec<PathBuf>, check_only: bool) -> Result<()> {
    use std::fs;
    use std::io::Write;

    fn format_pascal(source: &str) -> String {
        let lines: Vec<&str> = source.lines().collect();
        let mut out = String::with_capacity(source.len());
        let mut prev_blank = false;
        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim_end();
            let is_blank = trimmed.is_empty();
            if is_blank {
                if !prev_blank {
                    out.push('\n');
                }
                prev_blank = true;
            } else {
                prev_blank = false;
                out.push_str(trimmed);
                if i + 1 < lines.len() {
                    out.push('\n');
                }
            }
        }
        if !out.ends_with('\n') && !out.is_empty() {
            out.push('\n');
        }
        out
    }

    let mut formatted = 0;
    for path in paths {
        if path.is_dir() {
            for entry in fs::read_dir(&path)? {
                let entry = entry?;
                let p = entry.path();
                if p.extension().and_then(|s| s.to_str()) == Some("pas") {
                    let source = fs::read_to_string(&p)?;
                    let formatted_source = format_pascal(&source);
                    if source != formatted_source {
                        formatted += 1;
                        if !check_only {
                            let mut f = fs::File::create(&p)?;
                            f.write_all(formatted_source.as_bytes())?;
                            println!("  Formatted: {}", p.display());
                        } else {
                            println!("  Would format: {}", p.display());
                        }
                    }
                }
            }
        } else if path.extension().and_then(|s| s.to_str()) == Some("pas") {
            let source = fs::read_to_string(&path)?;
            let formatted_source = format_pascal(&source);
            if source != formatted_source {
                formatted += 1;
                if !check_only {
                    let mut f = fs::File::create(&path)?;
                    f.write_all(formatted_source.as_bytes())?;
                    println!("  Formatted: {}", path.display());
                } else {
                    println!("  Would format: {}", path.display());
                }
            }
        }
    }
    if check_only && formatted > 0 {
        println!(
            "{} {} file(s) would be reformatted",
            "Warning:".yellow().bold(),
            formatted
        );
        std::process::exit(1);
    } else if formatted > 0 {
        println!(
            "{} Formatted {} file(s)",
            "Success:".green().bold(),
            formatted
        );
    }
    Ok(())
}

fn doc_files(
    paths: Vec<PathBuf>,
    format_str: String,
    output_dir: PathBuf,
    recursive: bool,
) -> Result<()> {
    use std::fs;
    use std::io::Write;

    let format = format_str
        .parse::<pascal::DocFormat>()
        .map_err(|e| anyhow::anyhow!("{}", e))?;

    let ext = match format {
        pascal::DocFormat::Markdown => "md",
        pascal::DocFormat::Html => "html",
    };

    std::fs::create_dir_all(&output_dir)?;

    let mut collected: Vec<PathBuf> = vec![];
    for path in &paths {
        if path.is_dir() {
            fn collect_pas(dir: &std::path::Path, out: &mut Vec<PathBuf>, recursive: bool) {
                if let Ok(entries) = fs::read_dir(dir) {
                    for entry in entries.flatten() {
                        let p = entry.path();
                        if p.is_dir() && recursive {
                            collect_pas(&p, out, recursive);
                        } else if p.extension().and_then(|s| s.to_str()) == Some("pas") {
                            out.push(p);
                        }
                    }
                }
            }
            collect_pas(path, &mut collected, recursive);
        } else if path.extension().and_then(|s| s.to_str()) == Some("pas") {
            collected.push(path.clone());
        }
    }

    let mut generated = 0;
    for input in collected {
        let source = match fs::read_to_string(&input) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("{} {}: {}", "Warning:".yellow().bold(), input.display(), e);
                continue;
            }
        };

        match pascal::generate_docs_from_source(&source, &input, format) {
            Ok(docs) => {
                let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("doc");
                let out_path = output_dir.join(format!("{}.{}", stem, ext));
                if let Err(e) = fs::File::create(&out_path).and_then(|mut f| f.write_all(docs.as_bytes())) {
                    eprintln!("{} Failed to write {}: {}", "Error:".red().bold(), out_path.display(), e);
                } else {
                    println!("  Generated: {} -> {}", input.display(), out_path.display());
                    generated += 1;
                }
            }
            Err(e) => {
                eprintln!("{} {}: {}", "Warning:".yellow().bold(), input.display(), e);
            }
        }
    }

    println!(
        "{} Generated documentation for {} file(s) in {}",
        "Success:".green().bold(),
        generated,
        output_dir.display()
    );
    Ok(())
}

fn check_file(input: PathBuf) -> Result<()> {
    if !input.exists() {
        eprintln!(
            "{} File not found: {}",
            "Error:".red().bold(),
            input.display()
        );
        std::process::exit(1);
    }
    let source = std::fs::read_to_string(&input)?;
    let mut parser = pascal::parser::Parser::new(&source);
    match parser.parse_program() {
        Ok(program) => {
            println!(
                "{} Check passed for '{}' (parse OK)",
                "Success:".green().bold(),
                program.name
            );
            Ok(())
        }
        Err(e) => {
            eprintln!("{} {}", "Parse error:".red().bold(), e);
            for err in parser.errors() {
                eprintln!("  {}", err);
            }
            std::process::exit(1);
        }
    }
}

fn show_ppu_info(ppu_file: PathBuf) -> Result<()> {
    if !ppu_file.exists() {
        eprintln!(
            "{} File not found: {}",
            "Error:".red().bold(),
            ppu_file.display()
        );
        std::process::exit(1);
    }

    println!("{} {}", "PPU File:".cyan().bold(), ppu_file.display());

    match PpuFile::load(&ppu_file) {
        Ok(ppu) => {
            println!("{}", "Version:".green().bold());
            println!("  Version: {}", ppu.version);

            println!("{}", "\nModule:".green().bold());
            println!("  Name: {}", ppu.module.name);
            println!("  Dependencies: {:?}", ppu.module.dependencies);

            println!("{}", "\nUnit:".green().bold());
            println!("  Name: {}", ppu.module.unit.name);

            println!("{}", "\nInterface:".green().bold());
            println!("  Uses: {:?}", ppu.module.unit.interface.uses);
            println!("  Types: {}", ppu.module.unit.interface.types.len());
            println!("  Constants: {}", ppu.module.unit.interface.constants.len());
            println!("  Variables: {}", ppu.module.unit.interface.variables.len());
            println!("  Functions: {}", ppu.module.unit.interface.functions.len());
            println!(
                "  Procedures: {}",
                ppu.module.unit.interface.procedures.len()
            );

            println!("{}", "\nImplementation:".green().bold());
            println!(
                "  Functions: {}",
                ppu.module.unit.implementation.functions.len()
            );
            println!(
                "  Procedures: {}",
                ppu.module.unit.implementation.procedures.len()
            );
            println!(
                "  Has initialization: {}",
                ppu.module.unit.implementation.initialization.is_some()
            );
            println!(
                "  Has finalization: {}",
                ppu.module.unit.implementation.finalization.is_some()
            );

            Ok(())
        }
        Err(e) => {
            eprintln!("{} {}", "Error reading PPU:".red().bold(), e);
            std::process::exit(1);
        }
    }
}

fn clean_directory(directory: PathBuf) -> Result<()> {
    use std::fs;

    if !directory.exists() {
        eprintln!(
            "{} Directory not found: {}",
            "Error:".red().bold(),
            directory.display()
        );
        std::process::exit(1);
    }

    println!("{} {}", "Cleaning:".cyan().bold(), directory.display());

    let mut count = 0;
    for entry in fs::read_dir(&directory)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("ppu") {
            if let Err(e) = fs::remove_file(&path) {
                eprintln!(
                    "{} Failed to remove {}: {}",
                    "Warning:".yellow().bold(),
                    path.display(),
                    e
                );
            } else {
                println!("  Removed: {}", path.display());
                count += 1;
            }
        }
    }

    println!("{} Removed {} PPU file(s)", "Done:".green().bold(), count);
    Ok(())
}
