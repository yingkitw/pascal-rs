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
    },

    /// Build the current project (requires pascal.toml)
    Build {
        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
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

        /// Enable parallel compilation
        #[arg(short = 'j', long)]
        parallel: bool,

        /// Number of threads for parallel compilation (0 = auto)
        #[arg(long, default_value = "0")]
        threads: usize,
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
        Commands::Init { name, dir } => BuildSystem::init(&dir, &name),

        Commands::Build { verbose } => open_project(verbose)?.build(),

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
            parallel,
            threads,
        } => compile_file(
            input,
            output,
            search_paths,
            optimization,
            debug,
            !no_ppu,
            !no_cache,
            asm,
            verbose,
            parallel,
            threads,
        ),

        Commands::Info { ppu_file } => show_ppu_info(ppu_file),

        Commands::Run { input, verbose } => {
            if let Some(file) = input {
                run_file(file, verbose)
            } else {
                open_project(verbose)?.run()
            }
        }

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
    parallel: bool,
    threads: usize,
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
            eprintln!("{} Failed to initialize thread pool: {}", "Error:".red().bold(), e);
            std::process::exit(1);
        }

        if verbose {
            println!("{} Thread pool initialized", "Info:".cyan().bold());
        }
    }

    // Read source file
    let source = std::fs::read_to_string(&input)?;
    if verbose {
        println!("{} Read {} bytes", "Info:".cyan().bold(), source.len());
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
        println!("{} Optimization level {}", "Info:".cyan().bold(), optimization);
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

        println!(
            "{} Generated assembly: {}",
            "Success:".green().bold(),
            asm_path.display()
        );
    }

    println!(
        "{} Compiled '{}' successfully",
        "Success:".green().bold(),
        program.name
    );

    Ok(())
}

fn run_file(input: PathBuf, verbose: bool) -> Result<()> {
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
        println!("{} Parsed program '{}'", "Info:".cyan().bold(), program.name);
    }

    // Interpret
    let mut interpreter = pascal::interpreter::Interpreter::new(verbose);
    match interpreter.run_program(&program) {
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

    Ok(())
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
            println!("  Procedures: {}", ppu.module.unit.interface.procedures.len());

            println!("{}", "\nImplementation:".green().bold());
            println!("  Functions: {}", ppu.module.unit.implementation.functions.len());
            println!("  Procedures: {}", ppu.module.unit.implementation.procedures.len());
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
