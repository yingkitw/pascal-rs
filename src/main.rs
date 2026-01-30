use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::Colorize;
use pascal::{CompileOptions, Compiler, PpuFile};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "pascal")]
#[command(about = "Pascal - A production-ready optimizing Pascal compiler")]
#[command(version = "0.1.0")]
#[command(author = "Pascal Team")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
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
    },

    /// Show information about a compiled unit
    Info {
        /// PPU file to inspect
        ppu_file: PathBuf,
    },

    /// Clean compiled files
    Clean {
        /// Directory to clean
        #[arg(default_value = ".")]
        directory: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
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
        ),
        Commands::Info { ppu_file } => show_ppu_info(ppu_file),
        Commands::Clean { directory } => clean_directory(directory),
    }
}

fn compile_file(
    input: PathBuf,
    output: PathBuf,
    _search_paths: Vec<PathBuf>,
    _optimization: u8,
    _debug: bool,
    _generate_ppu: bool,
    _use_ppu: bool,
    _generate_asm: bool,
    verbose: bool,
) -> Result<()> {
    // Validate input file
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
    }

    // Create compilation options
    let options = CompileOptions {
        output_dir: output.clone(),
    };

    if verbose {
        println!("{}", "Configuration:".cyan().bold());
        println!("  Output directory: {}", output.display());
    }

    // Create compiler
    let mut compiler = Compiler::new(options);

    // Compile the file
    if verbose {
        println!("{}", "Parsing and compiling...".cyan().bold());
    }

    match compiler.compile_file(input.to_str().unwrap()) {
        Ok(result) => {
            println!("{} Compilation complete", "Success:".green().bold());

            for message in result.messages {
                println!("  {}", message);
            }

            Ok(())
        }
        Err(e) => {
            eprintln!("{} {}", "Compilation failed:".red().bold(), e);
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

    match PpuFile::read_from_file(&ppu_file) {
        Ok(ppu) => {
            println!("{}", "Version:".green().bold());
            println!("  Version: {}", ppu.version);

            println!("{}", "\nUnit:".green().bold());
            println!("  Name: {}", ppu.unit.name);

            println!("{}", "\nInterface:".green().bold());
            println!("  Uses: {:?}", ppu.unit.interface.uses);
            println!("  Types: {}", ppu.unit.interface.types.len());
            println!("  Constants: {}", ppu.unit.interface.constants.len());
            println!("  Variables: {}", ppu.unit.interface.variables.len());
            println!("  Functions: {}", ppu.unit.interface.functions.len());
            println!("  Procedures: {}", ppu.unit.interface.procedures.len());

            println!("{}", "\nImplementation:".green().bold());
            println!("  Functions: {}", ppu.unit.implementation.functions.len());
            println!("  Procedures: {}", ppu.unit.implementation.procedures.len());
            println!(
                "  Has initialization: {}",
                ppu.unit.implementation.initialization.is_some()
            );
            println!(
                "  Has finalization: {}",
                ppu.unit.implementation.finalization.is_some()
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
