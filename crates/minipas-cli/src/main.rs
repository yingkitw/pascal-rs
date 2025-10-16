use clap::{Parser, Subcommand};
use minipas_driver::{Compiler, CompileOptions, PpuFile};
use std::path::PathBuf;
use anyhow::Result;
use colored::Colorize;

#[derive(Parser)]
#[command(name = "minipas")]
#[command(about = "MiniPAS - A Pascal compiler with modern features")]
#[command(version = "0.1.0")]
#[command(author = "MiniPAS Team")]
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
            verbose,
        } => compile_file(
            input,
            output,
            search_paths,
            optimization,
            debug,
            !no_ppu,
            !no_cache,
            verbose,
        ),
        Commands::Info { ppu_file } => show_ppu_info(ppu_file),
        Commands::Clean { directory } => clean_directory(directory),
    }
}

fn compile_file(
    input: PathBuf,
    output: PathBuf,
    search_paths: Vec<PathBuf>,
    optimization: u8,
    debug: bool,
    generate_ppu: bool,
    use_ppu: bool,
    verbose: bool,
) -> Result<()> {
    // Validate input file
    if !input.exists() {
        eprintln!("{} File not found: {}", "Error:".red().bold(), input.display());
        std::process::exit(1);
    }
    
    if verbose {
        println!("{} {}", "Compiling:".green().bold(), input.display());
    }
    
    // Create compilation options
    let mut options = CompileOptions {
        search_paths: vec![PathBuf::from(".")],
        output_dir: output.clone(),
        generate_ppu,
        use_ppu,
        optimization_level: optimization,
        debug_info: debug,
        target: "native".to_string(),
    };
    
    // Add custom search paths
    for path in search_paths {
        options.search_paths.push(path);
    }
    
    if verbose {
        println!("{}", "Configuration:".cyan().bold());
        println!("  Output directory: {}", output.display());
        println!("  Optimization: O{}", optimization);
        println!("  Debug info: {}", debug);
        println!("  Generate PPU: {}", generate_ppu);
        println!("  Use PPU cache: {}", use_ppu);
        println!("  Search paths: {}", options.search_paths.len());
    }
    
    // Create compiler
    let mut compiler = Compiler::new(options);
    
    // Compile the file
    if verbose {
        println!("{}", "Parsing and compiling...".cyan().bold());
    }
    
    match compiler.compile_file(&input) {
        Ok(result) => {
            println!("{} Compiled module: {}", "Success:".green().bold(), result.module.name);
            
            if let Some(ppu_path) = result.ppu_path {
                if verbose {
                    println!("  PPU file: {}", ppu_path.display());
                }
            }
            
            // Show warnings
            if !result.warnings.is_empty() {
                println!("{}", "Warnings:".yellow().bold());
                for warning in &result.warnings {
                    println!("  {}", warning.yellow());
                }
            }
            
            // Show compilation order
            if verbose {
                match compiler.get_compilation_order() {
                    Ok(order) => {
                        println!("{}", "Compilation order:".cyan().bold());
                        for (i, module) in order.iter().enumerate() {
                            println!("  {}. {}", i + 1, module);
                        }
                    }
                    Err(e) => {
                        eprintln!("{} {}", "Warning:".yellow().bold(), e);
                    }
                }
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
        eprintln!("{} File not found: {}", "Error:".red().bold(), ppu_file.display());
        std::process::exit(1);
    }
    
    println!("{} {}", "PPU File:".cyan().bold(), ppu_file.display());
    
    match PpuFile::read_from_file(&ppu_file) {
        Ok(ppu) => {
            println!("{}", "Header:".green().bold());
            println!("  Version: {}", ppu.header.version);
            println!("  Interface CRC: 0x{:08x}", ppu.header.interface_crc);
            println!("  Implementation CRC: 0x{:08x}", ppu.header.implementation_crc);
            println!("  Unit CRC: 0x{:08x}", ppu.header.unit_crc);
            println!("  Data size: {} bytes", ppu.header.data_size);
            
            println!("{}", "\nUnit:".green().bold());
            println!("  Name: {}", ppu.unit.name);
            println!("  Uses: {:?}", ppu.unit.uses);
            
            println!("{}", "\nInterface:".green().bold());
            println!("  Types: {}", ppu.unit.interface.types.len());
            println!("  Constants: {}", ppu.unit.interface.constants.len());
            println!("  Variables: {}", ppu.unit.interface.variables.len());
            println!("  Functions: {}", ppu.unit.interface.functions.len());
            println!("  Procedures: {}", ppu.unit.interface.procedures.len());
            
            println!("{}", "\nImplementation:".green().bold());
            println!("  Uses: {:?}", ppu.unit.implementation.uses);
            println!("  Types: {}", ppu.unit.implementation.types.len());
            println!("  Constants: {}", ppu.unit.implementation.constants.len());
            println!("  Variables: {}", ppu.unit.implementation.variables.len());
            println!("  Functions: {}", ppu.unit.implementation.functions.len());
            println!("  Procedures: {}", ppu.unit.implementation.procedures.len());
            
            if ppu.verify_checksums() {
                println!("\n{} Checksums verified", "✓".green().bold());
            } else {
                println!("\n{} Checksum verification failed", "✗".red().bold());
            }
            
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
        eprintln!("{} Directory not found: {}", "Error:".red().bold(), directory.display());
        std::process::exit(1);
    }
    
    println!("{} {}", "Cleaning:".cyan().bold(), directory.display());
    
    let mut count = 0;
    for entry in fs::read_dir(&directory)? {
        let entry = entry?;
        let path = entry.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("ppu") {
            if let Err(e) = fs::remove_file(&path) {
                eprintln!("{} Failed to remove {}: {}", "Warning:".yellow().bold(), path.display(), e);
            } else {
                println!("  Removed: {}", path.display());
                count += 1;
            }
        }
    }
    
    println!("{} Removed {} PPU file(s)", "Done:".green().bold(), count);
    Ok(())
}
