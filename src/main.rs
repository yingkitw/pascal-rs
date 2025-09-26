mod ast;
mod lexer;
mod parser;
mod codegen;

use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};

use crate::{
    codegen::CodeGenerator,
    lexer::Lexer,
    parser::Parser as PascalParser,
};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// Input Pascal source file
    #[arg(short, long)]
    input: PathBuf,

    /// Output file (default: input filename with .s extension)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Show verbose output
    #[arg(short, long, default_value_t = false)]
    verbose: bool,

    /// Only tokenize the input (for debugging)
    #[arg(long, default_value_t = false)]
    tokenize: bool,

    /// Only parse the input (for debugging)
    #[arg(long, default_value_t = false)]
    parse: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    
    if args.verbose {
        println!("Compiling {}...", args.input.display());
    }

    // Read input file
    let input = fs::read_to_string(&args.input)
        .with_context(|| format!("Failed to read input file: {}", args.input.display()))?;

    // Tokenize
    let lexer = Lexer::new(&input);
    
    if args.tokenize {
        // Just print tokens and exit
        for token in lexer {
            match token {
                Ok((_, token, _)) => println!("{:?}", token),
                Err(e) => eprintln!("Lexer error: {}", e),
            }
        }
        return Ok(());
    }

    // Parse
    let mut parser = PascalParser::new(&input);
    let program = parser.parse_program()
        .with_context(|| "Failed to parse program")?;
    
    if args.parse {
        // Just print parsed program and exit
        println!("{:#?}", program);
        return Ok(());
    }

    // Generate code
    let mut codegen = CodeGenerator::new();
    let asm = codegen.generate(&program)
        .with_context(|| "Failed to generate code")?;

    // Determine output path
    let output_path = match args.output {
        Some(path) => path,
        None => {
            let mut path = args.input.clone();
            path.set_extension("s");
            path
        }
    };

    // Write output
    fs::write(&output_path, asm)
        .with_context(|| format!("Failed to write output file: {}", output_path.display()))?;

    if args.verbose {
        println!("Successfully compiled to {}", output_path.display());
        println!("To assemble and link, run:");
        println!("  as {} -o output.o", output_path.display());
        println!("  gcc -no-pie -o program output.o");
    }

    Ok(())
}
