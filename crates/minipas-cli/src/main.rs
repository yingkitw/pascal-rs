use clap::Parser;
use minipas_lexer::Lexer;
use minipas_parser::Parser as PascalParser;
use minipas_codegen::CodeGenerator;
use std::fs;
// std::io is not used in this file
use anyhow::Result;

#[derive(Parser)]
#[command(name = "minipas")]
#[command(about = "A minimal Pascal compiler written in Rust")]
#[command(version)]
struct Cli {
    /// Input Pascal file
    #[arg(short, long)]
    input: String,
    
    /// Output assembly file
    #[arg(short, long)]
    output: Option<String>,
    
    /// Show tokens (lexical analysis)
    #[arg(long)]
    tokenize: bool,
    
    /// Show parse tree (syntax analysis)
    #[arg(long)]
    parse: bool,
    
    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    if cli.verbose {
        eprintln!("Reading input file: {}", cli.input);
    }
    
    // Read input file
    let input = fs::read_to_string(&cli.input)?;
    
    if cli.tokenize {
        // Tokenize only
        let mut lexer = Lexer::new(&input);
        println!("Tokens:");
        while let Some(result) = lexer.next() {
            match result {
                Ok((start, token, end)) => {
                    println!("  {}..{}: {:?}", start, end, token);
                }
                Err(e) => {
                    eprintln!("Lexer error: {}", e);
                    return Err(e.into());
                }
            }
        }
        return Ok(());
    }
    
    if cli.parse {
        // Parse only
        let mut parser = PascalParser::new(&input);
        match parser.parse_program() {
            Ok(program) => {
                println!("Parse successful!");
                println!("Program: {}", program.name);
                println!("Variables: {}", program.block.vars.len());
                println!("Procedures: {}", program.block.procedures.len());
                println!("Functions: {}", program.block.functions.len());
                println!("Statements: {}", program.block.statements.len());
            }
            Err(e) => {
                eprintln!("Parse error: {}", e);
                return Err(e.into());
            }
        }
        return Ok(());
    }
    
    // Full compilation
    if cli.verbose {
        eprintln!("Parsing program...");
    }
    
    let mut parser = PascalParser::new(&input);
    let program = parser.parse_program()?;
    
    if cli.verbose {
        eprintln!("Generating assembly code...");
    }
    
    let mut codegen = CodeGenerator::new();
    let assembly = codegen.generate(&program)?;
    
    // Determine output file
    let output_file = cli.output.unwrap_or_else(|| {
        let input_path = std::path::Path::new(&cli.input);
        let stem = input_path.file_stem().unwrap().to_str().unwrap();
        format!("{}.s", stem)
    });
    
    if cli.verbose {
        eprintln!("Writing assembly to: {}", output_file);
    }
    
    // Write output
    fs::write(&output_file, assembly)?;
    
    println!("Compilation successful!");
    println!("Assembly written to: {}", output_file);
    
    Ok(())
}
