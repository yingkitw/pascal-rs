//! Build system and package manager for Pascal projects.
//!
//! Provides cargo/npm-like project management:
//! - `pascal init` — scaffold a new project
//! - `pascal build` — compile all units in dependency order
//! - `pascal add <dep>` — add a dependency
//! - `pascal remove <dep>` — remove a dependency
//!
//! Project manifest: `pascal.toml`
//! Lock file: `pascal.lock`

use anyhow::{anyhow, Context, Result};
use colored::Colorize;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Manifest (pascal.toml)
// ---------------------------------------------------------------------------

/// Top-level pascal.toml structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: Package,
    #[serde(default)]
    pub dependencies: BTreeMap<String, DependencySpec>,
    #[serde(default)]
    pub build: BuildConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    #[serde(default = "default_version")]
    pub version: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default = "default_license")]
    pub license: String,
    #[serde(default = "default_src")]
    pub src: String,
    #[serde(default)]
    pub main: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    /// Simple version string: `"1.0"`
    Version(String),
    /// Detailed spec with path or git
    Detailed(DetailedDependency),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetailedDependency {
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub git: Option<String>,
    #[serde(default)]
    pub branch: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct BuildConfig {
    #[serde(default)]
    pub optimization: u8,
    #[serde(default = "default_output")]
    pub output: String,
    #[serde(default)]
    pub verbose: bool,
}

fn default_version() -> String { "0.1.0".to_string() }
fn default_license() -> String { "MIT".to_string() }
fn default_src() -> String { "src".to_string() }
fn default_output() -> String { "build".to_string() }

impl Manifest {
    /// Load manifest from a pascal.toml file
    pub fn load(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        let manifest: Manifest = toml::from_str(&content)
            .with_context(|| format!("Failed to parse {}", path.display()))?;
        Ok(manifest)
    }

    /// Save manifest to a pascal.toml file
    pub fn save(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self)
            .context("Failed to serialize manifest")?;
        std::fs::write(path, content)
            .with_context(|| format!("Failed to write {}", path.display()))?;
        Ok(())
    }

    /// Find pascal.toml by walking up from `start_dir`
    pub fn find(start_dir: &Path) -> Option<PathBuf> {
        let mut dir = start_dir.to_path_buf();
        loop {
            let candidate = dir.join("pascal.toml");
            if candidate.exists() {
                return Some(candidate);
            }
            if !dir.pop() {
                return None;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Lock file (pascal.lock)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LockFile {
    pub packages: BTreeMap<String, LockedPackage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    pub version: String,
    pub source: String,
    pub checksum: String,
}

impl LockFile {
    pub fn load(path: &Path) -> Result<Self> {
        if !path.exists() {
            return Ok(Self::default());
        }
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        let lock: LockFile = toml::from_str(&content)
            .with_context(|| format!("Failed to parse {}", path.display()))?;
        Ok(lock)
    }

    pub fn save(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self)
            .context("Failed to serialize lock file")?;
        std::fs::write(path, content)
            .with_context(|| format!("Failed to write {}", path.display()))?;
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Build graph — topological sort of units
// ---------------------------------------------------------------------------

#[derive(Debug)]
struct BuildUnit {
    name: String,
    path: PathBuf,
    uses: Vec<String>,
}

/// Discover .pas files in `src_dir`, parse their `uses` clauses, and return
/// a topologically sorted build order.
fn discover_units(src_dir: &Path) -> Result<Vec<BuildUnit>> {
    let mut units = Vec::new();
    if !src_dir.exists() {
        return Ok(units);
    }
    for entry in std::fs::read_dir(src_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("pas") {
            let source = std::fs::read_to_string(&path)?;
            let uses = extract_uses(&source);
            let name = path.file_stem().unwrap().to_str().unwrap().to_string();
            units.push(BuildUnit { name, path, uses });
        }
    }
    Ok(units)
}

/// Quick extraction of `uses` clause identifiers from source without full parse.
fn extract_uses(source: &str) -> Vec<String> {
    let lower = source.to_lowercase();
    // Find "uses" keyword — may be followed by space, newline, or other whitespace
    let Some(pos) = lower.find("uses") else {
        return vec![];
    };
    let after_uses = pos + 4;
    // Must be followed by whitespace (not part of a longer identifier)
    if after_uses >= lower.len() {
        return vec![];
    }
    let next_ch = lower.as_bytes()[after_uses];
    if !next_ch.is_ascii_whitespace() {
        return vec![];
    }
    let rest = &source[after_uses..];
    let Some(semi) = rest.find(';') else {
        return vec![];
    };
    let clause = &rest[..semi];
    clause
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect()
}

/// Topological sort of build units by their `uses` dependencies.
fn topo_sort(units: &[BuildUnit]) -> Result<Vec<usize>> {
    let name_to_idx: HashMap<String, usize> = units
        .iter()
        .enumerate()
        .map(|(i, u)| (u.name.to_lowercase(), i))
        .collect();

    let n = units.len();
    let mut in_degree = vec![0usize; n];
    let mut adj: Vec<Vec<usize>> = vec![vec![]; n];

    for (i, unit) in units.iter().enumerate() {
        for dep in &unit.uses {
            if let Some(&j) = name_to_idx.get(&dep.to_lowercase()) {
                adj[j].push(i);
                in_degree[i] += 1;
            }
            // External deps (not in src/) are ignored in ordering
        }
    }

    let mut queue: Vec<usize> = (0..n).filter(|&i| in_degree[i] == 0).collect();
    let mut order = Vec::with_capacity(n);

    while let Some(idx) = queue.pop() {
        order.push(idx);
        for &next in &adj[idx] {
            in_degree[next] -= 1;
            if in_degree[next] == 0 {
                queue.push(next);
            }
        }
    }

    if order.len() != n {
        return Err(anyhow!("Circular dependency detected among units"));
    }
    Ok(order)
}

// ---------------------------------------------------------------------------
// BuildSystem — the main entry point
// ---------------------------------------------------------------------------

pub struct BuildSystem {
    project_root: PathBuf,
    manifest: Manifest,
    verbose: bool,
}

impl BuildSystem {
    /// Create a BuildSystem from a project root containing pascal.toml
    pub fn open(project_root: &Path, verbose: bool) -> Result<Self> {
        let manifest_path = project_root.join("pascal.toml");
        let manifest = Manifest::load(&manifest_path)?;
        Ok(Self {
            project_root: project_root.to_path_buf(),
            manifest,
            verbose,
        })
    }

    /// `pascal init [name]` — scaffold a new project
    pub fn init(dir: &Path, name: &str) -> Result<()> {
        let project_dir = dir.join(name);
        std::fs::create_dir_all(&project_dir)?;

        // pascal.toml
        let manifest = Manifest {
            package: Package {
                name: name.to_string(),
                version: "0.1.0".to_string(),
                description: format!("A Pascal project: {}", name),
                authors: vec![],
                license: "MIT".to_string(),
                src: "src".to_string(),
                main: Some(format!("{}.pas", name)),
            },
            dependencies: BTreeMap::new(),
            build: BuildConfig {
                optimization: 0,
                output: "build".to_string(),
                verbose: false,
            },
        };
        manifest.save(&project_dir.join("pascal.toml"))?;

        // src/
        let src_dir = project_dir.join("src");
        std::fs::create_dir_all(&src_dir)?;

        // src/<name>.pas
        let main_source = format!(
            "program {};\nbegin\n  writeln('Hello from {}!');\nend.\n",
            capitalize(name),
            name
        );
        std::fs::write(src_dir.join(format!("{}.pas", name)), main_source)?;

        // tests/
        std::fs::create_dir_all(project_dir.join("tests"))?;

        // examples/
        std::fs::create_dir_all(project_dir.join("examples"))?;

        // .gitignore
        std::fs::write(
            project_dir.join(".gitignore"),
            "build/\n*.ppu\n*.o\n*.asm\n",
        )?;

        // README.md
        let readme = format!("# {}\n\nA Pascal project.\n\n## Build\n\n```bash\npascal build\npascal run\n```\n", name);
        std::fs::write(project_dir.join("README.md"), readme)?;

        println!(
            "{} Created project '{}' at {}",
            "Success:".green().bold(),
            name,
            project_dir.display()
        );
        println!("  {}", "pascal.toml".cyan());
        println!("  {}", format!("src/{}.pas", name).cyan());
        println!("  {}", "tests/".cyan());
        println!("  {}", "examples/".cyan());
        println!("\nGet started:");
        println!("  cd {}", name);
        println!("  pascal build");
        println!("  pascal run");

        Ok(())
    }

    /// `pascal build` — compile all units in dependency order, then the main program
    pub fn build(&self) -> Result<()> {
        let src_dir = self.project_root.join(&self.manifest.package.src);
        let output_dir = self.project_root.join(&self.manifest.build.output);
        std::fs::create_dir_all(&output_dir)?;

        println!(
            "{} {} v{}",
            "Building".green().bold(),
            self.manifest.package.name,
            self.manifest.package.version
        );

        // Resolve local path dependencies
        self.resolve_dependencies()?;

        // Discover and sort units
        let units = discover_units(&src_dir)?;
        if units.is_empty() {
            println!("  {} No .pas files found in {}", "Warning:".yellow().bold(), src_dir.display());
            return Ok(());
        }

        let order = topo_sort(&units)?;

        if self.verbose {
            println!("  {} Build order:", "Info:".cyan().bold());
            for &idx in &order {
                println!("    {} (uses: {:?})", units[idx].name, units[idx].uses);
            }
        }

        // Compile each unit in order
        let mut compiled = 0;
        let mut errors = 0;
        let total = order.len();

        for &idx in &order {
            let unit = &units[idx];
            let source = std::fs::read_to_string(&unit.path)?;

            print!(
                "  {} [{}/{}] {}...",
                "Compiling".green(),
                compiled + 1,
                total,
                unit.name
            );

            let mut parser = crate::parser::Parser::new(&source);
            match parser.parse_program() {
                Ok(program) => {
                    // Run through interpreter to validate
                    let mut interp = crate::interpreter::Interpreter::new(false);
                    match interp.run_program(&program) {
                        Ok(()) => {
                            println!(" {}", "ok".green());
                        }
                        Err(e) => {
                            println!(" {}", "FAILED".red());
                            eprintln!("    Runtime error: {}", e);
                            errors += 1;
                        }
                    }
                    compiled += 1;
                }
                Err(e) => {
                    println!(" {}", "FAILED".red());
                    eprintln!("    Parse error: {}", e);
                    for err in parser.errors() {
                        eprintln!("    {}", err);
                    }
                    errors += 1;
                    compiled += 1;
                }
            }
        }

        // Update lock file
        self.update_lock_file()?;

        println!();
        if errors == 0 {
            println!(
                "  {} Built {} unit(s) successfully",
                "Finished".green().bold(),
                compiled
            );
        } else {
            println!(
                "  {} {} error(s) in {} unit(s)",
                "Failed:".red().bold(),
                errors,
                compiled
            );
            return Err(anyhow!("Build failed with {} error(s)", errors));
        }

        Ok(())
    }

    /// `pascal run` (project mode) — build then run the main program
    pub fn run(&self) -> Result<()> {
        let src_dir = self.project_root.join(&self.manifest.package.src);

        // Determine main file
        let main_file = if let Some(ref main) = self.manifest.package.main {
            src_dir.join(main)
        } else {
            // Default: look for <name>.pas
            src_dir.join(format!("{}.pas", self.manifest.package.name))
        };

        if !main_file.exists() {
            return Err(anyhow!(
                "Main file not found: {}\nSet [package] main in pascal.toml",
                main_file.display()
            ));
        }

        let source = std::fs::read_to_string(&main_file)?;
        let mut parser = crate::parser::Parser::new(&source);
        let program = parser
            .parse_program()
            .map_err(|e| anyhow!("Parse error: {}", e))?;

        let mut interp = crate::interpreter::Interpreter::new(self.verbose);
        interp
            .run_program(&program)
            .map_err(|e| anyhow!("Runtime error: {}", e))?;

        Ok(())
    }

    /// `pascal add <name> [--path <path>] [--git <url>]`
    pub fn add_dependency(
        &mut self,
        name: &str,
        version: Option<&str>,
        path: Option<&str>,
        git: Option<&str>,
    ) -> Result<()> {
        let spec = if let Some(p) = path {
            DependencySpec::Detailed(DetailedDependency {
                version: version.map(|s| s.to_string()),
                path: Some(p.to_string()),
                git: None,
                branch: None,
            })
        } else if let Some(g) = git {
            DependencySpec::Detailed(DetailedDependency {
                version: version.map(|s| s.to_string()),
                path: None,
                git: Some(g.to_string()),
                branch: None,
            })
        } else {
            DependencySpec::Version(version.unwrap_or("*").to_string())
        };

        self.manifest
            .dependencies
            .insert(name.to_string(), spec);

        let manifest_path = self.project_root.join("pascal.toml");
        self.manifest.save(&manifest_path)?;

        println!(
            "{} Added dependency '{}'",
            "Success:".green().bold(),
            name
        );
        Ok(())
    }

    /// `pascal remove <name>`
    pub fn remove_dependency(&mut self, name: &str) -> Result<()> {
        if self.manifest.dependencies.remove(name).is_none() {
            return Err(anyhow!("Dependency '{}' not found in pascal.toml", name));
        }

        let manifest_path = self.project_root.join("pascal.toml");
        self.manifest.save(&manifest_path)?;

        println!(
            "{} Removed dependency '{}'",
            "Success:".green().bold(),
            name
        );
        Ok(())
    }

    /// Resolve path-based dependencies: copy/link their units into the build
    fn resolve_dependencies(&self) -> Result<()> {
        for (name, spec) in &self.manifest.dependencies {
            match spec {
                DependencySpec::Version(_ver) => {
                    if self.verbose {
                        println!(
                            "  {} Dependency '{}' (registry — not yet supported)",
                            "Info:".cyan().bold(),
                            name
                        );
                    }
                }
                DependencySpec::Detailed(detail) => {
                    if let Some(ref dep_path) = detail.path {
                        let abs_path = self.project_root.join(dep_path);
                        if !abs_path.exists() {
                            return Err(anyhow!(
                                "Dependency '{}' path not found: {}",
                                name,
                                abs_path.display()
                            ));
                        }
                        if self.verbose {
                            println!(
                                "  {} Dependency '{}' from {}",
                                "Info:".cyan().bold(),
                                name,
                                abs_path.display()
                            );
                        }
                    }
                    if let Some(ref _git_url) = detail.git {
                        if self.verbose {
                            println!(
                                "  {} Dependency '{}' (git — fetch not yet implemented)",
                                "Info:".cyan().bold(),
                                name
                            );
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Update pascal.lock with checksums of current source files
    fn update_lock_file(&self) -> Result<()> {
        let src_dir = self.project_root.join(&self.manifest.package.src);
        let lock_path = self.project_root.join("pascal.lock");
        let mut lock = LockFile::default();

        if src_dir.exists() {
            for entry in std::fs::read_dir(&src_dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("pas") {
                    let name = path.file_stem().unwrap().to_str().unwrap().to_string();
                    let content = std::fs::read_to_string(&path)?;
                    let checksum = format!("{:x}", Sha256::digest(content.as_bytes()));
                    lock.packages.insert(
                        name,
                        LockedPackage {
                            version: self.manifest.package.version.clone(),
                            source: "local".to_string(),
                            checksum,
                        },
                    );
                }
            }
        }

        // Include dependency info
        for (name, spec) in &self.manifest.dependencies {
            let (version, source) = match spec {
                DependencySpec::Version(v) => (v.clone(), "registry".to_string()),
                DependencySpec::Detailed(d) => {
                    let v = d.version.clone().unwrap_or_else(|| "*".to_string());
                    let s = if d.path.is_some() {
                        format!("path:{}", d.path.as_ref().unwrap())
                    } else if d.git.is_some() {
                        format!("git:{}", d.git.as_ref().unwrap())
                    } else {
                        "registry".to_string()
                    };
                    (v, s)
                }
            };
            lock.packages.entry(name.clone()).or_insert(LockedPackage {
                version,
                source,
                checksum: String::new(),
            });
        }

        lock.save(&lock_path)?;
        Ok(())
    }

    pub fn manifest(&self) -> &Manifest {
        &self.manifest
    }

    pub fn project_root(&self) -> &Path {
        &self.project_root
    }
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_manifest_roundtrip() {
        let manifest = Manifest {
            package: Package {
                name: "myproject".to_string(),
                version: "1.0.0".to_string(),
                description: "Test project".to_string(),
                authors: vec!["Alice".to_string()],
                license: "MIT".to_string(),
                src: "src".to_string(),
                main: Some("myproject.pas".to_string()),
            },
            dependencies: BTreeMap::new(),
            build: BuildConfig::default(),
        };

        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("pascal.toml");
        manifest.save(&path).unwrap();

        let loaded = Manifest::load(&path).unwrap();
        assert_eq!(loaded.package.name, "myproject");
        assert_eq!(loaded.package.version, "1.0.0");
    }

    #[test]
    fn test_manifest_parse_with_deps() {
        let toml_str = r#"
[package]
name = "calculator"
version = "0.2.0"
description = "A calculator"

[dependencies]
mathlib = "1.0"
utils = { path = "../utils" }
network = { git = "https://github.com/example/network.git", branch = "main" }

[build]
optimization = 2
output = "dist"
"#;
        let manifest: Manifest = toml::from_str(toml_str).unwrap();
        assert_eq!(manifest.package.name, "calculator");
        assert_eq!(manifest.dependencies.len(), 3);
        assert_eq!(manifest.build.optimization, 2);
        assert_eq!(manifest.build.output, "dist");

        match &manifest.dependencies["mathlib"] {
            DependencySpec::Version(v) => assert_eq!(v, "1.0"),
            _ => panic!("Expected version string"),
        }
        match &manifest.dependencies["utils"] {
            DependencySpec::Detailed(d) => assert_eq!(d.path.as_deref(), Some("../utils")),
            _ => panic!("Expected detailed dep"),
        }
    }

    #[test]
    fn test_manifest_find() {
        let dir = tempfile::tempdir().unwrap();
        let sub = dir.path().join("a").join("b").join("c");
        fs::create_dir_all(&sub).unwrap();
        fs::write(dir.path().join("pascal.toml"), "[package]\nname = \"test\"\n").unwrap();

        let found = Manifest::find(&sub);
        assert!(found.is_some());
        assert_eq!(found.unwrap(), dir.path().join("pascal.toml"));
    }

    #[test]
    fn test_extract_uses() {
        assert_eq!(extract_uses("program Foo; uses A, B, C; begin end."),
                   vec!["A", "B", "C"]);
        assert_eq!(extract_uses("program Foo; begin end."), Vec::<String>::new());
        assert_eq!(extract_uses("program Foo;\nuses\n  SysUtils,\n  Classes;\nbegin\nend."),
                   vec!["SysUtils", "Classes"]);
    }

    #[test]
    fn test_topo_sort_simple() {
        let units = vec![
            BuildUnit { name: "a".into(), path: "a.pas".into(), uses: vec!["b".into()] },
            BuildUnit { name: "b".into(), path: "b.pas".into(), uses: vec![] },
            BuildUnit { name: "c".into(), path: "c.pas".into(), uses: vec!["a".into(), "b".into()] },
        ];
        let order = topo_sort(&units).unwrap();
        let names: Vec<&str> = order.iter().map(|&i| units[i].name.as_str()).collect();
        // b must come before a, a before c
        let pos_a = names.iter().position(|&n| n == "a").unwrap();
        let pos_b = names.iter().position(|&n| n == "b").unwrap();
        let pos_c = names.iter().position(|&n| n == "c").unwrap();
        assert!(pos_b < pos_a);
        assert!(pos_a < pos_c);
    }

    #[test]
    fn test_topo_sort_circular() {
        let units = vec![
            BuildUnit { name: "a".into(), path: "a.pas".into(), uses: vec!["b".into()] },
            BuildUnit { name: "b".into(), path: "b.pas".into(), uses: vec!["a".into()] },
        ];
        assert!(topo_sort(&units).is_err());
    }

    #[test]
    fn test_init_creates_project() {
        let dir = tempfile::tempdir().unwrap();
        BuildSystem::init(dir.path(), "hello").unwrap();

        let project = dir.path().join("hello");
        assert!(project.join("pascal.toml").exists());
        assert!(project.join("src/hello.pas").exists());
        assert!(project.join("tests").exists());
        assert!(project.join("examples").exists());
        assert!(project.join(".gitignore").exists());
        assert!(project.join("README.md").exists());

        // Verify manifest is valid
        let manifest = Manifest::load(&project.join("pascal.toml")).unwrap();
        assert_eq!(manifest.package.name, "hello");
        assert_eq!(manifest.package.version, "0.1.0");
    }

    #[test]
    fn test_build_simple_project() {
        let dir = tempfile::tempdir().unwrap();
        BuildSystem::init(dir.path(), "testproj").unwrap();

        let project = dir.path().join("testproj");
        let bs = BuildSystem::open(&project, false).unwrap();
        bs.build().unwrap();

        // Lock file should exist after build
        assert!(project.join("pascal.lock").exists());
    }

    #[test]
    fn test_add_remove_dependency() {
        let dir = tempfile::tempdir().unwrap();
        BuildSystem::init(dir.path(), "deptest").unwrap();

        let project = dir.path().join("deptest");
        let mut bs = BuildSystem::open(&project, false).unwrap();

        // Add
        bs.add_dependency("mathlib", Some("1.0"), None, None).unwrap();
        assert!(bs.manifest().dependencies.contains_key("mathlib"));

        // Reload from disk
        let manifest = Manifest::load(&project.join("pascal.toml")).unwrap();
        assert!(manifest.dependencies.contains_key("mathlib"));

        // Remove
        bs.remove_dependency("mathlib").unwrap();
        assert!(!bs.manifest().dependencies.contains_key("mathlib"));
    }

    #[test]
    fn test_lock_file_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("pascal.lock");

        let mut lock = LockFile::default();
        lock.packages.insert(
            "mylib".to_string(),
            LockedPackage {
                version: "1.0.0".to_string(),
                source: "local".to_string(),
                checksum: "abc123".to_string(),
            },
        );
        lock.save(&path).unwrap();

        let loaded = LockFile::load(&path).unwrap();
        assert_eq!(loaded.packages["mylib"].version, "1.0.0");
        assert_eq!(loaded.packages["mylib"].checksum, "abc123");
    }

    #[test]
    fn test_build_multi_unit_project() {
        let dir = tempfile::tempdir().unwrap();
        BuildSystem::init(dir.path(), "multi").unwrap();

        let project = dir.path().join("multi");
        let src = project.join("src");

        // Add a unit
        fs::write(
            src.join("mathutils.pas"),
            "program MathUtils;\nfunction Add(a, b: integer): integer;\nbegin\n  Add := a + b;\nend;\nbegin\nend.\n",
        ).unwrap();

        // Main uses it (conceptually)
        fs::write(
            src.join("multi.pas"),
            "program Multi;\nbegin\n  writeln('Multi project');\nend.\n",
        ).unwrap();

        let bs = BuildSystem::open(&project, false).unwrap();
        bs.build().unwrap();
    }

    #[test]
    fn test_run_project() {
        let dir = tempfile::tempdir().unwrap();
        BuildSystem::init(dir.path(), "runtest").unwrap();

        let project = dir.path().join("runtest");
        let bs = BuildSystem::open(&project, false).unwrap();
        bs.run().unwrap();
    }
}
