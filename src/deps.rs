//! Package dependency resolution: path, git, registry, and dependency graphs.

use crate::build_system::{DependencySpec, Manifest};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::process::Command;

/// Resolved dependency with on-disk location
#[derive(Debug, Clone)]
pub struct ResolvedDependency {
    pub name: String,
    pub version: String,
    pub source: DependencySource,
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencySource {
    Path,
    Git { url: String, branch: Option<String> },
    Registry { url: String },
}

/// Node in a dependency graph
#[derive(Debug, Clone, Default)]
pub struct DepGraphNode {
    pub name: String,
    pub source: String,
    pub children: Vec<DepGraphNode>,
}

/// Resolver for pascal.toml dependencies
pub struct DependencyResolver {
    project_root: PathBuf,
    cache_dir: PathBuf,
    registry_url: Option<String>,
    verbose: bool,
}

impl DependencyResolver {
    pub fn new(project_root: &Path, verbose: bool) -> Self {
        let cache_dir = project_root.join(".pascal");
        let registry_url = std::env::var("PASCAL_REGISTRY").ok().filter(|s| !s.is_empty());
        Self {
            project_root: project_root.to_path_buf(),
            cache_dir,
            registry_url,
            verbose,
        }
    }

    /// Resolve all dependencies; returns search paths for unit loading.
    pub fn resolve_all(&self, manifest: &Manifest) -> Result<Vec<ResolvedDependency>> {
        std::fs::create_dir_all(self.cache_dir.join("deps"))?;
        std::fs::create_dir_all(self.cache_dir.join("registry"))?;

        let mut resolved = Vec::new();
        for (name, spec) in &manifest.dependencies {
            let dep = self.resolve_one(name, spec)?;
            resolved.push(dep);
        }
        Ok(resolved)
    }

    fn resolve_one(&self, name: &str, spec: &DependencySpec) -> Result<ResolvedDependency> {
        match spec {
            DependencySpec::Version(ver) => self.resolve_registry(name, ver),
            DependencySpec::Detailed(detail) => {
                if let Some(path) = &detail.path {
                    return self.resolve_path(name, path, detail.version.as_deref());
                }
                if let Some(git_url) = &detail.git {
                    return self.resolve_git(name, git_url, detail.branch.as_deref(), detail.version.as_deref());
                }
                if let Some(ver) = &detail.version {
                    return self.resolve_registry(name, ver);
                }
                Err(anyhow!("Dependency '{}' has no path, git, or version", name))
            }
        }
    }

    fn resolve_path(&self, name: &str, rel_path: &str, version: Option<&str>) -> Result<ResolvedDependency> {
        let abs = self.project_root.join(rel_path);
        if !abs.exists() {
            return Err(anyhow!("Dependency '{}' path not found: {}", name, abs.display()));
        }
        if self.verbose {
            eprintln!("  resolved {} from path {}", name, abs.display());
        }
        Ok(ResolvedDependency {
            name: name.to_string(),
            version: version.unwrap_or("*").to_string(),
            source: DependencySource::Path,
            path: abs,
        })
    }

    fn resolve_git(
        &self,
        name: &str,
        url: &str,
        branch: Option<&str>,
        version: Option<&str>,
    ) -> Result<ResolvedDependency> {
        let target = self.cache_dir.join("deps").join(name);
        if target.exists() {
            self.git_update(&target, branch)?;
        } else {
            self.git_clone(url, &target, branch)?;
        }
        let src = self.find_src_dir(&target);
        if self.verbose {
            eprintln!("  resolved {} from git {} -> {}", name, url, src.display());
        }
        Ok(ResolvedDependency {
            name: name.to_string(),
            version: version.unwrap_or("*").to_string(),
            source: DependencySource::Git {
                url: url.to_string(),
                branch: branch.map(String::from),
            },
            path: src,
        })
    }

    fn resolve_registry(&self, name: &str, version: &str) -> Result<ResolvedDependency> {
        let cache_path = self.cache_dir.join("registry").join(format!("{}-{}", name, version));
        if cache_path.exists() {
            let src = self.find_src_dir(&cache_path);
            return Ok(ResolvedDependency {
                name: name.to_string(),
                version: version.to_string(),
                source: DependencySource::Registry {
                    url: self.registry_url.clone().unwrap_or_else(|| "local".to_string()),
                },
                path: src,
            });
        }

        if let Some(ref registry) = self.registry_url {
            self.fetch_from_registry(name, version, registry)?;
            let src = self.find_src_dir(&cache_path);
            return Ok(ResolvedDependency {
                name: name.to_string(),
                version: version.to_string(),
                source: DependencySource::Registry {
                    url: registry.clone(),
                },
                path: src,
            });
        }

        Err(anyhow!(
            "Registry dependency '{name} = \"{version}\" not cached.\n\
             Set PASCAL_REGISTRY to a registry URL, or vendor to .pascal/registry/{name}-{version}/"
        ))
    }

    fn fetch_from_registry(&self, name: &str, version: &str, registry: &str) -> Result<()> {
        let cache_path = self.cache_dir.join("registry").join(format!("{}-{}", name, version));
        if cache_path.exists() {
            return Ok(());
        }

        // Local registry index: .pascal/registry-index/{name}/{version}.json
        let index_path = self
            .cache_dir
            .join("registry-index")
            .join(name)
            .join(format!("{version}.json"));
        if index_path.exists() {
            let content = std::fs::read_to_string(&index_path)?;
            let manifest: RegistryManifest = serde_json::from_str(&content)?;
            std::fs::create_dir_all(&cache_path)?;
            for (file, content_b64) in manifest.files {
                let bytes = base64_decode(&content_b64)?;
                let out = cache_path.join(&file);
                if let Some(parent) = out.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::write(out, bytes)?;
            }
            return Ok(());
        }

        Err(anyhow!(
            "Package {name}@{version} not found.\n\
             Vendor to .pascal/registry/{name}-{version}/ or add registry index at {}\n\
             (PASCAL_REGISTRY={registry})",
            index_path.display()
        ))
    }

    fn git_clone(&self, url: &str, target: &Path, branch: Option<&str>) -> Result<()> {
        let status = Command::new("git")
            .args(["clone", "--depth", "1", url, target.to_str().unwrap()])
            .status()
            .context("git clone failed (is git installed?)")?;
        if !status.success() {
            return Err(anyhow!("git clone failed for {url}"));
        }
        if let Some(b) = branch {
            let status = Command::new("git")
                .args(["checkout", b])
                .current_dir(target)
                .status()?;
            if !status.success() {
                return Err(anyhow!("git checkout {b} failed"));
            }
        }
        Ok(())
    }

    fn git_update(&self, target: &Path, branch: Option<&str>) -> Result<()> {
        let _ = Command::new("git").args(["fetch", "--depth", "1"]).current_dir(target).status();
        if let Some(b) = branch {
            let status = Command::new("git")
                .args(["checkout", b])
                .current_dir(target)
                .status()?;
            if !status.success() {
                return Err(anyhow!("git checkout {b} failed"));
            }
        }
        let _ = Command::new("git").args(["pull", "--ff-only"]).current_dir(target).status();
        Ok(())
    }

    fn find_src_dir(&self, root: &Path) -> PathBuf {
        for candidate in ["src", ""] {
            let p = if candidate.is_empty() {
                root.to_path_buf()
            } else {
                root.join(candidate)
            };
            if p.exists() {
                return p;
            }
        }
        root.to_path_buf()
    }

    /// Build dependency graph for display/analysis.
    pub fn dependency_graph(manifest: &Manifest) -> DepGraphNode {
        let mut root = DepGraphNode {
            name: manifest.package.name.clone(),
            source: "project".to_string(),
            children: Vec::new(),
        };
        for (name, spec) in &manifest.dependencies {
            root.children.push(DepGraphNode {
                name: name.clone(),
                source: spec_source_label(spec),
                children: Vec::new(),
            });
        }
        root
    }

    /// Format dependency tree as text.
    pub fn format_tree(node: &DepGraphNode, prefix: &str) -> String {
        let mut out = format!("{}{}\n", prefix, node.name);
        for (i, child) in node.children.iter().enumerate() {
            let is_last = i + 1 == node.children.len();
            let branch = if is_last { "└── " } else { "├── " };
            let child_prefix = format!("{prefix}{}", if is_last { "    " } else { "│   " });
            out.push_str(&format!("{prefix}{branch}{} ({})", child.name, child.source));
            if !child.children.is_empty() {
                out.push('\n');
                out.push_str(&Self::format_tree(child, &child_prefix));
            } else {
                out.push('\n');
            }
        }
        out
    }

    /// Search paths from resolved dependencies.
    pub fn search_paths(deps: &[ResolvedDependency]) -> Vec<PathBuf> {
        deps.iter().map(|d| d.path.clone()).collect()
    }
}

fn spec_source_label(spec: &DependencySpec) -> String {
    match spec {
        DependencySpec::Version(v) => format!("registry:{v}"),
        DependencySpec::Detailed(d) => {
            if let Some(p) = &d.path {
                format!("path:{p}")
            } else if let Some(g) = &d.git {
                format!("git:{g}")
            } else {
                format!("registry:{}", d.version.as_deref().unwrap_or("*"))
            }
        }
    }
}

#[derive(Debug, Deserialize)]
struct RegistryManifest {
    files: HashMap<String, String>,
}

fn base64_decode(input: &str) -> Result<Vec<u8>> {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = Vec::new();
    let mut buf = 0u32;
    let mut bits = 0u32;
    for b in input.bytes().filter(|&c| c != b'=' && !c.is_ascii_whitespace()) {
        let val = TABLE.iter().position(|&t| t == b).ok_or_else(|| anyhow!("invalid base64"))? as u32;
        buf = (buf << 6) | val;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((buf >> bits) as u8);
            buf &= (1 << bits) - 1;
        }
    }
    Ok(out)
}

/// Semver-compatible version comparison (major.minor.patch).
pub fn version_satisfies(requested: &str, available: &str) -> bool {
    if requested == "*" || requested == available {
        return true;
    }
    parse_version(requested) <= parse_version(available)
}

fn parse_version(v: &str) -> (u64, u64, u64) {
    let mut parts = v.split('.');
    (
        parts.next().and_then(|p| p.parse().ok()).unwrap_or(0),
        parts.next().and_then(|p| p.parse().ok()).unwrap_or(0),
        parts.next().and_then(|p| p.parse().ok()).unwrap_or(0),
    )
}

/// Dependency version compatibility matrix entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompatibilityEntry {
    pub name: String,
    pub versions: BTreeMap<String, String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build_system::{DetailedDependency, Package};

    #[test]
    fn test_version_satisfies() {
        assert!(version_satisfies("1.0", "1.0.0"));
        assert!(version_satisfies("*", "2.1.3"));
        assert!(version_satisfies("1.0.0", "1.2.0"));
    }

    #[test]
    fn test_dependency_graph() {
        let manifest = Manifest {
            package: Package {
                name: "app".to_string(),
                version: "1.0".to_string(),
                description: String::new(),
                authors: vec![],
                license: "MIT".to_string(),
                src: "src".to_string(),
                main: None,
            },
            dependencies: BTreeMap::from([
                ("utils".to_string(), DependencySpec::Version("1.0".to_string())),
            ]),
            build: Default::default(),
            profile: BTreeMap::new(),
            features: BTreeMap::new(),
        };
        let graph = DependencyResolver::dependency_graph(&manifest);
        assert_eq!(graph.children.len(), 1);
    }

    #[test]
    fn test_resolve_path_dependency() {
        let dir = tempfile::tempdir().unwrap();
        let dep_dir = dir.path().join("lib");
        std::fs::create_dir_all(&dep_dir).unwrap();
        let manifest = Manifest {
            package: Package {
                name: "app".to_string(),
                version: "1.0".to_string(),
                description: String::new(),
                authors: vec![],
                license: "MIT".to_string(),
                src: "src".to_string(),
                main: None,
            },
            dependencies: BTreeMap::from([(
                "lib".to_string(),
                DependencySpec::Detailed(DetailedDependency {
                    version: None,
                    path: Some("lib".to_string()),
                    git: None,
                    branch: None,
                }),
            )]),
            build: Default::default(),
            profile: BTreeMap::new(),
            features: BTreeMap::new(),
        };
        let resolver = DependencyResolver::new(dir.path(), false);
        let resolved = resolver.resolve_all(&manifest).unwrap();
        assert_eq!(resolved.len(), 1);
        assert!(resolved[0].path.exists());
    }
}
