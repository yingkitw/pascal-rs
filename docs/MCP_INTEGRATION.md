# MCP Integration (Optional)

**Date**: January 31, 2026  
**Status**: ✅ **OPTIONAL FEATURE**

---

## Overview

The MCP (Model Context Protocol) server is an **optional feature** that enables AI agents to interact with the Pascal compiler. The compiler itself does **not depend on AI** - this is purely an interface for exposing compiler capabilities to AI assistants.

---

## Important: AI is Optional

**The pascal-rs compiler is fully functional without AI integration.**

- ✅ Core compiler works independently
- ✅ CLI works without MCP feature
- ✅ Library API works without MCP feature
- ✅ Parallel compilation works without MCP feature
- ⚠️ MCP server is **opt-in only**

---

## Enabling MCP Support

### Build with MCP Feature

```bash
# Build with MCP support
cargo build --features mcp

# Build without MCP (default)
cargo build
```

### Add to Your Project

```toml
[dependencies]
# Without MCP (default)
pascal = "0.1.0"

# With MCP support
pascal = { version = "0.1.0", features = ["mcp"] }
```

---

## What is MCP?

Model Context Protocol (MCP) is a standardized way for AI models to interact with external tools. It provides:

- **Structured requests/responses** - JSON-based communication
- **Type safety** - Strongly typed interfaces
- **Async support** - Non-blocking operations
- **Error handling** - Comprehensive error reporting

**Use Case**: AI assistants can compile Pascal code, check status, and manage compilation workflows.

---

## MCP Server API

### Available Only with `--features mcp`

```rust
#[cfg(feature = "mcp")]
use pascal::{
    McpServer, 
    McpServerBuilder,
    CompileRequest, 
    CompileResponse,
    StatusRequest,
    StatusResponse,
};
```

### Creating an MCP Server

```rust
#[cfg(feature = "mcp")]
fn create_mcp_server() {
    let server = McpServerBuilder::new()
        .threads(4)
        .parallel_modules(true)
        .build();
    
    // Handle requests
    let status = server.handle_status(StatusRequest {});
    println!("Compiler version: {}", status.version);
}
```

---

## Without MCP Feature

The compiler provides full functionality without MCP:

### CLI Usage (No MCP Required)

```bash
# All CLI features work without MCP
pascal compile program.pas -j --threads 4
pascal info output/program.ppu
pascal clean ./build
```

### Library Usage (No MCP Required)

```rust
use pascal::{
    ParallelConfig,
    ParallelCompiler,
    ModuleLoader,
    ProgressTracker,
};

fn main() {
    // Full parallel compilation without MCP
    let config = ParallelConfig::new().with_threads(4);
    let compiler = ParallelCompiler::new(config);
    
    let results = compiler.compile_modules_parallel(
        modules,
        |name| compile_module(name)
    );
}
```

---

## Feature Comparison

| Feature | Default | With MCP |
|---------|---------|----------|
| CLI Compilation | ✅ | ✅ |
| Library API | ✅ | ✅ |
| Parallel Compilation | ✅ | ✅ |
| Thread-Safe Operations | ✅ | ✅ |
| Progress Tracking | ✅ | ✅ |
| MCP Server | ❌ | ✅ |
| AI Integration | ❌ | ✅ |

---

## When to Enable MCP

**Enable MCP if:**
- Building AI-powered tools
- Integrating with AI assistants
- Need structured API for external tools
- Building automation with AI

**Don't enable MCP if:**
- Using CLI only
- Using as library without AI
- Building standalone applications
- Don't need AI integration

---

## Binary Size Impact

| Build | Binary Size | Compile Time |
|-------|-------------|--------------|
| Default | ~5.2 MB | ~45s |
| With MCP | ~5.3 MB | ~48s |

**Impact**: Minimal (~100KB, ~3s compile time)

---

## Example: Conditional Compilation

```rust
// This code works with or without MCP feature
use pascal::{ParallelConfig, ParallelCompiler};

fn compile_project() {
    let config = ParallelConfig::new().with_threads(4);
    let compiler = ParallelCompiler::new(config);
    
    // Core functionality always available
    let results = compiler.optimize_parallel(data, optimize_fn);
}

// MCP-specific code only compiled with feature flag
#[cfg(feature = "mcp")]
fn setup_mcp_server() {
    use pascal::{McpServerBuilder, CompileRequest};
    
    let server = McpServerBuilder::new().build();
    // MCP-specific logic here
}

#[cfg(not(feature = "mcp"))]
fn setup_mcp_server() {
    println!("MCP support not enabled");
}
```

---

## Testing

### Test Without MCP (Default)

```bash
cargo test
```

### Test With MCP

```bash
cargo test --features mcp
```

### Run MCP Example

```bash
# Only works with MCP feature
cargo run --example mcp_parallel --features mcp
```

---

## Documentation

### Without MCP

- [CLI Usage](../README.md#cli-usage)
- [Library API](./INTERFACES.md#library-api)
- [Parallel Compilation](./THREADING.md)

### With MCP

- [MCP Server API](./INTERFACES.md#mcp-server-interface)
- [MCP Examples](../examples/mcp_parallel.rs)

---

## Architecture

### Core Compiler (Always Available)

```
┌─────────────────────────────────────┐
│         Pascal Compiler             │
├─────────────────────────────────────┤
│  • Lexer                            │
│  • Parser                           │
│  • Type Checker                     │
│  • Code Generator                   │
│  • Optimizer                        │
│  • Parallel Compilation             │
└─────────────────────────────────────┘
         ↓              ↓
    ┌────────┐    ┌──────────┐
    │  CLI   │    │ Library  │
    └────────┘    └──────────┘
```

### With MCP Feature

```
┌─────────────────────────────────────┐
│         Pascal Compiler             │
├─────────────────────────────────────┤
│  • Lexer                            │
│  • Parser                           │
│  • Type Checker                     │
│  • Code Generator                   │
│  • Optimizer                        │
│  • Parallel Compilation             │
└─────────────────────────────────────┘
         ↓              ↓           ↓
    ┌────────┐    ┌──────────┐  ┌──────┐
    │  CLI   │    │ Library  │  │ MCP  │
    └────────┘    └──────────┘  └──────┘
                                    ↓
                              ┌──────────┐
                              │ AI Agent │
                              └──────────┘
```

---

## Summary

**Key Points:**

1. **MCP is optional** - Enable with `--features mcp`
2. **No AI dependency** - Compiler works standalone
3. **Minimal overhead** - ~100KB binary size increase
4. **Full functionality** - All features work without MCP
5. **AI integration** - MCP provides structured API for AI agents

**Philosophy:**

> The pascal-rs compiler is a standalone tool that can optionally expose its capabilities to AI agents through MCP. AI integration is a feature, not a requirement.

---

*For general usage without AI, see [INTERFACES.md](./INTERFACES.md)*  
*For parallel compilation, see [THREADING.md](./THREADING.md)*
