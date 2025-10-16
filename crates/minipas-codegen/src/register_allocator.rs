//! Register allocation using graph coloring
//! 
//! Implements live range analysis, interference graph construction, and register allocation

use std::collections::{HashMap, HashSet};
use anyhow::{Result, anyhow};

/// Register representation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    // General purpose registers (x86-64)
    RAX, RBX, RCX, RDX,
    RSI, RDI, RBP, RSP,
    R8, R9, R10, R11,
    R12, R13, R14, R15,
    
    // Virtual registers (before allocation)
    Virtual(u32),
}

impl Register {
    /// Get all available registers for allocation
    pub fn available_registers() -> Vec<Register> {
        vec![
            Register::RAX, Register::RBX, Register::RCX, Register::RDX,
            Register::RSI, Register::RDI,
            Register::R8, Register::R9, Register::R10, Register::R11,
            Register::R12, Register::R13, Register::R14, Register::R15,
        ]
    }
    
    /// Check if register is callee-saved
    pub fn is_callee_saved(&self) -> bool {
        matches!(self, 
            Register::RBX | Register::R12 | Register::R13 | 
            Register::R14 | Register::R15 | Register::RBP
        )
    }
    
    /// Get register name
    pub fn name(&self) -> String {
        match self {
            Register::RAX => "rax".to_string(),
            Register::RBX => "rbx".to_string(),
            Register::RCX => "rcx".to_string(),
            Register::RDX => "rdx".to_string(),
            Register::RSI => "rsi".to_string(),
            Register::RDI => "rdi".to_string(),
            Register::RBP => "rbp".to_string(),
            Register::RSP => "rsp".to_string(),
            Register::R8 => "r8".to_string(),
            Register::R9 => "r9".to_string(),
            Register::R10 => "r10".to_string(),
            Register::R11 => "r11".to_string(),
            Register::R12 => "r12".to_string(),
            Register::R13 => "r13".to_string(),
            Register::R14 => "r14".to_string(),
            Register::R15 => "r15".to_string(),
            Register::Virtual(n) => format!("v{}", n),
        }
    }
}

/// Live range for a variable
#[derive(Debug, Clone)]
pub struct LiveRange {
    pub variable: String,
    pub start: usize,
    pub end: usize,
    pub register: Option<Register>,
    pub spilled: bool,
    pub spill_slot: Option<i32>,
}

impl LiveRange {
    /// Check if two live ranges interfere
    pub fn interferes_with(&self, other: &LiveRange) -> bool {
        !(self.end < other.start || other.end < self.start)
    }
}

/// Interference graph for register allocation
#[derive(Debug)]
pub struct InterferenceGraph {
    nodes: HashMap<String, HashSet<String>>,
    live_ranges: HashMap<String, LiveRange>,
}

impl InterferenceGraph {
    /// Create a new interference graph
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            live_ranges: HashMap::new(),
        }
    }
    
    /// Add a variable to the graph
    pub fn add_variable(&mut self, var: String, live_range: LiveRange) {
        self.nodes.entry(var.clone()).or_insert_with(HashSet::new);
        self.live_ranges.insert(var, live_range);
    }
    
    /// Add an interference edge between two variables
    pub fn add_interference(&mut self, var1: &str, var2: &str) {
        if var1 != var2 {
            self.nodes.entry(var1.to_string())
                .or_insert_with(HashSet::new)
                .insert(var2.to_string());
            self.nodes.entry(var2.to_string())
                .or_insert_with(HashSet::new)
                .insert(var1.to_string());
        }
    }
    
    /// Get degree of a node (number of interferences)
    pub fn degree(&self, var: &str) -> usize {
        self.nodes.get(var).map(|s| s.len()).unwrap_or(0)
    }
    
    /// Get neighbors of a node
    pub fn neighbors(&self, var: &str) -> Vec<String> {
        self.nodes.get(var)
            .map(|s| s.iter().cloned().collect())
            .unwrap_or_default()
    }
    
    /// Remove a node from the graph
    pub fn remove_node(&mut self, var: &str) {
        if let Some(neighbors) = self.nodes.remove(var) {
            for neighbor in neighbors {
                if let Some(neighbor_set) = self.nodes.get_mut(&neighbor) {
                    neighbor_set.remove(var);
                }
            }
        }
    }
}

impl Default for InterferenceGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Register allocator using graph coloring
pub struct RegisterAllocator {
    interference_graph: InterferenceGraph,
    allocation: HashMap<String, Register>,
    spilled_vars: HashSet<String>,
    next_spill_slot: i32,
    num_colors: usize,
}

impl RegisterAllocator {
    /// Create a new register allocator
    pub fn new() -> Self {
        Self {
            interference_graph: InterferenceGraph::new(),
            allocation: HashMap::new(),
            spilled_vars: HashSet::new(),
            next_spill_slot: 0,
            num_colors: Register::available_registers().len(),
        }
    }
    
    /// Build interference graph from live ranges
    pub fn build_interference_graph(&mut self, live_ranges: Vec<LiveRange>) {
        // Add all variables
        for range in &live_ranges {
            self.interference_graph.add_variable(range.variable.clone(), range.clone());
        }
        
        // Add interference edges
        for i in 0..live_ranges.len() {
            for j in (i + 1)..live_ranges.len() {
                if live_ranges[i].interferes_with(&live_ranges[j]) {
                    self.interference_graph.add_interference(
                        &live_ranges[i].variable,
                        &live_ranges[j].variable,
                    );
                }
            }
        }
    }
    
    /// Allocate registers using graph coloring
    pub fn allocate(&mut self) -> Result<()> {
        let mut stack = Vec::new();
        let mut graph = self.interference_graph.nodes.clone();
        
        // Simplify: remove nodes with degree < k
        while !graph.is_empty() {
            // Find a node with degree < num_colors
            if let Some(var) = graph.iter()
                .find(|(_, neighbors)| neighbors.len() < self.num_colors)
                .map(|(v, _)| v.clone())
            {
                stack.push(var.clone());
                
                // Remove from graph
                if let Some(neighbors) = graph.remove(&var) {
                    for neighbor in neighbors {
                        if let Some(neighbor_set) = graph.get_mut(&neighbor) {
                            neighbor_set.remove(&var);
                        }
                    }
                }
            } else {
                // Spill: pick node with highest degree
                if let Some(var) = graph.iter()
                    .max_by_key(|(_, neighbors)| neighbors.len())
                    .map(|(v, _)| v.clone())
                {
                    self.spilled_vars.insert(var.clone());
                    stack.push(var.clone());
                    
                    // Remove from graph
                    if let Some(neighbors) = graph.remove(&var) {
                        for neighbor in neighbors {
                            if let Some(neighbor_set) = graph.get_mut(&neighbor) {
                                neighbor_set.remove(&var);
                            }
                        }
                    }
                } else {
                    break;
                }
            }
        }
        
        // Select: assign colors (registers)
        let available_regs = Register::available_registers();
        
        while let Some(var) = stack.pop() {
            if self.spilled_vars.contains(&var) {
                // Assign spill slot
                self.next_spill_slot += 8;
                continue;
            }
            
            // Get colors used by neighbors
            let mut used_colors = HashSet::new();
            for neighbor in self.interference_graph.neighbors(&var) {
                if let Some(reg) = self.allocation.get(&neighbor) {
                    used_colors.insert(*reg);
                }
            }
            
            // Find available color
            if let Some(reg) = available_regs.iter()
                .find(|r| !used_colors.contains(r))
            {
                self.allocation.insert(var, *reg);
            } else {
                // Need to spill
                self.spilled_vars.insert(var);
                self.next_spill_slot += 8;
            }
        }
        
        Ok(())
    }
    
    /// Get allocated register for a variable
    pub fn get_register(&self, var: &str) -> Option<Register> {
        self.allocation.get(var).copied()
    }
    
    /// Check if variable was spilled
    pub fn is_spilled(&self, var: &str) -> bool {
        self.spilled_vars.contains(var)
    }
    
    /// Get spill slot offset
    pub fn get_spill_slot(&self, var: &str) -> Option<i32> {
        if self.is_spilled(var) {
            Some(self.next_spill_slot)
        } else {
            None
        }
    }
    
    /// Get all allocated registers
    pub fn get_allocations(&self) -> &HashMap<String, Register> {
        &self.allocation
    }
    
    /// Get callee-saved registers that need to be preserved
    pub fn get_callee_saved_registers(&self) -> Vec<Register> {
        self.allocation.values()
            .filter(|r| r.is_callee_saved())
            .copied()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect()
    }
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

/// Live range analyzer
pub struct LiveRangeAnalyzer {
    live_ranges: HashMap<String, LiveRange>,
    current_position: usize,
}

impl LiveRangeAnalyzer {
    /// Create a new live range analyzer
    pub fn new() -> Self {
        Self {
            live_ranges: HashMap::new(),
            current_position: 0,
        }
    }
    
    /// Mark variable as used at current position
    pub fn use_variable(&mut self, var: &str) {
        self.live_ranges.entry(var.to_string())
            .and_modify(|range| {
                range.end = self.current_position;
            })
            .or_insert_with(|| LiveRange {
                variable: var.to_string(),
                start: self.current_position,
                end: self.current_position,
                register: None,
                spilled: false,
                spill_slot: None,
            });
    }
    
    /// Mark variable as defined at current position
    pub fn define_variable(&mut self, var: &str) {
        self.live_ranges.entry(var.to_string())
            .and_modify(|range| {
                range.end = self.current_position;
            })
            .or_insert_with(|| LiveRange {
                variable: var.to_string(),
                start: self.current_position,
                end: self.current_position,
                register: None,
                spilled: false,
                spill_slot: None,
            });
    }
    
    /// Advance to next instruction
    pub fn next_instruction(&mut self) {
        self.current_position += 1;
    }
    
    /// Get all live ranges
    pub fn get_live_ranges(&self) -> Vec<LiveRange> {
        self.live_ranges.values().cloned().collect()
    }
}

impl Default for LiveRangeAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_live_range_interference() {
        let range1 = LiveRange {
            variable: "x".to_string(),
            start: 0,
            end: 5,
            register: None,
            spilled: false,
            spill_slot: None,
        };
        
        let range2 = LiveRange {
            variable: "y".to_string(),
            start: 3,
            end: 8,
            register: None,
            spilled: false,
            spill_slot: None,
        };
        
        let range3 = LiveRange {
            variable: "z".to_string(),
            start: 6,
            end: 10,
            register: None,
            spilled: false,
            spill_slot: None,
        };
        
        assert!(range1.interferes_with(&range2));
        assert!(!range1.interferes_with(&range3));
        assert!(range2.interferes_with(&range3));
    }
    
    #[test]
    fn test_register_allocation() {
        let mut allocator = RegisterAllocator::new();
        
        let ranges = vec![
            LiveRange {
                variable: "x".to_string(),
                start: 0,
                end: 2,
                register: None,
                spilled: false,
                spill_slot: None,
            },
            LiveRange {
                variable: "y".to_string(),
                start: 3,
                end: 5,
                register: None,
                spilled: false,
                spill_slot: None,
            },
        ];
        
        allocator.build_interference_graph(ranges);
        allocator.allocate().unwrap();
        
        // x and y don't interfere, so both should get registers
        assert!(allocator.get_register("x").is_some());
        assert!(allocator.get_register("y").is_some());
    }
    
    #[test]
    fn test_live_range_analyzer() {
        let mut analyzer = LiveRangeAnalyzer::new();
        
        analyzer.define_variable("x");
        analyzer.next_instruction();
        analyzer.use_variable("x");
        analyzer.next_instruction();
        analyzer.use_variable("x");
        
        let ranges = analyzer.get_live_ranges();
        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].variable, "x");
        assert_eq!(ranges[0].start, 0);
        assert_eq!(ranges[0].end, 2);
    }
}
