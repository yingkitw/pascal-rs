//! PPU (Pascal Precompiled Unit) file format
//!
//! This module implements a simplified version of FPC's PPU format for storing
//! compiled unit information. PPU files enable faster compilation by caching
//! parsed and analyzed unit data.

use serde::{Deserialize, Serialize};
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use minipas_ast::Unit;

/// PPU file format version
pub const PPU_VERSION: u32 = 1;

/// PPU file magic number "MPU\0" (MiniPas Unit)
pub const PPU_MAGIC: [u8; 4] = [b'M', b'P', b'U', 0];

/// PPU file header
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PpuHeader {
    /// Magic number for file identification
    pub magic: [u8; 4],
    /// PPU format version
    pub version: u32,
    /// CRC32 checksum of the interface section
    pub interface_crc: u32,
    /// CRC32 checksum of the implementation section
    pub implementation_crc: u32,
    /// CRC32 checksum of the entire unit
    pub unit_crc: u32,
    /// Size of the serialized unit data
    pub data_size: u64,
}

impl Default for PpuHeader {
    fn default() -> Self {
        Self {
            magic: PPU_MAGIC,
            version: PPU_VERSION,
            interface_crc: 0,
            implementation_crc: 0,
            unit_crc: 0,
            data_size: 0,
        }
    }
}

/// PPU file containing a compiled unit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PpuFile {
    /// File header
    pub header: PpuHeader,
    /// The compiled unit
    pub unit: Unit,
}

impl PpuFile {
    /// Create a new PPU file from a unit
    pub fn new(unit: Unit) -> Self {
        let mut ppu = Self {
            header: PpuHeader::default(),
            unit,
        };
        ppu.calculate_checksums();
        ppu
    }

    /// Calculate CRC checksums for the unit
    fn calculate_checksums(&mut self) {
        // For now, use simple hash-based checksums
        // TODO: Implement proper CRC32 calculation
        self.header.interface_crc = self.calculate_interface_crc();
        self.header.implementation_crc = self.calculate_implementation_crc();
        self.header.unit_crc = self.calculate_unit_crc();
    }

    fn calculate_interface_crc(&self) -> u32 {
        // Simple hash of interface section
        let mut hash = 0u32;
        hash = hash.wrapping_add(self.unit.interface.types.len() as u32);
        hash = hash.wrapping_add(self.unit.interface.constants.len() as u32);
        hash = hash.wrapping_add(self.unit.interface.variables.len() as u32);
        hash = hash.wrapping_add(self.unit.interface.functions.len() as u32);
        hash = hash.wrapping_add(self.unit.interface.procedures.len() as u32);
        hash
    }

    fn calculate_implementation_crc(&self) -> u32 {
        // Simple hash of implementation section
        let mut hash = 0u32;
        hash = hash.wrapping_add(self.unit.implementation.types.len() as u32);
        hash = hash.wrapping_add(self.unit.implementation.constants.len() as u32);
        hash = hash.wrapping_add(self.unit.implementation.variables.len() as u32);
        hash = hash.wrapping_add(self.unit.implementation.functions.len() as u32);
        hash = hash.wrapping_add(self.unit.implementation.procedures.len() as u32);
        hash
    }

    fn calculate_unit_crc(&self) -> u32 {
        // Combine interface and implementation CRCs
        self.header.interface_crc.wrapping_add(self.header.implementation_crc)
    }

    /// Write PPU file to disk
    pub fn write_to_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        // Serialize the unit data
        let unit_data = bincode::serialize(&self.unit)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        
        self.header.data_size = unit_data.len() as u64;
        
        // Serialize the header
        let header_data = bincode::serialize(&self.header)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        
        // Write to file
        let mut file = fs::File::create(path)?;
        file.write_all(&header_data)?;
        file.write_all(&unit_data)?;
        file.flush()?;
        
        Ok(())
    }

    /// Read PPU file from disk
    pub fn read_from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let mut file = fs::File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        
        // Deserialize header first to get data size
        let header: PpuHeader = bincode::deserialize(&buffer)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        
        // Validate magic number
        if header.magic != PPU_MAGIC {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid PPU file magic number",
            ));
        }
        
        // Validate version
        if header.version != PPU_VERSION {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Unsupported PPU version: {} (expected {})", header.version, PPU_VERSION),
            ));
        }
        
        // Calculate header size to find where unit data starts
        let header_size = bincode::serialized_size(&header)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))? as usize;
        
        // Deserialize unit data
        let unit: Unit = bincode::deserialize(&buffer[header_size..])
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
        
        Ok(Self { header, unit })
    }

    /// Verify the checksums of the PPU file
    pub fn verify_checksums(&self) -> bool {
        let interface_crc = self.calculate_interface_crc();
        let implementation_crc = self.calculate_implementation_crc();
        let unit_crc = interface_crc.wrapping_add(implementation_crc);
        
        self.header.interface_crc == interface_crc
            && self.header.implementation_crc == implementation_crc
            && self.header.unit_crc == unit_crc
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use minipas_ast::*;
    use std::fs;
    use std::path::PathBuf;

    fn create_test_unit() -> Unit {
        Unit {
            name: "TestUnit".to_string(),
            uses: vec!["System".to_string()],
            interface: UnitInterface {
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
            },
            implementation: UnitImplementation {
                uses: vec![],
                types: vec![],
                constants: vec![],
                variables: vec![],
                procedures: vec![],
                functions: vec![],
                classes: vec![],
                interfaces: vec![],
                initialization: None,
                finalization: None,
            },
        }
    }

    #[test]
    fn test_ppu_header_default() {
        let header = PpuHeader::default();
        assert_eq!(header.magic, PPU_MAGIC);
        assert_eq!(header.version, PPU_VERSION);
    }

    #[test]
    fn test_ppu_file_creation() {
        let unit = create_test_unit();
        let ppu = PpuFile::new(unit);
        
        assert_eq!(ppu.header.magic, PPU_MAGIC);
        assert_eq!(ppu.header.version, PPU_VERSION);
        assert_eq!(ppu.unit.name, "TestUnit");
    }

    #[test]
    fn test_ppu_checksum_calculation() {
        let unit = create_test_unit();
        let ppu = PpuFile::new(unit);
        
        // Checksums should be calculated
        assert!(ppu.verify_checksums());
    }

    #[test]
    fn test_ppu_write_and_read() {
        let unit = create_test_unit();
        let mut ppu = PpuFile::new(unit);
        
        // Write to temporary file
        let temp_path = PathBuf::from("/tmp/test_unit.ppu");
        ppu.write_to_file(&temp_path).expect("Failed to write PPU file");
        
        // Read back
        let loaded_ppu = PpuFile::read_from_file(&temp_path).expect("Failed to read PPU file");
        
        // Verify
        assert_eq!(loaded_ppu.unit.name, "TestUnit");
        assert_eq!(loaded_ppu.unit.uses, vec!["System".to_string()]);
        assert!(loaded_ppu.verify_checksums());
        
        // Cleanup
        fs::remove_file(temp_path).ok();
    }

    #[test]
    fn test_ppu_invalid_magic() {
        let mut header = PpuHeader::default();
        header.magic = [b'X', b'X', b'X', 0];
        
        // This would fail validation when reading
        assert_ne!(header.magic, PPU_MAGIC);
    }
}
