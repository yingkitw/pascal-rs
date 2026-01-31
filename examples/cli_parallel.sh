#!/bin/bash
# Example: Using parallel compilation via CLI

echo "=== Pascal Compiler - CLI Parallel Compilation Examples ==="
echo ""

# Example 1: Basic parallel compilation
echo "1. Basic parallel compilation (auto-detect threads):"
pascal compile program.pas -j --verbose

echo ""
echo "2. Parallel compilation with specific thread count:"
pascal compile program.pas -j --threads 4 --verbose

echo ""
echo "3. Parallel compilation with optimization:"
pascal compile program.pas -j --threads 8 -O 2 --verbose

echo ""
echo "4. Parallel compilation with multiple search paths:"
pascal compile program.pas -j \
    -I ./stdlib \
    -I ./lib \
    --threads 4 \
    --verbose

echo ""
echo "5. Parallel compilation with assembly output:"
pascal compile program.pas -j --threads 4 -S --verbose

echo ""
echo "6. Sequential compilation (no parallel):"
pascal compile program.pas --verbose

echo ""
echo "7. Get compiler info:"
pascal info output/program.ppu

echo ""
echo "8. Clean compiled files:"
pascal clean ./output
