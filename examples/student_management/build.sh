#!/bin/bash
# Build script for Student Management System

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$PROJECT_ROOT/target/release/pascal"
PROJECT_DIR="projects/student_management"

echo "========================================"
echo "Student Management System - Build Script"
echo "========================================"
echo ""

# Check if compiler exists
if [ ! -f "$COMPILER" ]; then
    echo "Compiler not found. Building..."
    cd "$PROJECT_ROOT"
    cargo build --release
    echo ""
fi

# Get command line arguments
MODE=${1:-compile}
VERBOSE=${2:-false}

case $MODE in
    compile)
        echo "Compiling Student Management System..."
        echo ""
        cd "$PROJECT_ROOT"
        if [ "$VERBOSE" = "true" ]; then
            "$COMPILER" compile "$PROJECT_DIR/main.pas" -v
        else
            "$COMPILER" compile "$PROJECT_DIR/main.pas"
        fi
        ;;

    assembly)
        echo "Compiling to Assembly..."
        echo ""
        cd "$PROJECT_ROOT"
        "$COMPILER" compile "$PROJECT_DIR/main.pas" -S -v
        ;;

    optimized)
        echo "Compiling with Optimization (-O2)..."
        echo ""
        cd "$PROJECT_ROOT"
        "$COMPILER" compile "$PROJECT_DIR/main.pas" -O2 -v
        ;;

    clean)
        echo "Cleaning build artifacts..."
        cd "$PROJECT_ROOT"
        rm -f "$PROJECT_DIR"/*.o "$PROJECT_DIR"/*.s "$PROJECT_DIR"/*.ppu
        echo "Clean complete."
        ;;

    *)
        echo "Usage: $0 {compile|assembly|optimized|clean} [verbose]"
        echo ""
        echo "Examples:"
        echo "  $0 compile         - Compile normally"
        echo "  $0 compile true    - Compile with verbose output"
        echo "  $0 assembly        - Generate assembly output"
        echo "  $0 optimized       - Compile with optimizations"
        echo "  $0 clean           - Clean build artifacts"
        exit 1
        ;;
esac

echo ""
echo "========================================"
echo "Build Complete!"
echo "========================================"
