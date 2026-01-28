#!/bin/bash

set -e

echo "=== VKS Compiler Build Script ==="
echo ""

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

echo "Step 1: Checking dependencies..."
if ! command_exists cargo; then
    echo -e "${RED}Error: Rust/Cargo is not installed.${NC}"
    exit 1
fi
echo -e "${GREEN}✓ cargo found${NC}"
echo ""

echo "Step 2: Building VKS Unified Compiler..."
cd compiler
cargo build --release
cd ..
echo -e "${GREEN}✓ Compiler built successfully${NC}"
echo ""

echo "Step 3: Copying binaries to release directory..."
mkdir -p release/bin
if [ -f "compiler/target/release/vks" ]; then
    cp compiler/target/release/vks release/bin/
    echo -e "${GREEN}✓ Copied unified vks binary${NC}"
else
    echo -e "${RED}Error: Could not find VKS unified binary${NC}"
    exit 1
fi

echo ""
echo -e "${GREEN}=== Build Complete! ===${NC}"
echo "Binary location: release/bin/vks"
echo ""
echo "To test the compiler:"
echo "  ./release/bin/vks build examples/hello.vks"
echo "  ./outbin"
