# Mercury-RE Library

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Mercury language project implementing a regex library that provides Python re module functionality. The main module (`regex.m`) provides a Mercury interface to PCRE2 (Perl Compatible Regular Expressions 2) library.

## Directory Structure
```
mercury-re/
├── CLAUDE.md                   # This documentation file
├── Make.options               # Build configuration
├── Makefile                   # Main makefile
├── src/                       # Library source code
│   ├── Makefile              # Source build configuration
│   ├── Mercury.options       # Mercury compiler options
│   └── regex.m               # Main regex module
├── tests/                     # Test suite
│   ├── Makefile              # Test build configuration  
│   ├── Mercury.options       # Test compiler options
│   └── test_regex.m          # Test suite module
└── examples/                  # Example programs (empty)
```

## Build Commands

### Using Make (Recommended)
```bash
# Build the library
make

# Run tests
make runtests

# Clean build files
make realclean
```

### Manual Mercury Compilation
```bash
# Compile the regex module and test program
mmc --make test_regex \
    --clang-flags "`pkg-config --cflags libpcre2-8`" \
    --ld-flags "`pkg-config --libs libpcre2-8`"

# Run tests
./test_regex
```

## Library Files
After building, the library will be available as:
- Static library: `src/Mercury/hlc.gc/.../Mercury/lib/libregex.a`
- Shared library: `src/Mercury/hlc.gc/.../Mercury/lib/libregex.dylib`

## Installation
```bash
make install
```

This installs the library to `/usr/local/mercury` (configurable via INSTALL_PREFIX in Make.options).

## Code Architecture

### Main Components

1. **regex.m** - Core regex module providing:
   - Pattern compilation with flags (ignorecase, multiline, dotall, verbose, ascii, unicode)
   - Search operations: `search`, `match`, `fullmatch`, `findall`, `finditer`
   - Substitution operations: `sub`, `subn`
   - String splitting: `split`
   - Match object operations for accessing captured groups
   - Escape utility for special regex characters

2. **test_regex.m** - Test program demonstrating regex functionality including:
   - Basic pattern search
   - Match at beginning/full string matching
   - Case-insensitive matching
   - Special character escaping

### Build Artifacts Structure

The Mercury compiler generates various intermediate files:
- `Mercury/cs/*.c` - Generated C source files
- `Mercury/os/*.o` - Object files
- `Mercury/*s/*.{int,int2,int3,date,date3,err_date,mh,mih}` - Interface files for different compilation grades
- `*.err` - Error output files

### Foreign Language Interface

The regex module uses Mercury's C foreign language interface to integrate with PCRE2. Key implementation details:
- Uses `pragma foreign_proc` for C integration
- Memory management via Mercury's GC (`MR_GC_malloc`)
- PCRE2 contexts for compilation and matching
- Proper handling of capture groups and match positions

## Testing

Run the test program to verify regex functionality:
```bash
./test_regex
```

The test program exercises various regex operations and displays results with pass/fail indicators (✓/✗).