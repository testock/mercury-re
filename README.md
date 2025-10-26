# mercury-re

A Mercury language library providing a comprehensive interface to PCRE2 (Perl Compatible Regular Expressions 2) with an API inspired by Python's `re` module.

## Features

- **Pattern Compilation**: Compile regex patterns with multiple flags (ignorecase, multiline, dotall, verbose, ascii, unicode)
- **Search Operations**: `search`, `match`, `fullmatch`, `findall`, `finditer`
- **Capture Groups**: Full support for numbered and named capture groups
- **Substitution**: `sub` and `subn` for pattern-based string replacement
- **String Splitting**: Split strings by regex patterns
- **Match Objects**: Rich match object API for accessing groups, positions, and named groups
- **Utility Functions**: Escape special regex characters

## Prerequisites

- **Mercury Compiler**: Version 22.01 or later recommended
  - Download from [mercurylang.org](https://www.mercurylang.org/)
  - Or use a ROTD (Release of the Day) build for latest features
- **PCRE2 Library**: Version 10.x
  - Install via package manager: `brew install pcre2` (macOS) or `apt-get install libpcre2-dev` (Ubuntu)
- **pkg-config**: For locating PCRE2 headers and libraries

## Installation

### Building from Source

```bash
# Build the library
make

# Run tests
make runtests

# Install to system location (default: /usr/local/mercury)
make install
```

### Configuration

You can customize the installation prefix in `Make.options`:

```makefile
INSTALL_PREFIX = /usr/local/mercury
```

## Quick Start

### Basic Pattern Matching

```mercury
:- import_module regex.
:- import_module maybe.

main(!IO) :-
    % Search for a pattern
    regex.search("chicken", "I like chicken soup", Result),
    (
        Result = yes(Match),
        Match = match(_, _, Start, End, _, _),
        io.format("Found at positions %d-%d\n", [i(Start), i(End)], !IO)
    ;
        Result = no,
        io.write_string("No match found\n", !IO)
    ).
```

### Named Capture Groups

```mercury
% Pattern with named groups
Pattern = "(?P<letter>[A-Z])(?P<digit>[0-9]+)",
regex.search(Pattern, "A123", Result),
(
    Result = yes(Match),
    regex.group_by_name(Match, "letter", LetterGroup),
    regex.group_by_name(Match, "digit", DigitGroup),
    % LetterGroup = yes("A"), DigitGroup = yes("123")
    ...
).
```

### Case-Insensitive Matching

```mercury
% Compile with flags
regex.compile("CHICKEN", [ignorecase], CompileResult),
(
    CompileResult = ok(CompiledRegex),
    regex.search_compiled(CompiledRegex, "I like chicken", Result),
    % Matches despite different case
    ...
).
```

### Compile Once, Use Multiple Times

```mercury
% More efficient for repeated use
regex.compile("\\d+", [], CompileResult),
(
    CompileResult = ok(NumberRegex),
    regex.search_compiled(NumberRegex, "Price: $123", Result1),
    regex.search_compiled(NumberRegex, "Quantity: 456", Result2),
    ...
).
```

## API Overview

### Pattern Compilation

- `compile/2`, `compile/3` - Compile a regex pattern with optional flags

### Matching Operations

- `search/3`, `search_compiled/3` - Find first occurrence anywhere in string
- `match/3`, `match_compiled/3` - Match at beginning of string
- `fullmatch/3`, `fullmatch_compiled/3` - Match entire string
- `findall/3`, `findall_compiled/3` - Find all non-overlapping matches
- `finditer/3`, `finditer_compiled/3` - Find all match objects

### Substitution Operations

- `sub/3`, `sub/4`, `sub_compiled/3`, `sub_compiled/4` - Replace pattern matches
- `subn/5`, `subn_compiled/5` - Replace and return substitution count

### String Splitting

- `split/3`, `split/4`, `split_compiled/3`, `split_compiled/4` - Split by pattern

### Match Object Operations

- `group/3` - Get numbered capture group (0 = entire match)
- `groups/2` - Get all capture groups
- `span/3`, `start/3`, `end/3` - Get match positions
- `group_by_name/3` - Get named capture group
- `named_group_names/2` - Get list of all named group names

### Utilities

- `escape/2` - Escape special regex characters in a string

### Regex Flags

- `ignorecase` - Case-insensitive matching (PCRE2_CASELESS)
- `multiline` - ^ and $ match line boundaries (PCRE2_MULTILINE)
- `dotall` - . matches newlines (PCRE2_DOTALL)
- `verbose` - Ignore whitespace and comments (PCRE2_EXTENDED)
- `ascii` - ASCII-only matching (PCRE2_NEVER_UTF)
- `unicode` - Unicode matching (PCRE2_UTF)

## Building & Testing

### Build Commands

```bash
# Build the library
make

# Build and run tests
make runtests

# Clean all build artifacts
make realclean
```

### Manual Compilation

```bash
# Compile manually with Mercury compiler
mmc --make test_regex \
    --cflags "`pkg-config --cflags libpcre2-8`" \
    --ld-flags "`pkg-config --libs libpcre2-8`"
```

### Test Suite

The test suite (`tests/test_regex.m`) covers:
- Basic pattern search and matching
- Capture groups (numbered and named)
- Case-insensitive matching
- Special character escaping
- Optional and empty groups
- Match object operations

Run tests with: `make runtests` or `./tests/test_regex`

## Project Structure

```
mercury-re/
├── Make.options          # Build configuration
├── Makefile             # Main makefile
├── README.md            # This file
├── CLAUDE.md            # Development guidance
├── TUTORIAL.md          # Detailed tutorial
├── src/                 # Library source code
│   └── regex.m         # Main regex module
├── tests/               # Test suite
│   └── test_regex.m    # Test program
└── examples/            # Example programs
```

## Implementation Notes

- Uses Mercury's C foreign language interface to integrate with PCRE2
- Memory management via Mercury's garbage collector
- Match objects contain both numbered and named capture groups
- Some operations (findall, finditer, sub, split) are currently placeholder implementations

## Author

William Stock (ocelot_laments7j@icloud.com)

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

Copyright 2025 William Stock

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
