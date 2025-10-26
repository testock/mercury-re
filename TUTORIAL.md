# Mercury-RE Tutorial

## Table of Contents
1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Getting Started](#getting-started)
4. [Pattern Matching Operations](#pattern-matching-operations)
5. [Working with Match Objects](#working-with-match-objects)
6. [Capture Groups](#capture-groups)
7. [Compilation Flags](#compilation-flags)
8. [Finding Multiple Matches](#finding-multiple-matches)
9. [String Substitution](#string-substitution)
10. [String Splitting](#string-splitting)
11. [Special Characters and Escaping](#special-characters-and-escaping)
12. [Error Handling](#error-handling)
13. [Common Patterns Cookbook](#common-patterns-cookbook)
14. [Performance Tips](#performance-tips)
15. [Complete Examples](#complete-examples)

## Introduction

Mercury-RE is a regular expression library for the Mercury programming language that provides Python re module functionality using the PCRE2 (Perl Compatible Regular Expressions 2) library. It offers a familiar API for Mercury developers who need powerful pattern matching capabilities.

### Features
- Full PCRE2 regex support
- Python-style named groups
- Compilation flags (case-insensitive, multiline, etc.)
- Find all matches functionality
- String substitution and splitting
- Proper Mercury type safety

## Installation

### Prerequisites
1. Mercury compiler installed and in PATH
2. PCRE2 library installed:
   ```bash
   # macOS
   brew install pcre2
   
   # Ubuntu/Debian
   sudo apt-get install libpcre2-dev
   
   # Fedora/RHEL
   sudo dnf install pcre2-devel
   ```

### Building the Library
```bash
cd mercury-re
make
make install
```

### Using in Your Project
Add to your Mercury.options file:
```
EXTRA_LIBRARIES = regex
EXTRA_LIB_DIRS = /path/to/mercury-re/src
MCFLAGS = --clang-flags "`pkg-config --cflags libpcre2-8`" \
          --ld-flags "`pkg-config --libs libpcre2-8`"
```

## Getting Started

### Basic Pattern Matching
```mercury
:- module hello_regex.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module regex.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    % Simple pattern search
    Text = "Hello, World! Welcome to Mercury.",
    Pattern = "World",
    
    regex.search(Pattern, Text, Result),
    (
        Result = yes(Match),
        Match = match(_, _, Start, End, _, _),
        io.format("Found '%s' at positions %d-%d\n", 
                  [s(Pattern), i(Start), i(End)], !IO)
    ;
        Result = no,
        io.write_string("Pattern not found\n", !IO)
    ).
```

### Understanding Mercury String Escaping
Mercury requires backslashes in strings to be escaped. Common patterns:
- `\d` becomes `"\\d"` (digit)
- `\w` becomes `"\\w"` (word character)
- `\s` becomes `"\\s"` (whitespace)
- `\\` becomes `"\\\\"` (literal backslash)

## Pattern Matching Operations

### search/3 - Find First Match Anywhere
```mercury
% Find the first number in a string
regex.search("\\d+", "The answer is 42", Result),
% Finds "42"
```

### match/3 - Match at Beginning Only
```mercury
% Check if string starts with pattern
regex.match("Hello", "Hello, World!", Result1),  % succeeds
regex.match("World", "Hello, World!", Result2),  % fails
```

### fullmatch/3 - Match Entire String
```mercury
% Validate entire string against pattern
regex.fullmatch("\\d{3}-\\d{3}-\\d{4}", "555-123-4567", Result1),  % succeeds
regex.fullmatch("\\d{3}-\\d{3}-\\d{4}", "Call 555-123-4567", Result2),  % fails
```

## Working with Match Objects

The `match` type contains:
```mercury
:- type match
    --->    match(
                string      :: string,      % Original string
                pattern     :: string,      % Pattern used
                start_pos   :: int,         % Start position of match
                end_pos     :: int,         % End position of match
                groups      :: list(maybe(string)),  % Captured groups
                named_groups :: map(string, maybe(string))  % Named groups
            ).
```

### Extracting Match Information
```mercury
regex.search("(\\w+)@(\\w+\\.\\w+)", "Contact: john@example.com", Result),
(
    Result = yes(Match),
    Match = match(_, _, Start, End, _, _),
    
    % Get the matched substring
    string.between("Contact: john@example.com", Start, End, Email),
    io.format("Email: %s\n", [s(Email)], !IO),
    
    % Get match positions
    regex.span(Match, 0, Span),  % Span = yes({Start, End})
    regex.start(Match, 0, StartPos),  % StartPos = yes(Start)
    regex.end(Match, 0, EndPos)       % EndPos = yes(End)
).
```

## Capture Groups

### Numbered Groups
Groups are numbered starting from 1 (group 0 is the entire match):

```mercury
% Parse a date
DatePattern = "(\\d{4})-(\\d{2})-(\\d{2})",
regex.search(DatePattern, "Today is 2024-01-15", Result),
(
    Result = yes(Match),
    
    % Access individual groups
    regex.group(Match, 0, FullMatch),   % yes("2024-01-15")
    regex.group(Match, 1, Year),        % yes("2024")
    regex.group(Match, 2, Month),       % yes("01")
    regex.group(Match, 3, Day),         % yes("15")
    
    % Get all groups at once (excludes group 0)
    regex.groups(Match, AllGroups),     % [yes("2024"), yes("01"), yes("15")]
    
    io.format("Date: %s/%s/%s\n", 
              [s(maybe_to_string(Month)), 
               s(maybe_to_string(Day)), 
               s(maybe_to_string(Year))], !IO)
).

:- func maybe_to_string(maybe(string)) = string.
maybe_to_string(yes(S)) = S.
maybe_to_string(no) = "".
```

### Named Groups
Use Python-style `(?P<name>...)` syntax:

```mercury
% Parse a log entry with named groups
LogPattern = "(?P<timestamp>\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}) " ++
             "\\[(?P<level>\\w+)\\] (?P<message>.*)",
             
LogLine = "2024-01-15 10:30:45 [ERROR] Connection failed",

regex.search(LogPattern, LogLine, Result),
(
    Result = yes(Match),
    
    % Access by name
    regex.group_by_name(Match, "timestamp", Timestamp),
    regex.group_by_name(Match, "level", Level),
    regex.group_by_name(Match, "message", Message),
    
    % Get all named group names
    regex.named_group_names(Match, Names),
    % Names = ["level", "message", "timestamp"]
    
    (
        Level = yes("ERROR"),
        io.write_string("ERROR detected!\n", !IO)
    ;
        Level = yes("WARNING"),
        io.write_string("Warning logged\n", !IO)
    ;
        true
    )
).
```

### Optional Groups
Groups can be optional using `?`:

```mercury
% Phone number with optional area code
PhonePattern = "(\\d{3}-)?(\\d{3}-\\d{4})",
regex.search(PhonePattern, "555-1234", Result1),
% Group 1 = no, Group 2 = yes("555-1234")

regex.search(PhonePattern, "800-555-1234", Result2),
% Group 1 = yes("800-"), Group 2 = yes("555-1234")
```

## Compilation Flags

### Available Flags
- `ignorecase` - Case-insensitive matching
- `multiline` - ^ and $ match line boundaries
- `dotall` - . matches newlines
- `verbose` - Ignore whitespace and comments in pattern
- `ascii` - ASCII-only matching
- `unicode` - Unicode matching (default)

### Using Flags
```mercury
% Case-insensitive search
regex.compile("hello", [ignorecase], CompiledResult),
(
    CompiledResult = ok(CaseInsensitiveRegex),
    regex.search_compiled(CaseInsensitiveRegex, "HELLO WORLD", Match),
    % This will find "HELLO"
    
    % Use multiple flags
    regex.compile("^world", [ignorecase, multiline], MultilineResult),
    (
        MultilineResult = ok(MultilineRegex),
        regex.search_compiled(MultilineRegex, "Hello\nWORLD", Match2)
        % Matches "WORLD" at start of second line
    )
).

% Direct use with flags
regex.search_with_flags("HELLO", "hello world", [ignorecase], Result).
```

## Finding Multiple Matches

### findall/3 - Get All Matching Strings
```mercury
% Extract all email addresses
EmailPattern = "\\b[\\w._%+-]+@[\\w.-]+\\.[A-Z|a-z]{2,}\\b",
Text = "Contact alice@example.com or bob@test.org for details",

regex.findall(EmailPattern, Text, Emails),
% Emails = ["alice@example.com", "bob@test.org"]

% Extract all numbers
regex.findall("\\d+", "Room 101, Floor 3, Building 42", Numbers),
% Numbers = ["101", "3", "42"]
```

### finditer/3 - Get All Match Objects
```mercury
% Get detailed information about all matches
regex.finditer("\\w+", "Hello World", Matches),
list.foldl(process_match, Matches, !IO).

:- pred process_match(match::in, io::di, io::uo) is det.
process_match(Match, !IO) :-
    Match = match(_, _, Start, End, _, _),
    string.between("Hello World", Start, End, Word),
    io.format("Found '%s' at %d-%d\n", [s(Word), i(Start), i(End)], !IO).
% Output:
% Found 'Hello' at 0-5
% Found 'World' at 6-11
```

## String Substitution

### sub/4 - Replace All Occurrences
```mercury
% Simple replacement
regex.sub("cat", "dog", "The cat chased the cat", Result),
% Result = "The dog chased the dog"

% Using backreferences
regex.sub("(\\w+)@(\\w+)", "\\2.\\1", "user@domain", Result2),
% Result2 = "domain.user"

% Complex replacement
regex.sub("\\b(\\w)(\\w+)", "\\U\\1\\L\\2", "hello world", Result3),
% Result3 = "Hello World" (capitalize words)
```

### sub/5 - Replace with Limit
```mercury
% Replace only first N occurrences
regex.sub("a", "A", "banana", 2, Result),
% Result = "bAnAna" (only first 2 'a's replaced)

% Count = 0 means replace all
regex.sub("e", "E", "elephant", 0, Result2),
% Result2 = "ElEphant"
```

### subn/6 - Replace and Count
```mercury
% Get replacement count
regex.subn("\\d+", "[NUM]", "Room 101 on Floor 3", 0, Result, Count),
% Result = "Room [NUM] on Floor [NUM]"
% Count = 2
```

## String Splitting

### split/3 - Split with No Limit
```mercury
% Split by whitespace
regex.split("\\s+", "one   two     three", Parts),
% Parts = ["one", "two", "three"]

% Split by punctuation
regex.split("[,;]", "apple,banana;orange,grape", Fruits),
% Fruits = ["apple", "banana", "orange", "grape"]
```

### split/4 - Split with Limit
```mercury
% Limit number of splits
regex.split(",", "a,b,c,d,e", 2, Parts),
% Parts = ["a", "b", "c,d,e"] (only 2 splits performed)

% Split lines with limit
regex.split("\\n", "line1\nline2\nline3\nline4", 2, FirstTwoLines),
% FirstTwoLines = ["line1", "line2", "line3\nline4"]
```

## Special Characters and Escaping

### escape/2 - Escape Special Characters
```mercury
% Escape user input for safe pattern use
UserInput = "What's the cost? $5.99!",
regex.escape(UserInput, SafePattern),
% SafePattern = "What's the cost\\? \\$5\\.99!"

% Use escaped string in search
regex.search(SafePattern, "The price is: What's the cost? $5.99!", Found).
```

### Common Special Characters
- `.` - Any character (except newline unless dotall flag)
- `*` - Zero or more
- `+` - One or more
- `?` - Zero or one
- `^` - Start of string/line
- `$` - End of string/line
- `[]` - Character class
- `()` - Group
- `{}` - Quantifier
- `|` - Alternation
- `\` - Escape character

## Error Handling

### Compilation Errors
```mercury
:- pred safe_regex_search(string::in, string::in, maybe(match)::out, 
                         string::out, io::di, io::uo) is det.
safe_regex_search(Pattern, Text, Result, ErrorMsg, !IO) :-
    regex.compile(Pattern, CompileResult),
    (
        CompileResult = ok(Regex),
        regex.search_compiled(Regex, Text, Result),
        ErrorMsg = ""
    ;
        CompileResult = error(compile_error(Msg)),
        Result = no,
        ErrorMsg = Msg,
        io.format("Regex compilation error: %s\n", [s(Msg)], !IO)
    ).

% Example usage
safe_regex_search("[invalid", "text", Result, Error, !IO).
% Outputs: Regex compilation error: missing terminating ] for character class
```

### Pattern Validation
```mercury
:- pred is_valid_pattern(string::in) is semidet.
is_valid_pattern(Pattern) :-
    regex.compile(Pattern, ok(_)).

% Use in input validation
( is_valid_pattern(UserPattern) ->
    process_with_pattern(UserPattern, !IO)
;
    io.write_string("Invalid regex pattern\n", !IO)
).
```

## Common Patterns Cookbook

### Email Validation
```mercury
email_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$".

:- pred is_valid_email(string::in) is semidet.
is_valid_email(Email) :-
    regex.fullmatch(email_pattern, Email, yes(_)).
```

### URL Extraction
```mercury
url_pattern = "https?://[^\\s]+".

:- pred extract_urls(string::in, list(string)::out) is det.
extract_urls(Text, URLs) :-
    regex.findall(url_pattern, Text, URLs).
```

### Phone Number Validation
```mercury
% US phone number formats
us_phone_pattern = "^\\(?([0-9]{3})\\)?[-. ]?([0-9]{3})[-. ]?([0-9]{4})$".

:- pred normalize_phone(string::in, string::out) is semidet.
normalize_phone(Phone, Normalized) :-
    regex.search(us_phone_pattern, Phone, yes(Match)),
    regex.group(Match, 1, yes(Area)),
    regex.group(Match, 2, yes(Prefix)),
    regex.group(Match, 3, yes(Number)),
    Normalized = string.format("(%s) %s-%s", [s(Area), s(Prefix), s(Number)]).
```

### IP Address Validation
```mercury
ipv4_pattern = "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}" ++
               "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$".

:- pred is_valid_ipv4(string::in) is semidet.
is_valid_ipv4(IP) :-
    regex.fullmatch(ipv4_pattern, IP, yes(_)).
```

### HTML Tag Removal
```mercury
:- pred strip_html_tags(string::in, string::out) is det.
strip_html_tags(HTML, Text) :-
    regex.sub("<[^>]+>", "", HTML, Text).
```

### CSV Parsing
```mercury
:- pred parse_csv_line(string::in, list(string)::out) is det.
parse_csv_line(Line, Fields) :-
    % Handle quoted fields with commas
    CSVPattern = "(?:^|,)(?:\"([^\"]*)\"|([^,]*))",
    regex.finditer(CSVPattern, Line, Matches),
    list.map(extract_csv_field, Matches, Fields).

:- pred extract_csv_field(match::in, string::out) is det.
extract_csv_field(Match, Field) :-
    ( regex.group(Match, 1, yes(Quoted)) ->
        Field = Quoted
    ; regex.group(Match, 2, yes(Unquoted)) ->
        Field = Unquoted
    ;
        Field = ""
    ).
```

## Performance Tips

### 1. Compile Once, Use Many Times
```mercury
:- type processor
    --->    processor(
                log_regex    :: regex,
                error_regex  :: regex,
                warn_regex   :: regex
            ).

:- pred init_processor(processor::out, io::di, io::uo) is det.
init_processor(Processor, !IO) :-
    regex.compile("\\[(\\d{4}-\\d{2}-\\d{2})\\] (\\w+): (.*)", LogRes),
    regex.compile("ERROR|FATAL", [ignorecase], ErrorRes),
    regex.compile("WARN|WARNING", [ignorecase], WarnRes),
    (
        LogRes = ok(LogRegex),
        ErrorRes = ok(ErrorRegex),
        WarnRes = ok(WarnRegex),
        Processor = processor(LogRegex, ErrorRegex, WarnRegex)
    ;
        error("Failed to compile regexes")
    ).
```

### 2. Use Specific Patterns
```mercury
% Good: Specific pattern
good_pattern = "\\b[A-Z]{2}\\d{6}\\b".  % Matches specific ID format

% Bad: Too general
bad_pattern = ".*".  % Matches everything, inefficient
```

### 3. Use Anchors When Possible
```mercury
% Good: Anchored pattern
regex.match("^ERROR:", LogLine, _).  % Only checks beginning

% Less efficient: Unanchored
regex.search("ERROR:", LogLine, _).  % Searches entire string
```

### 4. Avoid Backtracking
```mercury
% Good: Possessive quantifiers
efficient_pattern = "\\d++\\.\\d++".  % No backtracking

% Bad: Greedy with potential backtracking
inefficient_pattern = "\\d+\\.\\d+".
```

## Complete Examples

### Example 1: Log File Analyzer
```mercury
:- module log_analyzer.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module regex, string, list, maybe, int, map.

:- type log_level
    --->    error
    ;       warning
    ;       info
    ;       debug.

:- type log_entry
    --->    log_entry(
                timestamp :: string,
                level     :: log_level,
                message   :: string
            ).

main(!IO) :-
    % Compile regex patterns
    LogPattern = "\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\] " ++
                 "(ERROR|WARNING|INFO|DEBUG): (.+)",
    
    regex.compile(LogPattern, CompileResult),
    (
        CompileResult = ok(LogRegex),
        
        % Initialize counters
        map.init(Counters0),
        map.set(error, 0, Counters0, Counters1),
        map.set(warning, 0, Counters1, Counters2),
        map.set(info, 0, Counters2, Counters3),
        map.set(debug, 0, Counters3, Counters),
        
        % Process log file
        io.open_input("application.log", OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            process_log_stream(Stream, LogRegex, Counters, FinalCounters, !IO),
            io.close_input(Stream, !IO),
            
            % Print summary
            print_summary(FinalCounters, !IO)
        ;
            OpenResult = error(Error),
            io.format("Error opening log file: %s\n", 
                     [s(io.error_message(Error))], !IO)
        )
    ;
        CompileResult = error(compile_error(Msg)),
        io.format("Regex compilation failed: %s\n", [s(Msg)], !IO)
    ).

:- pred process_log_stream(io.text_input_stream::in, regex::in,
                          map(log_level, int)::in, map(log_level, int)::out,
                          io::di, io::uo) is det.
process_log_stream(Stream, LogRegex, !Counters, !IO) :-
    io.read_line_as_string(Stream, Result, !IO),
    (
        Result = ok(Line),
        process_log_line(Line, LogRegex, !Counters, !IO),
        process_log_stream(Stream, LogRegex, !Counters, !IO)
    ;
        Result = eof
    ;
        Result = error(_)
    ).

:- pred process_log_line(string::in, regex::in,
                        map(log_level, int)::in, map(log_level, int)::out,
                        io::di, io::uo) is det.
process_log_line(Line, LogRegex, !Counters, !IO) :-
    regex.search_compiled(LogRegex, Line, MatchResult),
    (
        MatchResult = yes(Match),
        regex.group(Match, 1, yes(Timestamp)),
        regex.group(Match, 2, yes(LevelStr)),
        regex.group(Match, 3, yes(Message)),
        
        ( parse_level(LevelStr, Level) ->
            % Update counter
            map.lookup(!.Counters, Level, OldCount),
            map.set(Level, OldCount + 1, !Counters),
            
            % Process based on level
            ( Level = error ->
                io.format("ERROR at %s: %s\n", 
                         [s(Timestamp), s(Message)], !IO)
            ; Level = warning ->
                % Could send warnings to a different handler
                true
            ;
                % INFO and DEBUG - usually just count
                true
            )
        ;
            io.format("Unknown log level: %s\n", [s(LevelStr)], !IO)
        )
    ;
        MatchResult = no
        % Not a valid log line, skip
    ).

:- pred parse_level(string::in, log_level::out) is semidet.
parse_level("ERROR", error).
parse_level("WARNING", warning).
parse_level("INFO", info).
parse_level("DEBUG", debug).

:- pred print_summary(map(log_level, int)::in, io::di, io::uo) is det.
print_summary(Counters, !IO) :-
    io.write_string("\nLog Summary:\n", !IO),
    io.write_string("============\n", !IO),
    map.lookup(Counters, error, Errors),
    map.lookup(Counters, warning, Warnings),
    map.lookup(Counters, info, Infos),
    map.lookup(Counters, debug, Debugs),
    io.format("Errors:   %d\n", [i(Errors)], !IO),
    io.format("Warnings: %d\n", [i(Warnings)], !IO),
    io.format("Info:     %d\n", [i(Infos)], !IO),
    io.format("Debug:    %d\n", [i(Debugs)], !IO),
    Total = Errors + Warnings + Infos + Debugs,
    io.format("Total:    %d\n", [i(Total)], !IO).
```

### Example 2: Configuration File Parser
```mercury
:- module config_parser.
:- interface.
:- import_module io, map.

:- type config == map(string, string).

:- pred parse_config_file(string::in, maybe_error(config)::out, 
                         io::di, io::uo) is det.

:- implementation.
:- import_module regex, string, list, maybe.

parse_config_file(Filename, Result, !IO) :-
    io.open_input(Filename, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        parse_config_stream(Stream, map.init, Config, !IO),
        io.close_input(Stream, !IO),
        Result = ok(Config)
    ;
        OpenResult = error(Error),
        Result = error(io.error_message(Error))
    ).

:- pred parse_config_stream(io.text_input_stream::in, config::in, config::out,
                           io::di, io::uo) is det.
parse_config_stream(Stream, !Config, !IO) :-
    io.read_line_as_string(Stream, ReadResult, !IO),
    (
        ReadResult = ok(Line),
        process_config_line(Line, !Config),
        parse_config_stream(Stream, !Config, !IO)
    ;
        ReadResult = eof
    ;
        ReadResult = error(_)
    ).

:- pred process_config_line(string::in, config::in, config::out) is det.
process_config_line(Line, !Config) :-
    % Remove comments and trim
    CommentPattern = "#.*$",
    regex.sub(CommentPattern, "", Line, NoComment),
    Trimmed = string.strip(NoComment),
    
    % Parse key=value pairs
    ( Trimmed = "" ->
        % Empty line, skip
        true
    ;
        KeyValuePattern = "^([\\w\\.]+)\\s*=\\s*(.*)$",
        regex.search(KeyValuePattern, Trimmed, MatchResult),
        (
            MatchResult = yes(Match),
            regex.group(Match, 1, yes(Key)),
            regex.group(Match, 2, yes(RawValue)),
            
            % Process value (remove quotes if present)
            process_value(RawValue, Value),
            map.set(Key, Value, !Config)
        ;
            MatchResult = no
            % Invalid line format, skip
        )
    ).

:- pred process_value(string::in, string::out) is det.
process_value(RawValue, Value) :-
    % Remove surrounding quotes
    QuotePattern = "^[\"'](.*)['|\"]$",
    ( regex.search(QuotePattern, RawValue, yes(Match)) ->
        regex.group(Match, 1, yes(Value))
    ;
        Value = RawValue
    ).
```

### Example 3: Data Validator
```mercury
:- module data_validator.
:- interface.
:- import_module io, list, maybe.

:- type validation_error
    --->    invalid_email(string)
    ;       invalid_phone(string)
    ;       invalid_date(string)
    ;       invalid_ssn(string).

:- pred validate_user_data(string::in, string::in, string::in, string::in,
                          list(validation_error)::out) is det.

:- implementation.
:- import_module regex, string.

validate_user_data(Email, Phone, Date, SSN, Errors) :-
    validate_email(Email, EmailErrors),
    validate_phone(Phone, PhoneErrors),
    validate_date(Date, DateErrors),
    validate_ssn(SSN, SSNErrors),
    list.append_list([EmailErrors, PhoneErrors, DateErrors, SSNErrors], Errors).

:- pred validate_email(string::in, list(validation_error)::out) is det.
validate_email(Email, Errors) :-
    EmailPattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$",
    ( regex.fullmatch(EmailPattern, Email, yes(_)) ->
        Errors = []
    ;
        Errors = [invalid_email(Email)]
    ).

:- pred validate_phone(string::in, list(validation_error)::out) is det.
validate_phone(Phone, Errors) :-
    % US phone number: (123) 456-7890 or 123-456-7890 or 1234567890
    PhonePattern = "^\\(?([0-9]{3})\\)?[-. ]?([0-9]{3})[-. ]?([0-9]{4})$",
    ( regex.fullmatch(PhonePattern, Phone, yes(_)) ->
        Errors = []
    ;
        Errors = [invalid_phone(Phone)]
    ).

:- pred validate_date(string::in, list(validation_error)::out) is det.
validate_date(Date, Errors) :-
    % ISO format: YYYY-MM-DD
    DatePattern = "^(\\d{4})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$",
    ( regex.fullmatch(DatePattern, Date, yes(Match)) ->
        regex.group(Match, 1, yes(YearStr)),
        regex.group(Match, 2, yes(MonthStr)),
        regex.group(Match, 3, yes(DayStr)),
        
        % Additional validation could check for valid dates
        % (e.g., no Feb 30th)
        Errors = []
    ;
        Errors = [invalid_date(Date)]
    ).

:- pred validate_ssn(string::in, list(validation_error)::out) is det.
validate_ssn(SSN, Errors) :-
    % US SSN: 123-45-6789
    SSNPattern = "^(?!000|666)[0-9]{3}-(?!00)[0-9]{2}-(?!0000)[0-9]{4}$",
    ( regex.fullmatch(SSNPattern, SSN, yes(_)) ->
        Errors = []
    ;
        Errors = [invalid_ssn(SSN)]
    ).
```

## Troubleshooting

### Common Issues

1. **Pattern Not Matching**
   - Check Mercury string escaping (double backslashes)
   - Test pattern with online regex testers
   - Use `verbose` flag for complex patterns

2. **Compilation Errors**
   - Unmatched brackets or parentheses
   - Invalid escape sequences
   - Use `escape/2` for literal strings

3. **Performance Issues**
   - Compile patterns once and reuse
   - Avoid catastrophic backtracking
   - Use more specific patterns

4. **Memory Issues**
   - PCRE2 manages its own memory
   - Match objects are garbage collected
   - Consider streaming for large files

## Quick Reference

### Basic Operations
```mercury
regex.search(Pattern, Text, Result)           % Find first
regex.match(Pattern, Text, Result)            % Match at start
regex.fullmatch(Pattern, Text, Result)        % Match entire string
regex.findall(Pattern, Text, Matches)         % Find all strings
regex.finditer(Pattern, Text, MatchObjects)   % Find all matches
regex.sub(Pattern, Replacement, Text, Result) % Replace all
regex.split(Pattern, Text, Parts)             % Split string
regex.escape(Text, SafePattern)               % Escape special chars
```

### Match Object Access
```mercury
regex.group(Match, N, Group)                  % Get group N
regex.groups(Match, AllGroups)                % Get all groups
regex.group_by_name(Match, Name, Group)       % Get named group
regex.named_group_names(Match, Names)         % Get group names
regex.span(Match, N, {Start, End})           % Get positions
```

### Compilation
```mercury
regex.compile(Pattern, Flags, Result)         % Compile with flags
regex.search_compiled(Regex, Text, Result)    % Use compiled regex
```

## Additional Resources

- [PCRE2 Documentation](https://www.pcre.org/current/doc/html/)
- [Python re Module](https://docs.python.org/3/library/re.html) (similar API)
- [Regular Expression Reference](https://www.regular-expressions.info/)
- Mercury Language Reference Manual

## Contributing

To contribute to mercury-re:
1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

See LICENSE file in the mercury-re repository.
