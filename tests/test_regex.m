%---------------------------------------------------------------------------%
% File: test_regex.m
% Test program for the regex module
%
% This program demonstrates finding "chicken" in a test string.
%
%---------------------------------------------------------------------------%

:- module test_regex.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module regex.
:- import_module string.
:- import_module maybe.
:- import_module list.
:- import_module int.

%---------------------------------------------------------------------------%

:- pred print_group(maybe(string)::in, int::in, int::out, io::di, io::uo) is det.
print_group(MaybeGroup, Index, NextIndex, !IO) :-
    NextIndex = Index + 1,
    (
        MaybeGroup = yes(Group),
        io.format("    Group %d: \"%s\"\n", [i(Index), s(Group)], !IO)
    ;
        MaybeGroup = no,
        io.format("    Group %d: (no match)\n", [i(Index)], !IO)
    ).

:- pred print_group_name(string::in, io::di, io::uo) is det.
print_group_name(GroupName, !IO) :-
    io.format("\"%s\" ", [s(GroupName)], !IO).

main(!IO) :-
    % Test strings
    Pattern = "([A-Z][0-9]|[0-9][A-Z]|[A-Z]{2})\\s*([0-9]{3,4})\\s*([A-Z]?)",
    TestString = "wow, that is a ✗G4 012A!",
    TestString2 = "wow, 🐓 that is a big fat Chicken.",
    
    io.write_string("Testing regex library\n", !IO),
    io.write_string("=====================\n", !IO),
    io.format("Pattern: \"%s\"\n", [s(Pattern)], !IO),
    io.format("Text: \"%s\"\n", [s(TestString)], !IO),
    io.nl(!IO),
    
    % Test 1: Simple search
    io.write_string("Test 1: search/3\n", !IO),
    regex.search(Pattern, TestString, SearchResult),
    (
        SearchResult = yes(Match),
        Match = match(_, _, Start, End, MaybeGroups, _),
        io.format("✓ Found match at positions %d-%d\n", [i(Start), i(End)], !IO),
        % Extract the matched substring
        string.between(TestString, Start, End, MatchedText),
        io.format("  Matched text: \"%s\"\n", [s(MatchedText)], !IO),
        io.format("  Groups (%d):\n", [i(list.length(MaybeGroups))], !IO),
        list.foldl2(print_group, MaybeGroups, 1, _, !IO)
    ;
        SearchResult = no,
        io.write_string("✗ No match found\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 2: Match at beginning (should fail)
    io.write_string("Test 2: match/3 (should fail - pattern not at start)\n", !IO),
    regex.match(Pattern, TestString, MatchResult),
    (
        MatchResult = yes(_),
        io.write_string("✗ Match found at beginning (fail)\n", !IO)
    ;
        MatchResult = no,
        io.write_string("✓ No match at beginning (expected)\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 3: Full match (should fail)
    io.write_string("Test 3: fullmatch/3 (should fail - pattern doesn't match entire string)\n", !IO),
    regex.fullmatch(Pattern, TestString, FullMatchResult),
    (
        FullMatchResult = yes(_),
        io.write_string("✗ Full match found\n", !IO)
    ;
        FullMatchResult = no,
        io.write_string("✓ No full match (expected)\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 4: Full match with exact string
    io.write_string("Test 4: fullmatch/3 with exact string\n", !IO),
    regex.fullmatch("chicken", "chicken", ExactMatchResult),
    (
        ExactMatchResult = yes(_),
        io.write_string("✓ Full match found for exact string\n", !IO)
    ;
        ExactMatchResult = no,
        io.write_string("✗ No full match for exact string\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 5: Case-insensitive search
    io.write_string("Test 5: Case-insensitive search\n", !IO),
    UpperPattern = "CHickEN",
    ( if regex.compile(UpperPattern, [ignorecase], ok(CompiledRegex)) then
        regex.search_compiled(CompiledRegex, TestString2, CaseInsensitiveResult),
        (
            CaseInsensitiveResult = yes(CIMatch),
            CIMatch = match(_, _, CIStart, CIEnd, _, _),
            io.format("✓ Case-insensitive match found at positions %d-%d: %s\n", 
                     [i(CIStart), i(CIEnd), s(string.between(TestString2, CIStart, CIEnd))], !IO)
        ;
            CaseInsensitiveResult = no,
            io.write_string("✗ No case-insensitive match found\n", !IO)
        )
    else
        io.write_string("✗ Failed to compile case-insensitive regex\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 6: Test with regex metacharacters
    io.write_string("Test 6: Escaping special characters\n", !IO),
    SpecialString = "cost is $5.99",
    DollarPattern = "\\$5\\.99",
    regex.search(DollarPattern, SpecialString, SpecialResult),
    (
        SpecialResult = yes(_),
        io.write_string("✓ Found escaped dollar sign and period\n", !IO)
    ;
        SpecialResult = no,
        io.write_string("✗ Failed to find escaped pattern\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 7: Optional groups and empty matches
    io.write_string("Test 7: Optional groups\n", !IO),
    OptionalPattern = "(\\w+)?(\\d+)?",
    OptionalString = "abc123",
    regex.search(OptionalPattern, OptionalString, OptionalResult),
    (
        OptionalResult = yes(OptMatch),
        OptMatch = match(_, _, OptStart, OptEnd, OptGroups, _),
        io.format("✓ Found optional match at positions %d-%d\n", [i(OptStart), i(OptEnd)], !IO),
        string.between(OptionalString, OptStart, OptEnd, OptMatchedText),
        io.format("  Matched text: \"%s\"\n", [s(OptMatchedText)], !IO),
        io.format("  Optional groups (%d):\n", [i(list.length(OptGroups))], !IO),
        list.foldl2(print_group, OptGroups, 1, _, !IO)
    ;
        OptionalResult = no,
        io.write_string("✗ No optional match found\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 8: Empty capture group  
    io.write_string("Test 8: Empty capture groups\n", !IO),
    EmptyPattern = "(\\w*)x(\\w*)",
    EmptyString = "x",
    regex.search(EmptyPattern, EmptyString, EmptyResult),
    (
        EmptyResult = yes(EmptyMatch),
        EmptyMatch = match(_, _, EmptyStart, EmptyEnd, EmptyGroups, _),
        io.format("✓ Found empty group match at positions %d-%d\n", [i(EmptyStart), i(EmptyEnd)], !IO),
        string.between(EmptyString, EmptyStart, EmptyEnd, EmptyMatchedText),
        io.format("  Matched text: \"%s\"\n", [s(EmptyMatchedText)], !IO),
        io.format("  Empty groups (%d):\n", [i(list.length(EmptyGroups))], !IO),
        list.foldl2(print_group, EmptyGroups, 1, _, !IO)
    ;
        EmptyResult = no,
        io.write_string("✗ No empty group match found\n", !IO)
    ),
    io.nl(!IO),
    
    % Test 9: Named groups (basic test)
    io.write_string("Test 9: Named groups\n", !IO),
    NamedPattern = "(?P<letter>[A-Z])(?P<digit>[0-9]+)",
    NamedString = "A123",
    regex.search(NamedPattern, NamedString, NamedResult),
    (
        NamedResult = yes(NamedMatch),
        NamedMatch = match(_, _, NamedStart, NamedEnd, _NamedGroups, _NamedGroupsMap),
        io.format("✓ Found named group match at positions %d-%d\n", [i(NamedStart), i(NamedEnd)], !IO),
        string.between(NamedString, NamedStart, NamedEnd, NamedMatchedText),
        io.format("  Matched text: \"%s\"\n", [s(NamedMatchedText)], !IO),
        
        % Test group_by_name predicate
        regex.group_by_name(NamedMatch, "letter", LetterGroup),
        regex.group_by_name(NamedMatch, "digit", DigitGroup),
        regex.group_by_name(NamedMatch, "nonexistent", NonexistentGroup),
        
        io.format("  Named groups:\n", [], !IO),
        (
            LetterGroup = yes(Letter),
            io.format("    letter: \"%s\"\n", [s(Letter)], !IO)
        ;
            LetterGroup = no,
            io.format("    letter: (no match)\n", [], !IO)
        ),
        (
            DigitGroup = yes(Digit),
            io.format("    digit: \"%s\"\n", [s(Digit)], !IO)
        ;
            DigitGroup = no,
            io.format("    digit: (no match)\n", [], !IO)
        ),
        (
            NonexistentGroup = yes(Nonexistent),
            io.format("    nonexistent: \"%s\" (unexpected!)\n", [s(Nonexistent)], !IO)
        ;
            NonexistentGroup = no,
            io.format("    nonexistent: (no match - expected)\n", [], !IO)
        ),
        
        % Test named_group_names predicate
        regex.named_group_names(NamedMatch, GroupNames),
        io.format("  Available group names (%d): ", [i(list.length(GroupNames))], !IO),
        list.foldl(print_group_name, GroupNames, !IO),
        io.nl(!IO)
    ;
        NamedResult = no,
        io.write_string("✗ No named group match found\n", !IO)
    ),
    
    io.write_string("\nTest completed!\n", !IO).

:- func maybe_empty_string(maybe(string)) = string.
maybe_empty_string(MaybeValue) = Out :-
    (
        MaybeValue = yes(Out)
    ;
        MaybeValue = no,
        Out = ""
    ).
%---------------------------------------------------------------------------%
:- end_module test_regex.
%---------------------------------------------------------------------------%
