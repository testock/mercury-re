%---------------------------------------------------------------------------%
% File: regex.m
% Main Author: William Stock
% Stability: low
%
% A Mercury interface to PCRE2 providing Python re module functionality.
%
%---------------------------------------------------------------------------%

:- module regex.
:- interface.

:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

% Regular expression compilation flags
:- type regex_flag
    --->    ignorecase      % re.IGNORECASE
    ;       multiline       % re.MULTILINE
    ;       dotall          % re.DOTALL
    ;       verbose         % re.VERBOSE
    ;       ascii           % re.ASCII
    ;       unicode.        % Default Unicode mode

% Compiled regular expression pattern
:- type regex.

% Match object containing match results
:- type match
    --->    match(
                string      :: string,      % Original string
                pattern     :: string,      % Pattern used
                start_pos   :: int,         % Start position of match
                end_pos     :: int,         % End position of match
                groups      :: list(maybe(string)),  % Captured groups (by number)
                named_groups :: map(string, maybe(string))  % Named groups
            ).

% Exception type for regex errors
:- type regex_error
    --->    compile_error(string)
    ;       runtime_error(string).

%---------------------------------------------------------------------------%

% Pattern compilation

    % compile(Pattern, Flags, Result)
    % Compile a regular expression pattern with optional flags.
    %
:- pred compile(string::in, list(regex_flag)::in, 
    maybe_error(regex, regex_error)::out) is det.

    % compile(Pattern, Result)
    % Compile a regular expression pattern with default flags.
    %
:- pred compile(string::in, maybe_error(regex, regex_error)::out) is det.

%---------------------------------------------------------------------------%

% Matching operations

    % search(Pattern, String, Match)
    % Search for first occurrence of pattern in string.
    %
:- pred search(string::in, string::in, maybe(match)::out) is det.

    % search(Regex, String, Match)
    % Search using compiled regex.
    %
:- pred search_compiled(regex::in, string::in, maybe(match)::out) is det.

    % match(Pattern, String, Match)
    % Match pattern at beginning of string.
    %
:- pred match(string::in, string::in, maybe(match)::out) is det.

    % match(Regex, String, Match)
    % Match compiled regex at beginning of string.
    %
:- pred match_compiled(regex::in, string::in, maybe(match)::out) is det.

    % fullmatch(Pattern, String, Match)
    % Match pattern against entire string.
    %
:- pred fullmatch(string::in, string::in, maybe(match)::out) is det.

    % fullmatch(Regex, String, Match)
    % Match compiled regex against entire string.
    %
:- pred fullmatch_compiled(regex::in, string::in, maybe(match)::out) is det.

    % findall(Pattern, String, Matches)
    % Find all non-overlapping matches.
    %
:- pred findall(string::in, string::in, list(string)::out) is det.

    % findall(Regex, String, Matches)
    % Find all matches using compiled regex.
    %
:- pred findall_compiled(regex::in, string::in, list(string)::out) is det.

    % finditer(Pattern, String, Matches)
    % Find all match objects.
    %
:- pred finditer(string::in, string::in, list(match)::out) is det.

    % finditer(Regex, String, Matches)
    % Find all match objects using compiled regex.
    %
:- pred finditer_compiled(regex::in, string::in, list(match)::out) is det.

%---------------------------------------------------------------------------%

% Substitution operations

    % sub(Pattern, Replacement, String, Count, Result)
    % Replace first Count occurrences (0 = all).
    %
:- pred sub(string::in, string::in, string::in, int::in, string::out) is det.

    % sub(Pattern, Replacement, String, Result)
    % Replace all occurrences.
    %
:- pred sub(string::in, string::in, string::in, string::out) is det.

    % sub(Regex, Replacement, String, Count, Result)
    % Replace using compiled regex.
    %
:- pred sub_compiled(regex::in, string::in, string::in, int::in, string::out) is det.

    % sub(Regex, Replacement, String, Result)
    % Replace all using compiled regex.
    %
:- pred sub_compiled(regex::in, string::in, string::in, string::out) is det.

    % subn(Pattern, Replacement, String, Count, Result, NumSubs)
    % Replace and return number of substitutions made.
    %
:- pred subn(string::in, string::in, string::in, int::in, 
    string::out, int::out) is det.

    % subn(Regex, Replacement, String, Count, Result, NumSubs)
    % Replace using compiled regex and return substitution count.
    %
:- pred subn_compiled(regex::in, string::in, string::in, int::in, 
    string::out, int::out) is det.

%---------------------------------------------------------------------------%

% String splitting

    % split(Pattern, String, MaxSplit, Parts)
    % Split string by pattern (MaxSplit = 0 means no limit).
    %
:- pred split(string::in, string::in, int::in, list(string)::out) is det.

    % split(Pattern, String, Parts)
    % Split string by pattern with no limit.
    %
:- pred split(string::in, string::in, list(string)::out) is det.

    % split(Regex, String, MaxSplit, Parts)
    % Split using compiled regex.
    %
:- pred split_compiled(regex::in, string::in, int::in, list(string)::out) is det.

    % split(Regex, String, Parts)
    % Split using compiled regex with no limit.
    %
:- pred split_compiled(regex::in, string::in, list(string)::out) is det.

%---------------------------------------------------------------------------%

% Match object operations

    % group(Match, GroupNum, Group)
    % Get numbered capture group (0 = entire match).
    %
:- pred group(match::in, int::in, maybe(string)::out) is det.

    % groups(Match, Groups)
    % Get all capture groups (excluding group 0).
    %
:- pred groups(match::in, list(maybe(string))::out) is det.

    % span(Match, GroupNum, Start, End)
    % Get start and end positions of group.
    %
:- pred span(match::in, int::in, maybe({int, int})::out) is det.

    % start(Match, GroupNum, Position)
    % Get start position of group.
    %
:- pred start(match::in, int::in, maybe(int)::out) is det.

    % end(Match, GroupNum, Position)
    % Get end position of group.
    %
:- pred end(match::in, int::in, maybe(int)::out) is det.

    % group_by_name(Match, Name, Group)
    % Get named group by name.
    %
:- pred group_by_name(match::in, string::in, maybe(string)::out) is det.

    % named_group_names(Match, Names)
    % Get all named group names.
    %
:- pred named_group_names(match::in, list(string)::out) is det.

%---------------------------------------------------------------------------%

% Utility predicates

    % escape(String, Escaped)
    % Escape special regex characters in string.
    %
:- pred escape(string::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module char.
:- import_module string.

%---------------------------------------------------------------------------%

% We'll need to use the foreign language interface to call PCRE2
:- pragma foreign_decl("C", "
    #define PCRE2_CODE_UNIT_WIDTH 8
    #include <pcre2.h>
    #include <string.h>
    #include <stdlib.h>
    
    typedef struct {
        pcre2_code *code;
        pcre2_match_context *mcontext;
        pcre2_compile_context *ccontext;
    } MR_Regex;
    
    // Helper function to safely create maybe(string) values
    static MR_Word make_maybe_string(const char* original_string, 
                                    PCRE2_SIZE start_pos, PCRE2_SIZE end_pos) {
        if (start_pos == PCRE2_UNSET || end_pos == PCRE2_UNSET) {
            // Return maybe.no (tag 0)
            return MR_mktag(0);
        } else {
            // Create the substring
            PCRE2_SIZE length = end_pos - start_pos;
            char* temp_string = malloc(length + 1);
            memcpy(temp_string, original_string + start_pos, length);
            temp_string[length] = '\\0';
            
            // Create Mercury string on heap
            char* mercury_string;
            MR_make_aligned_string_copy(mercury_string, temp_string);
            free(temp_string);
            
            // Create maybe.yes(string) (tag 1, one field)
            MR_Word maybe_val;
            MR_tag_incr_hp(maybe_val, MR_mktag(1), 1);
            MR_field(MR_mktag(1), maybe_val, 0) = (MR_Word) mercury_string;
            return maybe_val;
        }
    }
    
    // Forward declarations for Mercury-exported functions
    extern void mercury_regex_init_string_map(MR_Word *map);
    extern void mercury_regex_insert_into_string_map(MR_String key, MR_Word value, 
                                                     MR_Word map_in, MR_Word *map_out);
    
    // Helper function to extract named groups into a Mercury map
    static MR_Word extract_named_groups(pcre2_code* code, pcre2_match_data* match_data,
                                       const char* string) {
        MR_Word named_map;
        
        // Initialize empty map
        mercury_regex_init_string_map(&named_map);
        
        // Get name table information
        PCRE2_SPTR name_table;
        uint32_t name_count;
        uint32_t name_entry_size;
        
        pcre2_pattern_info(code, PCRE2_INFO_NAMECOUNT, &name_count);
        
        if (name_count > 0) {
            pcre2_pattern_info(code, PCRE2_INFO_NAMETABLE, &name_table);
            pcre2_pattern_info(code, PCRE2_INFO_NAMEENTRYSIZE, &name_entry_size);
            
            PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
            
            // Iterate through named groups
            for (uint32_t i = 0; i < name_count; i++) {
                // Each entry is: 2 bytes for number, then name (null-terminated)
                int group_number = (name_table[0] << 8) | name_table[1];
                const char* group_name = (const char*)(name_table + 2);
                
                // Create Mercury string for the group name
                char* mercury_name;
                MR_make_aligned_string_copy(mercury_name, group_name);
                
                // Get the group value
                MR_Word maybe_value = make_maybe_string(string, 
                                                       ovector[2*group_number], 
                                                       ovector[2*group_number+1]);
                
                // Insert into map
                MR_Word new_map;
                mercury_regex_insert_into_string_map(mercury_name, maybe_value, 
                                                    named_map, &new_map);
                named_map = new_map;
                
                // Move to next entry
                name_table += name_entry_size;
            }
        }
        
        return named_map;
    }
").

:- pragma foreign_type("C", regex, "MR_Regex *").

%---------------------------------------------------------------------------%

% Flag conversion
:- func flags_to_pcre2_options(list(regex_flag)) = int.

flags_to_pcre2_options([]) = 0.
flags_to_pcre2_options([Flag | Flags]) = 
    flag_to_pcre2_option(Flag) \/ flags_to_pcre2_options(Flags).

:- func flag_to_pcre2_option(regex_flag) = int.
:- pragma foreign_proc("C",
    flag_to_pcre2_option(Flag::in) = (Option::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    switch (Flag) {
        case 0: Option = PCRE2_CASELESS; break;      /* ignorecase */
        case 1: Option = PCRE2_MULTILINE; break;     /* multiline */
        case 2: Option = PCRE2_DOTALL; break;        /* dotall */
        case 3: Option = PCRE2_EXTENDED; break;      /* verbose */
        case 4: Option = PCRE2_NEVER_UTF; break;     /* ascii */
        case 5: Option = PCRE2_UTF; break;           /* unicode */
        default: Option = 0; break;
    }
").

%---------------------------------------------------------------------------%

% Compilation predicates

compile(Pattern, Result) :-
    compile(Pattern, [], Result).

compile(Pattern, Flags, Result) :-
    Options = flags_to_pcre2_options(Flags),
    do_compile(Pattern, Options, Success, CompiledRegex, ErrorMsg),
    ( if Success = 1 then
        Result = ok(CompiledRegex)
    else
        Result = error(compile_error(ErrorMsg))
    ).

:- pred do_compile(string::in, int::in, int::out, regex::out, string::out) is det.
:- pragma foreign_proc("C",
    do_compile(Pattern::in, Options::in, Success::out, CompiledRegex::out, ErrorMsg::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    int error_code;
    PCRE2_SIZE error_offset;
    MR_Regex *regex;
    
    regex = MR_GC_malloc(sizeof(MR_Regex));
    regex->ccontext = pcre2_compile_context_create(NULL);
    regex->mcontext = pcre2_match_context_create(NULL);
    
    regex->code = pcre2_compile(
        (PCRE2_SPTR8)Pattern,
        PCRE2_ZERO_TERMINATED,
        Options,
        &error_code,
        &error_offset,
        regex->ccontext
    );
    
    if (regex->code == NULL) {
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(error_code, buffer, sizeof(buffer));
        
        MR_make_aligned_string_copy(ErrorMsg, (char*)buffer);
        CompiledRegex = NULL;
        Success = 0;
        
        /* Clean up */
        pcre2_compile_context_free(regex->ccontext);
        pcre2_match_context_free(regex->mcontext);
        MR_GC_free(regex);
    } else {
        MR_make_aligned_string_copy(ErrorMsg, \"\");
        CompiledRegex = regex;
        Success = 1;
    }
").

%---------------------------------------------------------------------------%

% Search operations

search(Pattern, String, Result) :-
    ( if compile(Pattern, ok(Regex)) then
        search_compiled(Regex, String, Result)
    else
        Result = no
    ).

search_compiled(Regex, String, Result) :-
    do_search_match(Regex, String, Success, Start, End, Groups, NamedGroups),
    ( if Success = 1 then
        Match = match(String, "", Start, End, Groups, NamedGroups),
        Result = yes(Match)
    else
        Result = no
    ).

:- pred do_search_match(regex::in, string::in, int::out, int::out, int::out, list(maybe(string))::out, map(string, maybe(string))::out) is det.
:- pragma foreign_proc("C",
    do_search_match(Regex::in, String::in, Success::out, Start::out, End::out, Groups::out, NamedGroups::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pcre2_match_data *match_data;
    int rc;
    
    match_data = pcre2_match_data_create_from_pattern(Regex->code, NULL);
    
    rc = pcre2_match(
        Regex->code,
        (PCRE2_SPTR8)String,
        strlen(String),
        0,                    /* start offset */
        0,                    /* options */
        match_data,
        Regex->mcontext
    );
    
    if (rc >= 0) {
        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        Start = (int)ovector[0];
        End = (int)ovector[1];
        Success = 1;
        
        /* Extract capture groups using the helper function */
        Groups = MR_list_empty();
        
        /* Build list in reverse order (last group first) */
        for (int i = rc - 1; i >= 1; i--) {
            MR_Word maybe_group = make_maybe_string(String, ovector[2*i], ovector[2*i+1]);
            Groups = MR_list_cons(maybe_group, Groups);
        }
        
        /* Extract named groups */
        NamedGroups = extract_named_groups(Regex->code, match_data, String);
    } else {
        Start = 0;
        End = 0;
        Success = 0;
        Groups = MR_list_empty();
        NamedGroups = (MR_Word) MR_mktag(0);
    }
    
    pcre2_match_data_free(match_data);
").

%% :- pred do_search_match(regex::in, string::in, int::out, int::out, int::out, list(maybe(string))::out) is det.
%% :- pragma foreign_proc("C",
%%     do_search_match(Regex::in, String::in, Success::out, Start::out, End::out, Groups::out),
%%     [will_not_call_mercury, promise_pure, thread_safe],
%% "
%%     pcre2_match_data *match_data;
%%     int rc;
%%     
%%     match_data = pcre2_match_data_create_from_pattern(Regex->code, NULL);
%%     
%%     rc = pcre2_match(
%%         Regex->code,
%%         (PCRE2_SPTR8)String,
%%         strlen(String),
%%         0,                    /* start offset */
%%         0,                    /* options */
%%         match_data,
%%         Regex->mcontext
%%     );
%%     
%%     if (rc >= 0) {
%%         PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
%%         Start = (int)ovector[0];
%%         End = (int)ovector[1];
%%         Success = 1;
%%         
%%         /* For now, just return empty groups list */
%%         /* A full implementation would extract capture groups here */
%%         Groups = MR_list_empty();
%%     } else {
%%         Start = 0;
%%         End = 0;
%%         Success = 0;
%%         Groups = MR_list_empty();
%%     }
%%     
%%     pcre2_match_data_free(match_data);
%% ").

% Match at beginning of string
match(Pattern, String, Result) :-
    ( if compile("^(?:" ++ Pattern ++ ")", ok(Regex)) then
        search_compiled(Regex, String, Result)
    else
        Result = no
    ).

match_compiled(Regex, String, Result) :-
    do_match_at_start(Regex, String, Success, Start, End, Groups, NamedGroups),
    ( if Success = 1 then
        Match = match(String, "", Start, End, Groups, NamedGroups),
        Result = yes(Match)
    else
        Result = no
    ).

:- pred do_match_at_start(regex::in, string::in, int::out, int::out, int::out, list(maybe(string))::out, map(string, maybe(string))::out) is det.
:- pragma foreign_proc("C",
    do_match_at_start(Regex::in, String::in, Success::out, Start::out, End::out, Groups::out, NamedGroups::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pcre2_match_data *match_data;
    int rc;
    
    match_data = pcre2_match_data_create_from_pattern(Regex->code, NULL);
    
    rc = pcre2_match(
        Regex->code,
        (PCRE2_SPTR8)String,
        strlen(String),
        0,                    /* start offset */
        PCRE2_ANCHORED,       /* must match at start */
        match_data,
        Regex->mcontext
    );
    
    if (rc >= 0) {
        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        Start = (int)ovector[0];
        End = (int)ovector[1];
        Success = 1;
        
        /* Extract capture groups using the helper function */
        Groups = MR_list_empty();
        
        /* Build list in reverse order (last group first) */
        for (int i = rc - 1; i >= 1; i--) {
            MR_Word maybe_group = make_maybe_string(String, ovector[2*i], ovector[2*i+1]);
            Groups = MR_list_cons(maybe_group, Groups);
        }
        
        /* Extract named groups */
        NamedGroups = extract_named_groups(Regex->code, match_data, String);
    } else {
        Start = 0;
        End = 0;
        Success = 0;
        Groups = MR_list_empty();
        NamedGroups = (MR_Word) MR_mktag(0);
    }
    
    pcre2_match_data_free(match_data);
").

% Full match (entire string)
fullmatch(Pattern, String, Result) :-
    ( if compile("^(?:" ++ Pattern ++ ")$", ok(Regex)) then
        search_compiled(Regex, String, Result)
    else
        Result = no
    ).

fullmatch_compiled(Regex, String, Result) :-
    do_full_match(Regex, String, Success, Start, End, Groups, NamedGroups),
    ( if Success = 1 then
        Match = match(String, "", Start, End, Groups, NamedGroups),
        Result = yes(Match)
    else
        Result = no
    ).

:- pred do_full_match(regex::in, string::in, int::out, int::out, int::out, list(maybe(string))::out, map(string, maybe(string))::out) is det.
:- pragma foreign_proc("C",
    do_full_match(Regex::in, String::in, Success::out, Start::out, End::out, Groups::out, NamedGroups::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pcre2_match_data *match_data;
    int rc;
    int string_len = strlen(String);
    
    match_data = pcre2_match_data_create_from_pattern(Regex->code, NULL);
    
    rc = pcre2_match(
        Regex->code,
        (PCRE2_SPTR8)String,
        string_len,
        0,                    /* start offset */
        PCRE2_ANCHORED | PCRE2_ENDANCHORED,  /* must match entire string */
        match_data,
        Regex->mcontext
    );
    
    if (rc >= 0) {
        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        Start = (int)ovector[0];
        End = (int)ovector[1];
        /* For full match, should match the entire string */
        if (Start == 0 && End == string_len) {
            Success = 1;
            
            /* Extract capture groups */
            Groups = MR_list_empty();
            
            /* Build list in reverse order (last group first) */
            for (int i = rc - 1; i >= 1; i--) {
                MR_Word maybe_group = make_maybe_string(String, ovector[2*i], ovector[2*i+1]);
                Groups = MR_list_cons(maybe_group, Groups);
            }
            
            /* Extract named groups */
            NamedGroups = extract_named_groups(Regex->code, match_data, String);
        } else {
            Success = 0;
            Groups = MR_list_empty();
            NamedGroups = (MR_Word) MR_mktag(0);
        }
    } else {
        Start = 0;
        End = 0;
        Success = 0;
        Groups = MR_list_empty();
        NamedGroups = (MR_Word) MR_mktag(0);
    }
    
    pcre2_match_data_free(match_data);
").

% Simplified implementations for findall and finditer
findall(Pattern, String, Matches) :-
    ( if compile(Pattern, ok(Regex)) then
        findall_compiled(Regex, String, Matches)
    else
        Matches = []
    ).

findall_compiled(_Regex, _String, Matches) :-
    % Placeholder implementation - would need iterative matching
    Matches = [].

finditer(Pattern, String, Matches) :-
    ( if compile(Pattern, ok(Regex)) then
        finditer_compiled(Regex, String, Matches)
    else
        Matches = []
    ).

finditer_compiled(_Regex, _String, Matches) :-
    % Placeholder - would need iterative matching in C
    Matches = [].

%---------------------------------------------------------------------------%

% Substitution operations

sub(Pattern, Replacement, String, Result) :-
    sub(Pattern, Replacement, String, 0, Result).

sub(Pattern, Replacement, String, Count, Result) :-
    ( if compile(Pattern, ok(Regex)) then
        sub_compiled(Regex, Replacement, String, Count, Result)
    else
        Result = String
    ).

sub_compiled(Regex, Replacement, String, Result) :-
    sub_compiled(Regex, Replacement, String, 0, Result).

sub_compiled(_Regex, _Replacement, String, _Count, Result) :-
    % Placeholder implementation - real version would use pcre2_substitute
    Result = String.

subn(Pattern, Replacement, String, Count, Result, NumSubs) :-
    ( if compile(Pattern, ok(Regex)) then
        subn_compiled(Regex, Replacement, String, Count, Result, NumSubs)
    else
        Result = String,
        NumSubs = 0
    ).

subn_compiled(_Regex, _Replacement, String, _Count, Result, NumSubs) :-
    % Placeholder implementation
    Result = String,
    NumSubs = 0.

%---------------------------------------------------------------------------%

% String splitting operations

split(Pattern, String, Parts) :-
    split(Pattern, String, 0, Parts).

split(Pattern, String, MaxSplit, Parts) :-
    ( if compile(Pattern, ok(Regex)) then
        split_compiled(Regex, String, MaxSplit, Parts)
    else
        Parts = [String]
    ).

split_compiled(Regex, String, Parts) :-
    split_compiled(Regex, String, 0, Parts).

split_compiled(_Regex, String, _MaxSplit, Parts) :-
    % Placeholder implementation
    Parts = [String].

%---------------------------------------------------------------------------%

% Match object operations

group(Match, GroupNum, Group) :-
    Match = match(_, _, _, _, Groups, _),
    ( if GroupNum = 0 then
        % Group 0 is the entire match
        Match = match(String, _, Start, End, _, _),
        string.between(String, Start, End, Substring),
        Group = yes(Substring)
    else if list.index0(Groups, GroupNum - 1, MaybeGroup) then
        Group = MaybeGroup
    else
        Group = no
    ).

groups(Match, Groups) :-
    Match = match(_, _, _, _, AllGroups, _),
    Groups = AllGroups.

span(Match, GroupNum, Span) :-
    Match = match(_, _, Start, End, _, _),
    ( if GroupNum = 0 then
        Span = yes({Start, End})
    else
        % Would need to track individual group positions
        Span = no
    ).

start(Match, GroupNum, Position) :-
    ( if span(Match, GroupNum, yes({S, _})) then
        Position = yes(S)
    else
        Position = no
    ).

end(Match, GroupNum, Position) :-
    ( if span(Match, GroupNum, yes({_, E})) then
        Position = yes(E)
    else
        Position = no
    ).

group_by_name(Match, Name, Group) :-
    Match = match(_, _, _, _, _, NamedGroups),
    ( if map.search(NamedGroups, Name, FoundGroup) then
        Group = FoundGroup
    else
        Group = no
    ).

named_group_names(Match, Names) :-
    Match = match(_, _, _, _, _, NamedGroups),
    map.keys(NamedGroups, Names).

%---------------------------------------------------------------------------%

% Utility predicates

escape(String, Escaped) :-
    string.foldl(escape_char, String, "", Escaped).

:- pred escape_char(char::in, string::in, string::out) is det.
escape_char(C, Acc, Result) :-
    ( if char_needs_escape(C) then
        Result = Acc ++ "\\" ++ string.from_char(C)
    else
        Result = Acc ++ string.from_char(C)
    ).

:- pred char_needs_escape(char::in) is semidet.
char_needs_escape('\\').
char_needs_escape('^').
char_needs_escape('$').
char_needs_escape('.').
char_needs_escape('*').
char_needs_escape('+').
char_needs_escape('?').
char_needs_escape('{').
char_needs_escape('}').
char_needs_escape('[').
char_needs_escape(']').
char_needs_escape('(').
char_needs_escape(')').
char_needs_escape('|').

%---------------------------------------------------------------------------%

% Helper predicates for C foreign code to manipulate Mercury maps

:- pred init_string_map(map(string, maybe(string))::out) is det.
init_string_map(Map) :-
    map.init(Map).

:- pred insert_into_string_map(string::in, maybe(string)::in, 
    map(string, maybe(string))::in, map(string, maybe(string))::out) is det.
insert_into_string_map(Key, Value, !Map) :-
    map.set(Key, Value, !Map).

% Export these predicates so they can be called from C
:- pragma foreign_export("C", init_string_map(out), "mercury_regex_init_string_map").
:- pragma foreign_export("C", insert_into_string_map(in, in, in, out), 
    "mercury_regex_insert_into_string_map").

%---------------------------------------------------------------------------%
:- end_module regex.
%---------------------------------------------------------------------------%
