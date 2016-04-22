! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs assocs.extras combinators
combinators.short-circuit constructors continuations fry
io.encodings.utf8 io.files kernel locals macros make math
math.order modern.paths modern.slices multiline namespaces
quotations sequences sequences.extras splitting
splitting.monotonic strings unicode ;
IN: modern

<<
! Base rules, everything should have a generator macro
TUPLE: lexer generator ;

! Declarative rules, add more!
TUPLE: tag-lexer < lexer ; ! default, if nothing else matches, add one with regexp for c-style names etc
TUPLE: dquote-lexer < lexer delimiter escape ignore-whitespace? ; ! ``close`` slot someday to allow ` '
TUPLE: matched-lexer < lexer delimiter double-char ; ! ``close`` slot someday, to allow `` ''
TUPLE: backtick-lexer < lexer delimiter ;
TUPLE: backslash-lexer < lexer delimiter payload-exception? ; ! payload-exception is \n words
TUPLE: line-comment-lexer < lexer delimiter word-name-exception? ; ! escape-newline-exception? (like C)
TUPLE: colon-lexer < lexer delimiter ;
TUPLE: semicolon-lexer < lexer delimiter ; ! ; inline foldable
TUPLE: whitespace-lexer < lexer delimiter ; ! \s \r \n \t?
TUPLE: terminator-lexer < lexer delimiter ;
TUPLE: decorator-lexer < lexer delimiter ;

! Base lexer result
TUPLE: literal underlying seq lexer left-decorators right-decorators ;
TUPLE: tag-literal < literal tag ;
TUPLE: matched-literal < tag-literal delimiter payload closing-tag ;
TUPLE: delimited-literal < tag-literal delimiter payload ;
TUPLE: decorator-literal < literal delimiter payload ;

TUPLE: dquote-literal < delimited-literal ;
TUPLE: single-matched-literal < matched-literal ;
TUPLE: double-matched-literal < matched-literal ;
TUPLE: uppercase-colon-literal < single-matched-literal ;
TUPLE: lowercase-colon-literal < delimited-literal ;
TUPLE: backtick-literal < delimited-literal ;
TUPLE: backslash-literal < delimited-literal ;
TUPLE: semicolon-literal < delimited-literal ;
TUPLE: line-comment-literal < delimited-literal ;
TUPLE: terminator-literal < tag-literal ;
TUPLE: whitespace-literal < tag-literal ;

TUPLE: left-decorator-literal < decorator-literal ;
TUPLE: right-decorator-literal < decorator-literal ;

TUPLE: compound-sequence-literal sequence ;
CONSTRUCTOR: <compound-sequence-literal> compound-sequence-literal ( sequence -- obj ) ;
>>

GENERIC: lexed-underlying ( obj -- slice )
M: f lexed-underlying ;
M: object lexed-underlying underlying>> ;
M: slice lexed-underlying ;

TUPLE: compound-literal sequence ;
CONSTRUCTOR: <compound-literal> compound-literal ( sequence -- obj ) ;

! Ensure that we only have one decorated thing in a compound-literal
ERROR: bad-compound-literal seq decorators words ;
: check-compound-literal ( seq -- seq ) ;

GENERIC: make-compound-literals ( seq -- seq' )
M: object make-compound-literals ;
M: array make-compound-literals
    [
        {
            [ [ lexed-underlying ] bi@ slices-touch? ]
            [ [ ] [ left-decorator-literal? ] bi* and ]
            [ [ right-decorator-literal? ] [ ] bi* and ]
        } 2||
    ] monotonic-split
    [ dup length 1 > [ <compound-literal> ] [ first ] if ] map ;

! We have empty decorators, just the @ right here
! wrap the decorated object in the payload slot
GENERIC: collapse-decorators ( seq -- seq' )
M: object collapse-decorators ;
M: array collapse-decorators
    [
        {
            [ [ left-decorator-literal? ] [ ] bi* and ]
            [ [ ] [ right-decorator-literal? ] bi* and ]
        } 2||
    ] monotonic-split
    [
        dup length 1 > [
            first2
            2dup [ left-decorator-literal? ] [ ] bi* and [
                >>payload
            ] [
                [ payload<< ] keep
            ] if
        ] [
            first
        ] if
    ] map ;

: split-double-dash ( seq -- seqs )
    dup [ { [ tag-literal? ] [ tag>> "--" = ] } 1&& ] split-when
    dup length 1 > [
        nip <compound-sequence-literal>
    ] [
        drop
    ] if ;

: postprocess-lexed ( seq -- seq' )
    collapse-decorators make-compound-literals ;


ERROR: whitespace-expected-after n string ch ;
ERROR: expected-more-tokens n string expected ;
ERROR: string-expected-got-eof n string ;

:: make-tag-literal ( tag -- literal )
    tag-literal new
        tag >string >>tag
        tag >>underlying
        tag 1array >>seq ; inline

:: make-tag-class-literal ( tag class -- literal )
    class new
        tag >string >>tag
        tag >>underlying
        tag 1array >>seq ; inline

:: make-tag-payload-literal ( payload last tag class -- literal )
    class new
        tag >string >>tag
        payload >string >>payload
        tag last [ dup tag-literal? [ lexed-underlying ] when ] bi@ ?span-slices >>underlying
        tag payload 2array >>seq ; inline

:: make-delimited-literal ( payload last tag delimiter class -- literal )
    class new
        tag >string >>tag
        payload dup slice? [ >string ] when >>payload
        tag last [ dup tag-literal? [ lexed-underlying ] when ] bi@ ?span-slices >>underlying
        delimiter >string >>delimiter
        tag delimiter payload 3array >>seq ; inline

ERROR: mismatched-closing opening closing ;
:: make-matched-literal ( payload closing tag opening-delimiter class -- literal )
    class new
        tag >string >>tag
        payload postprocess-lexed opening-delimiter "\"" = [ split-double-dash ] unless >>payload
        tag closing [ dup tag-literal? [ lexed-underlying ] when ] bi@ ?span-slices >>underlying
        opening-delimiter >string >>delimiter
        dup single-matched-literal? [
            closing tag>> length 1 > [
                tag opening-delimiter append
                matching-delimiter-string closing tag>> sequence= [ opening-delimiter closing tag>> mismatched-closing ] unless
            ] when
            closing tag>> >>closing-tag
        ] when
        tag opening-delimiter payload closing 4array >>seq ; inline

:: make-decorator-literal ( payload delimiter class -- literal )
    class new
        delimiter >>delimiter
        payload >>payload
        payload delimiter [ lexed-underlying ] bi@ ?span-slices >>underlying
        class left-decorator-literal = [
            delimiter payload 2array
        ] [
            payload delimiter 2array
        ] if >>seq ; inline

:: make-decorator-sentinel ( delimiter left? -- literal )
    left? left-decorator-literal right-decorator-literal ? new
        delimiter >>delimiter
        delimiter 1array >>seq
        delimiter >>underlying ; inline

ERROR: long-opening-mismatch tag open n string ch ;

! (( )) [[ ]] {{ }}
MACRO:: read-double-matched ( open-ch -- quot: ( n string tag ch -- n' string seq ) )
    open-ch dup matching-delimiter {
        [ drop 2 swap <string> ]
        [ drop 1string ]
        [ nip 2 swap <string> ]
    } 2cleave :> ( openstr2 openstr1 closestr2 )
    [| n string tag! ch |
        ch {
            { CHAR: = [
                n string openstr1 slice-til-separator-inclusive [ -1 modify-from ] dip :> ( n' string' opening ch )
                ch open-ch = [ tag openstr2 n string ch long-opening-mismatch ] unless
                opening matching-delimiter-string :> needle

                n' string' needle slice-til-string :> ( n'' string'' payload closing )
                n'' string
                payload closing tag opening double-matched-literal make-matched-literal
            ] }
            { open-ch [
                tag 1 cut-slice* swap tag! 1 modify-to :> opening
                n 1 + string closestr2 slice-til-string :> ( n' string' payload closing )
                n' string
                payload closing tag opening double-matched-literal make-matched-literal
            ] }
            [ [ tag openstr2 n string ] dip long-opening-mismatch ]
        } case
     ] ;

: read-double-matched-paren ( n string tag ch -- n' string seq ) CHAR: ( read-double-matched ;
: read-double-matched-bracket ( n string tag ch -- n' string seq ) CHAR: [ read-double-matched ;
: read-double-matched-brace ( n string tag ch -- n' string seq ) CHAR: { read-double-matched ;

DEFER: lex
DEFER: lex-factor
ERROR: lex-expected-but-got-eof n string expected ;
! For implementing [ { (
: lex-until ( n string tags -- n' string payload closing )
    pick [
        3dup '[
            [
                lex-factor dup , [
                    dup tag-literal? [
                        ! } gets a chance, but then also full seq { } after recursion...
                        [ _ ] dip underlying>> '[ _ sequence= ] any? not
                    ] [
                        drop t ! loop again?
                    ] if
                ] [
                    _ _ _ lex-expected-but-got-eof
                ] if*
            ] loop
        ] { } make unclip-last
    ] [
        lex-expected-but-got-eof
    ] if ;

MACRO:: read-matched ( ch -- quot: ( n string tag -- n' string slice' ) )
    ch dup matching-delimiter {
        [ drop "=" swap prefix ]
        [ nip 1string ]
    } 2cleave :> ( openstreq closestr1 )  ! [= ]
    [| n string tag |
        n string tag
        2over nth-check-eof {
            { [ dup openstreq member? ] [ ch read-double-matched ] } ! (=( or ((
            { [ dup blank? ] [ drop dup '[ _ matching-delimiter-string closestr1 2array lex-until ] dip 1 cut-slice* single-matched-literal make-matched-literal ] } ! ( foo )
            [ drop [ slice-til-whitespace drop ] dip span-slices make-tag-literal ]  ! (foo)
        } cond
    ] ;

: read-bracket ( n string slice -- n' string slice' ) CHAR: [ read-matched ;
: read-brace ( n string slice -- n' string slice' ) CHAR: { read-matched ;
: read-paren ( n string slice -- n' string slice' ) CHAR: ( read-matched ;

: read-backtick ( n string opening -- n' string obj )
    [
        slice-til-whitespace drop
        dup
    ] dip 1 cut-slice* backtick-literal make-delimited-literal ;

: read-string-payload ( n string -- n' string )
    over [
        { CHAR: \ CHAR: " } slice-til-separator-inclusive {
            { f [ drop ] }
            { CHAR: " [ drop ] }
            { CHAR: \ [ drop next-char-from drop read-string-payload ] }
        } case
    ] [
        string-expected-got-eof
    ] if ;

:: read-string ( n string tag -- n' string seq )
    n string read-string-payload drop :> n'
    n' string
    n' [ n string string-expected-got-eof ] unless
    n n' 1 - string <slice>
    n' 1 - n' string <slice>
    tag 1 cut-slice* dquote-literal make-matched-literal ;

: take-comment ( n string slice -- n' string comment )
    2over ?nth CHAR: [ = [
        [ 1 + ] 2dip 2over ?nth read-double-matched-bracket
    ] [
        [ slice-til-eol-from drop dup ] dip 1 cut-slice* line-comment-literal make-delimited-literal
    ] if ;

: read-til-semicolon ( n string slice -- n' string semi )
    dup '[ but-last ";" append ";" 2array lex-until ] dip
    1 cut-slice* uppercase-colon-literal make-matched-literal ;

: read-word-or-til-semicolon ( n string slice -- n' string obj )
    2over next-char-from* "\s\r\n" member? [
        read-til-semicolon
    ] [
        merge-slice-til-whitespace make-tag-literal
    ] if ;

: read-lowercase-colon ( n string slice -- n' string lowercase-colon )
    [ lex-factor dup ] dip 1 cut-slice*
    lowercase-colon-literal make-delimited-literal ;

: strict-upper? ( string -- ? )
    [ { [ CHAR: A CHAR: Z between? ] [ "#:-" member? ] } 1|| ] all? ;

ERROR: colon-word-must-be-all-uppercase-or-lowercase n string word ;
: read-colon ( n string slice -- n' string colon )
    dup length 1 = [
        dup prev-char-from-slice { CHAR: \s CHAR: \r CHAR: \n f } member? [
            read-til-semicolon
        ] [
            read-lowercase-colon
        ] if
    ] [
        {
            { [ dup strict-upper? ] [ read-til-semicolon ] }
            [ read-lowercase-colon ]
        } cond
    ] if ;

! Words like append! and suffix! are allowed for now.
: read-exclamation ( n string slice -- n' string obj )
    dup { [ "!" sequence= ] [ "#!" sequence= ] } 1||
    [ take-comment ] [ merge-slice-til-whitespace make-tag-literal ] if ;

ERROR: backslash-expects-whitespace slice ;
: read-backslash ( n string slice -- n' string obj )
    2over peek-from blank? [
        ! \ foo, M\ foo
        [ skip-blank-from slice-til-whitespace drop dup ] dip 1 cut-slice* backslash-literal make-delimited-literal
    ] [
        ! M\N
        merge-slice-til-whitespace make-tag-literal
    ] if ;

! If the slice is 0 width, we stopped on whitespace.
! Advance the index and read again!
: read-token-or-whitespace ( n string slice -- n' string slice )
    dup length 0 =
    [ drop [ 1 + ] dip lex-factor ]
    [ make-tag-literal ] if ;

ERROR: mismatched-terminator n string slice ;
: read-terminator ( n string slice -- n' string slice )
    terminator-literal make-tag-class-literal ;

: ?blank? ( ch/f -- blank/f )
    { [ blank? ] [ f = ] } 1|| ;

<PRIVATE
! work on underlying, index is on the @
! @foo
: left-decorator? ( obj -- ? )
    {
        [ char-before-slice ?blank? ]
        [ next-char-from-slice ?blank? not ]
    } 1&& ;

! foo@
: right-decorator? ( slice -- ? )
    {
        [ prev-char-from-slice-end ?blank? not ]
        [ next-char-from-slice ?blank? ]
    } 1&& ;

PRIVATE>

: read-decorator ( n string slice -- n' string obj )
    {
        { [ dup left-decorator? ] [ t make-decorator-sentinel ] }
        { [ dup right-decorator? ] [
            dup length 1 > [
                [ -1 + ] 2dip
                -1 modify-to make-tag-literal
            ] [
                f make-decorator-sentinel
            ] if ] }
        [ make-tag-literal ]
    } cond ;

SYMBOL: lexing-delimiters

: add-lexing-delimiter ( rule -- )
    [ ] [ delimiter>> ] bi lexing-delimiters get set-once-at ;

<<
: lexer-rules>hashtable ( seq -- obj )
    H{ } clone lexing-delimiters [
        [ add-lexing-delimiter ] each
        lexing-delimiters get
    ] with-variable ;

: lexer-rules>delimiters ( seq -- string )
    [ delimiter>> ] "" map-as ;

: lexer-rules>assoc ( seq -- seq' )
    [ [ delimiter>> ] [ generator>> 1quotation ] bi ] { } map>assoc ;
>>

MACRO: rules>call-lexer ( seq -- quot: ( n/f string -- n'/f string literal ) )
    [ lexer-rules>delimiters ]
    [
        lexer-rules>assoc
        { f [ f like dup [ make-tag-literal ] when ] } suffix
    ] bi
    '[ _ slice-til-either _ case ] ;

CONSTANT: factor-lexing-rules {
    T{ line-comment-lexer { generator read-exclamation } { delimiter CHAR: ! } }
    T{ backtick-lexer { generator read-backtick } { delimiter CHAR: ` } }
    T{ backslash-lexer { generator read-backslash } { delimiter CHAR: \ } }
    T{ dquote-lexer { generator read-string } { delimiter CHAR: " } { escape CHAR: \ } }
    T{ decorator-lexer { generator read-decorator } { delimiter CHAR: @ } }
    
    T{ colon-lexer { generator read-colon } { delimiter CHAR: : } }
    T{ matched-lexer { generator read-bracket } { delimiter CHAR: [ } }
    T{ matched-lexer { generator read-brace } { delimiter CHAR: { } }
    T{ matched-lexer { generator read-paren } { delimiter CHAR: ( } }
    
    T{ terminator-lexer { generator read-terminator } { delimiter CHAR: ; } }
    T{ terminator-lexer { generator read-terminator } { delimiter CHAR: ] } }
    T{ terminator-lexer { generator read-terminator } { delimiter CHAR: } } }
    T{ terminator-lexer { generator read-terminator } { delimiter CHAR: ) } }
    
    T{ whitespace-lexer { generator read-token-or-whitespace } { delimiter CHAR: \s } }
    T{ whitespace-lexer { generator read-token-or-whitespace } { delimiter CHAR: \r } }
    T{ whitespace-lexer { generator read-token-or-whitespace } { delimiter CHAR: \n } }
}

: lex-factor ( n/f string -- n'/f string literal )
    factor-lexing-rules rules>call-lexer ;

: string>literals ( string -- sequence )
    [ 0 ] dip [ lex-factor ] loop>array 2nip postprocess-lexed ;

: vocab>literals ( vocab -- sequence )
    ".private" ?tail drop
    modern-source-path utf8 file-contents string>literals ;

: path>literals ( path -- sequence )
    utf8 file-contents string>literals ;

: lex-core ( -- assoc )
    core-bootstrap-vocabs [ [ vocab>literals ] [ nip ] recover ] map-zip ;

: filter-lex-errors ( assoc -- assoc' )
    [ nip array? not ] assoc-filter ;


/*
! What a lexer body looks like, produced by make-lexer
: lex ( n/f string -- n'/f string literal )
    "!`\\\"[{(\s\r\n" slice-til-either {
        { CHAR: ! [ read-exclamation ] }
        { CHAR: ` [ read-backtick ] }
        { CHAR: \ [ read-backslash ] }
        { CHAR: " [ read-string ] }
        { CHAR: [ [ read-bracket ] }
        { CHAR: { [ read-brace ] }
        { CHAR: ( [ read-paren ] }
        { CHAR: \s [ read-token-or-whitespace ] }
        { CHAR: \r [ read-token-or-whitespace ] }
        { CHAR: \n [ read-token-or-whitespace ] }
        { f [ f like dup [ make-tag-literal ] when ] }
    } case ; inline
*/
