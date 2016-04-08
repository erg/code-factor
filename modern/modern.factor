! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs assocs.extras combinators
combinators.short-circuit continuations fry io.encodings.utf8
io.files kernel locals macros make math modern.paths
modern.slices multiline namespaces quotations sequences
sequences.extras sorting splitting strings unicode ;
IN: modern

! Base rules, everything should have a generator macro and optional sublexers
TUPLE: lexer generator ;
TUPLE: lexer-with-sublexers < lexer sublexers ;

! Declarative rules, add more!
TUPLE: tag-lexer < lexer ; ! default, if nothing else matches, add one with regexp for c-style names etc
TUPLE: exact-tag-lexer < lexer token ;
TUPLE: dquote-lexer < lexer delimiter escape ignore-whitespace? ; ! ``close`` slot someday to allow ` '
TUPLE: matched-lexer < lexer-with-sublexers delimiter double-char ; ! ``close`` slot someday, to allow `` ''
TUPLE: backtick-lexer < lexer delimiter ;
TUPLE: backslash-lexer < lexer delimiter payload-exception? ; ! payload-exception is \n words
TUPLE: line-comment-lexer < lexer delimiter word-name-exception? ; ! escape-newline-exception? (like C)
TUPLE: whitespace-lexer < lexer delimiter ; ! \s \r \n \t?

! Base lexer result
TUPLE: literal underlying seq lexer ;
TUPLE: tag-literal < literal tag ;
TUPLE: matched-literal < tag-literal delimiter payload ;
TUPLE: delimited-literal < tag-literal delimiter payload ;

TUPLE: exact-tag-literal < tag-literal ;
TUPLE: dquote-literal < delimited-literal ;
TUPLE: single-matched-literal < matched-literal ;
TUPLE: double-matched-literal < matched-literal ;
TUPLE: backtick-literal < delimited-literal ;
TUPLE: backslash-literal < delimited-literal ;
TUPLE: line-comment-literal < delimited-literal ;
TUPLE: whitespace-literal < tag-literal ;

TUPLE: compound-literal sequence ;

GENERIC: lexed-underlying ( obj -- slice )
M: f lexed-underlying ;
M: object lexed-underlying underlying>> ;
M: slice lexed-underlying ;

ERROR: unknown-literal ch ;
<<
: lookup-literal ( ch -- literal )
    H{
        { CHAR: [ matched-literal }
        { CHAR: { matched-literal }
        { CHAR: ( matched-literal }
        { CHAR: " dquote-literal }
        { CHAR: ` backtick-literal }
        { CHAR: \ backslash-literal }
    } clone ?at [ unknown-literal ] unless ;
>>

ERROR: whitespace-expected-after n string ch ;
ERROR: subseq-expected-but-got-eof n string expected ;
ERROR: expected-more-tokens n string expected ;
ERROR: string-expected-got-eof n string ;

:: make-tag-literal ( tag -- literal )
    tag-literal new
        tag >string >>tag
        tag >>underlying
        tag 1array >>seq ; inline

:: make-tag-payload-literal ( payload last tag class -- literal )
    class new
        tag >string >>tag
        payload >string >>payload
        tag last [ dup tag-literal? [ lexed-underlying ] when ] bi@ span-slices >>underlying
        tag payload 2array >>seq ; inline

:: make-delimited-literal ( payload last tag delimiter class -- literal )
    class new
        tag >string >>tag
        payload >string >>payload
        tag last [ dup tag-literal? [ lexed-underlying ] when ] bi@ span-slices >>underlying
        delimiter >string >>delimiter
        tag delimiter payload 3array >>seq ; inline

:: make-matched-literal ( payload closing tag opening class -- literal )
    class new
        tag >string >>tag
        payload >>payload
        tag closing [ dup tag-literal? [ lexed-underlying ] when ] bi@ span-slices >>underlying
        opening >string >>delimiter
        tag opening payload closing 4array >>seq ; inline

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
                n string openstr1 slice-until-separator-inclusive [ -1 modify-from ] dip :> ( n' string' opening ch )
                ch open-ch = [ tag openstr2 n string ch long-opening-mismatch ] unless
                opening matching-delimiter-string :> needle

                n' string' needle slice-until-string :> ( n'' string'' payload closing )
                n'' string
                payload closing tag opening double-matched-literal make-matched-literal
            ] }
            { open-ch [
                tag 1 cut-slice* swap tag! 1 modify-to :> opening
                n 1 + string closestr2 slice-until-string :> ( n' string' payload closing )
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
: lex-until ( n string token -- n' string payload closing )
    pick [
        3dup '[
            [
                lex-factor dup , [
                    dup tag-literal? [
                        underlying>> _ sequence= not
                    ] [
                        drop t  ! loop again?
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
        [ drop lookup-literal ]
    } 2cleave :> ( openstreq closestr1 literal )  ! [= ]
    [| n string tag |
        n string tag
        2over nth-check-eof {
            { [ dup openstreq member? ] [ ch read-double-matched ] } ! (=( or ((
            { [ dup blank? ] [ drop [ closestr1 lex-until ] dip 1 cut-slice* single-matched-literal make-matched-literal ] } ! ( foo )
            [ drop [ slice-until-whitespace drop ] dip span-slices make-tag-literal ]  ! (foo)
        } cond
    ] ;

: read-paren ( n string slice -- n' string slice' ) CHAR: ( read-matched ;
: read-brace ( n string slice -- n' string slice' ) CHAR: { read-matched ;
: read-bracket ( n string slice -- n' string slice' ) CHAR: [ read-matched ;

: read-backtick ( n string opening -- n' string obj )
    [
        slice-until-whitespace drop
        dup
    ] dip 1 cut-slice* backtick-literal make-delimited-literal ;

: read-string-payload ( n string -- n' string )
    over [
        { CHAR: \ CHAR: " } slice-until-separator-inclusive {
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

! Words like append! and suffix! are allowed for now.
: read-exclamation ( n string slice -- n' string obj )
    dup { [ "!" sequence= ] [ "#!" sequence= ] } 1||
    [ take-comment ] [ merge-slice-until-whitespace make-tag-literal ] if ;

ERROR: backslash-expects-whitespace slice ;
: read-backslash ( n string slice -- n' string obj )
    2over peek-from blank? [
        ! \ foo, M\ foo
        [ skip-blank-from slice-until-whitespace drop dup ] dip 1 cut-slice* backslash-literal make-delimited-literal
    ] [
        ! M\N
        merge-slice-until-whitespace make-tag-literal
    ] if ;

! If the slice is 0 width, we stopped on whitespace.
! Advance the index and read again!
: read-token-or-whitespace ( n string slice -- n' string slice )
    dup length 0 =
    [ drop [ 1 + ] dip lex-factor ]
    [ make-tag-literal ] if ;

! MACRO:: make-read-token-or-whitespace ( lex -- quot: ( n string slice -- n' string slice ) )
!     [
!        dup length 0 =
!        [ drop [ 1 + ] dip lex ]
!        [ make-tag-literal ] if ;
!    ] ;

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

MACRO: make-lexer ( seq -- quot: ( n/f string -- n'/f string literal ) )
    [ lexer-rules>delimiters ]
    [
        lexer-rules>assoc
        { f [ f like dup [ make-tag-literal ] when ] } suffix
    ] bi
    '[ _ slice-until-either _ case ] ;

CONSTANT: factor-lexing-rules {
    T{ line-comment-lexer { generator read-exclamation } { delimiter CHAR: ! } }
    T{ backtick-lexer { generator read-backtick } { delimiter CHAR: ` } }
    T{ backslash-lexer { generator read-backslash } { delimiter CHAR: \ } }
    T{ dquote-lexer { generator read-string } { delimiter CHAR: " } { escape CHAR: \ } }
    T{ matched-lexer { generator read-bracket } { delimiter CHAR: [ } }
    T{ matched-lexer { generator read-brace } { delimiter CHAR: { } }
    T{ matched-lexer { generator read-paren } { delimiter CHAR: ( } }
    T{ whitespace-lexer { generator read-token-or-whitespace } { delimiter CHAR: \s } }
    T{ whitespace-lexer { generator read-token-or-whitespace } { delimiter CHAR: \r } }
    T{ whitespace-lexer { generator read-token-or-whitespace } { delimiter CHAR: \n } }
}

: lex-factor ( n/f string -- n'/f string literal )
    factor-lexing-rules make-lexer ;

: string>literals ( string -- sequence )
    [ 0 ] dip [ lex-factor ] loop>array 2nip ;

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
    "!`\\\"[{(\s\r\n" slice-until-either {
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
