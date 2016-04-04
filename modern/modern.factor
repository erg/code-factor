! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs assocs.extras combinators
combinators.short-circuit fry io.encodings.utf8 io.files kernel
locals make math modern.paths modern.slices namespaces sequences
sequences.extras sorting splitting strings ;
IN: modern

TUPLE: lexed rule tag payload underlying ;

TUPLE: string-lexer delimiter escape ;      ! url"lol.com"
TUPLE: matching-lexer delimiter ;           ! foo[ ] foo[[ ]] foo[lol[ ]lol]
TUPLE: backtick-lexer delimiter ;           ! fixnum`3           ! has no space after
TUPLE: backslash-lexer delimiter ;          ! word\ something    ! has a space after
TUPLE: til-eol-lexer delimiter ;            ! TODO# Fix the lexer, TODO#[lol[omg]lol]
TUPLE: standalone-only-lexer delimiter ;    ! example:   ! comment   word!

TUPLE: string-lexed < lexed ;
TUPLE: matching-lexed < lexed ;
TUPLE: backtick-lexed < lexed ;
TUPLE: backslash-lexed < lexed ;
TUPLE: til-eol-lexed < lexed ;
TUPLE: standalone-only-lexed < lexed ;

TUPLE: single-literal < lexed ;
TUPLE: double-literal < lexed ;
TUPLE: string-literal < lexed ;
TUPLE: backtick-literal < lexed ;
TUPLE: backslash-literal < lexed ;
TUPLE: til-eol-literal < lexed ;

ERROR: whitespace-expected-after n string ch ;
ERROR: subseq-expected-but-got-eof n string expected ;
ERROR: string-expected-got-eof n string ;
ERROR: unexpected-eof n string expected ;
ERROR: expected-more-tokens n string expected ;

: make-literal ( tag payload class -- literal )
    new
        swap >>payload
        swap >>tag ; inline
<<
: setup-long-macro ( ch -- openstr2 openstr1 closestr1 closestr2 )
    dup matching-char {
        [ drop 2 swap <string> ]
        [ drop 1string ]
        [ nip 1string ]
        [ nip 2 swap <string> ]
    } 2cleave ;
>>

ERROR: long-opening-mismatch tag open n string ch ;

! (( )) [[ ]] {{ }}
MACRO:: read-long ( open-ch -- quot )
    open-ch setup-long-macro :> ( openstr2 openstr1 closestr1 closestr2 )
    [| n string tag ch |
        ch {
            { CHAR: = [
                n string openstr1 slice-until-separator-inclusive :> (  n' string' tag2 ch )
                ch open-ch = [ tag openstr2 n string ch long-opening-mismatch ]  unless
                tag2 length 1 - CHAR: = <string> closestr1 closestr1 surround :> needle

                n' string' needle slice-until-string :> ( n'' string'' payload end )
                n'' string
                tag -1 modify-to payload double-literal make-literal
            ] }
            { open-ch [
                n 1 + string closestr2 slice-until-string :> ( n' string' payload end )
                n' string
                tag -1 modify-to
                payload
                double-literal make-literal
            ] }
            [ [ tag openstr2 n string ] dip long-opening-mismatch ]
        } case
     ] ;

: read-long-paren ( n string tag ch -- n string seq )
    CHAR: ( read-long ;

: read-long-bracket ( n string tag ch -- n string seq )
    CHAR: [ read-long ;

: read-long-brace ( n string tag ch -- n string seq )
    CHAR: { read-long ;

DEFER: lex

ERROR: lex-expected-but-got-eof n string expected ;
! For implementing [ { (
: lex-until ( n string token -- n' string payload closing )
    pick [
        3dup '[
            [
                lex dup , [
                    dup slice? [
                        _ sequence= not
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

<<
: setup-single-macro ( ch -- openstreq closestr1 )
    dup matching-char {
        [ drop "=" swap prefix ]
        [ nip 1string ]
    } 2cleave ;
>>

MACRO:: read-matching-typed ( ch -- quot )
    ch setup-single-macro :> ( openstreq closestr1 )
    [| n string seq |
        n string seq
        2over closestr1 nth-check-eof {
            { [ dup openstreq member? ] [ ch read-long ] }
            [ drop [ closestr1 lex-until drop ] dip -1 modify-to swap lexed make-literal ]
        } cond
    ] ;

: read-paren ( n string seq -- n' string seq )
    CHAR: ( read-matching-typed ;

: read-brace ( n string seq -- n' string seq )
    CHAR: { read-matching-typed ;

: read-bracket ( n string seq -- n' string seq )
    CHAR: [ read-matching-typed ;


: read-backtick ( n string opening -- n' string obj )
    [ slice-until-whitespace drop ] dip
    backtick-literal new
        swap but-last-slice >>tag
        swap >>payload
        dup [ tag>> ] [ payload>> ] bi span-slices >>underlying ;

: read-string-payload ( n string -- n' string )
    over [
        { CHAR: \ CHAR: " } slice-until-separator-inclusive {
            { f [ drop ] }
            { CHAR: " [ drop ] }
            { CHAR: \ [ next-char-from 2drop read-string-payload ] }
        } case
    ] [
        string-expected-got-eof
    ] if ;

:: read-string ( n string name -- n' string seq )
    n string read-string-payload drop :> n'
    n' string
    name [ from>> ] [ to>> 1 - ] [ seq>> ] tri <slice>
    n n' 1 - string <slice>
    string-literal make-literal ;

: take-comment ( n string slice -- n' string comment )
    2over ?nth CHAR: [ = [
        1 modify-to
        [ 1 + ] 2dip 2over ?nth read-long-bracket
    ] [
        drop slice-til-eol-from drop "" swap til-eol-literal make-literal
    ] if ;

! Words like append! and suffix! are allowed for now.
: read-exclamation ( n string slice -- n' string obj )
    dup { [ "!" sequence= ] [ "#!" sequence= ] } 1||
    [ take-comment ] [ merge-slice-until-whitespace ] if ;

ERROR: backslash-unexpected-eof slice n string ;
: read-backslash' ( n string ch -- n' string obj )
    [ skip-one-space-after skip-blank-from ] dip ! skip at least one space, then skip all blanks after that
    [ slice-until-whitespace drop ] dip over length 0 > [
        backslash-literal new
            swap >>payload
            swap >>tag
     ] [
        backslash-unexpected-eof
     ] if ;

: read-backslash ( n string ch -- n' string obj )
    dup "\\" head? [ read-backslash' ] [ merge-slice-until-whitespace ] if ;

! If we got more than 1 char, we got a real token, return it.
! 1 char or fewer, we got a whitespace, try token again.
: read-token-or-whitespace ( n string tok -- n string tok )
    dup length 0 = [ drop [ 1 + ] dip lex ] when ;



CONSTANT: factor-lexing-rules {
    T{ til-eol-lexer f CHAR: # }
    ! T{ standalone-or-word-lexer f CHAR: # }
    T{ backslash-lexer f CHAR: \ }
    T{ backtick-lexer f CHAR: ` }
    T{ string-lexer f CHAR: " CHAR: \ }
    T{ matching-lexer f CHAR: [ }
    T{ matching-lexer f CHAR: { }
    T{ matching-lexer f CHAR: ( }
}

SYMBOL: lexing-delimiters

: add-lexing-delimiter ( rule -- )
    [ ] [ delimiter>> ] bi lexing-delimiters get set-once-at ;

: lexer-rules>hashtable ( seq -- obj )
    H{ } clone lexing-delimiters [
        [ add-lexing-delimiter ] each
        lexing-delimiters get
    ] with-variable ;

: lexer-rules-delimiters ( hashtable -- seq )
    keys natural-sort "\r\n " "" append-as ;


: lex ( n/f string -- n'/f string token )
    over [
        ! XXX: order matteres, specifically comments vs #[[ ]]
        ! seq n string ch
        "!`()[]{}\"\s\r\n\\" slice-until-either {
            { CHAR: ! [ read-exclamation ] }
            { CHAR: ` [ read-backtick ] }
            { CHAR: " [ read-string ] }
            { CHAR: [ [ read-bracket ] }
            { CHAR: ] [ read-closing ] }
            { CHAR: { [ read-brace ] }
            { CHAR: } [ read-closing ] }
            { CHAR: ( [ read-paren ] }
            { CHAR: ) [ read-closing ] }
            { CHAR: \ [ read-backslash ] }
            { CHAR: \s [ read-token-or-whitespace ] }
            { CHAR: \r [ read-token-or-whitespace ] }
            { CHAR: \n [ read-token-or-whitespace ] }
            { f [ f like ] }
            [ drop ] ! <lexed> ]
        } case
    ] [
        f
    ] if ; inline recursive

: string>literals ( string -- sequence )
    [ 0 ] dip [ lex ] loop>array 2nip ;

: vocab>literals ( vocab -- sequence )
    ".private" ?tail drop
    modern-source-path utf8 file-contents string>literals ;
