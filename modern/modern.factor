! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit combinators.smart constructors
continuations effects fry hashtables io.encodings.utf8 io.files
kernel lexer locals make math math.parser modern.paths multiline
namespaces sequences sequences.extras shuffle splitting strings
unicode ;
IN: modern

TUPLE: lexed underlying ;

: <lexed> ( slice -- lexed )
    lexed new
        swap >>underlying ; inline
TUPLE: colon-lexed < lexed ;

TUPLE: literal < lexed tag payload ;

TUPLE: string-literal < literal ;
TUPLE: backtick-literal < literal ;
TUPLE: backslash-literal < literal ;

TUPLE: bracket-literal < literal ;
TUPLE: brace-literal < literal ;
TUPLE: paren-literal < literal ;

TUPLE: double-literal < literal ;
TUPLE: double-bracket-literal < double-literal ;
TUPLE: double-brace-literal < double-literal ;
TUPLE: double-paren-literal < double-literal ;

TUPLE: comment text underlying ;
TUPLE: eol-comment < comment ;
TUPLE: double-comment < comment ;
TUPLE: double-bracket-comment < double-comment ;
TUPLE: double-brace-comment < double-comment ;
TUPLE: double-paren-comment < double-comment ;

:: make-literal ( payload end tag literal-class -- literal )
    literal-class new
        tag >>tag
        payload >>payload
        tag from>> end [ to>> ] [ seq>> ] bi <slice> >>underlying ; inline

: make-comment ( text comment-class -- comment )
    new
        swap >>text ; inline
        ! swap >>underlying ; inline

: ?<slice> ( m n/f string -- slice )
    over [ nip [ length ] [ ] bi ] unless <slice> ; inline

! Allow eof
: take-next-char ( n/f string -- n/f string ch/f )
    over [
        2dup ?nth [ [ 1 + ] 2dip ] [ f ] if*
    ] [
        [ 2drop f ] [ nip ] 2bi f
    ] if ;

: skip-blank ( n string -- n' string )
    [ [ blank? not ] find-from drop ] keep ; inline

: skip-til-eol ( n string -- n' string )
    [ [ "\r\n" member? ] find-from drop ] keep ; inline

:: take-til-eol ( n string -- n' string slice/f ch/f )
    n string '[ "\r\n" member? ] find-from :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

: append-slice ( begin end -- slice )
    [ drop from>> ]
    [ nip [ to>> ] [ seq>> ] bi <slice> ] 2bi ; inline

: prepend-slice ( end begin -- slice )
    swap append-slice ; inline

! Don't include the whitespace in the slice
:: take-until-whitespace ( n string -- n' string slice/f ch/f )
    n string '[ "\s\r\n" member? ] find-from :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

:: take-including-separator ( n string tokens -- n' string slice/f ch/f )
    n string '[ tokens member? ] find-from [ dup [ 1 + ] when ] dip  :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

:: take-excluding-separator ( n string tokens -- n' string slice/f ch/f )
    n string '[ tokens member? ] find-from :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

:: take-until-either ( n string tokens -- n' string slice/f ch )
    n string '[ tokens member? ] find-from
    dup "\s\r\n" member? [
        :> ( n' ch )
        n' string
        n n' string ?<slice>
        ch
    ] [
        [ dup [ 1 + ] when ] dip :> ( n' ch )
        n' string
        n n' string ?<slice>
        ch
    ] if ; inline

: complete-lex ( n string token --  n' string token' )
    [ take-until-whitespace drop ] dip prepend-slice ;

ERROR: whitespace-expected-after n string ch ;
: skip-space-after ( n string -- n' string )
    take-next-char [
        dup blank?
        [ drop ]
        [ whitespace-expected-after ] if
    ] when* ;

ERROR: subseq-expected-but-got-eof n string expected ;
:: multiline-string-until' ( n string multi --  n' string inside end )
    multi string n start* :> n'
    n' [ n string multi subseq-expected-but-got-eof ] unless
    n' multi length +  string
    n n' string ?<slice>
    n' dup multi length + string ?<slice> ;

: multiline-string-until ( n string multi --  n' string inside )
    multiline-string-until' drop ; inline

: modify-to ( slice n -- slice' )
    [ [ from>> ] [ to>> ] [ seq>> ] tri ] dip
    swap [ + ] dip <slice> ;

! (( )) [[ ]] {{ }}
ERROR: long-opening-mismatch tag open n string ch ;

: matching-char ( ch -- ch' )
    H{
        { CHAR: ( CHAR: ) }
        { CHAR: [ CHAR: ] }
        { CHAR: { CHAR: } }
    } ?at drop ;

: setup-long-macro ( ch -- openstr2 openstr1 closestr1 closestr2 )
    dup matching-char {
        [ drop 2 swap <string> ]
        [ drop 1string ]
        [ nip 1string ]
        [ nip 2 swap <string> ]
    } 2cleave ;

MACRO:: read-long ( open-ch target-literal -- quot )
    open-ch setup-long-macro :> ( openstr2 openstr1 closestr1 closestr2 )
    [| n string tag ch |
        ch {
            { CHAR: = [
                n string openstr1 take-including-separator :> (  n' string' tag2 ch )
                ch open-ch = [ tag openstr2 n string ch long-opening-mismatch ]  unless
                tag2 length 1 - CHAR: = <string> closestr1 closestr1 surround :> needle

                n' string' needle multiline-string-until' :> ( n'' string'' inside end )
                n'' string
                inside end tag -1 modify-to target-literal make-literal
            ] }
            { open-ch [
                n 1 + string closestr2 multiline-string-until' :> ( n' string' inside end )
                n' string
                inside  end
                tag -1 modify-to
                target-literal make-literal
            ] }
            [ [ tag openstr2 n string ] dip long-opening-mismatch ]
        } case
     ] ;

: read-long-paren ( n string tag ch -- n string seq )
    CHAR: ( \ double-paren-literal read-long ;

: read-long-bracket ( n string tag ch -- n string seq )
    CHAR: [ \ double-bracket-literal read-long ;

: read-long-brace ( n string tag ch -- n string seq )
    CHAR: { \ double-brace-literal read-long ;

DEFER: lex

ERROR: lex-expected-but-got-eof n string expected ;    
: lex-until' ( n string token -- n string out end-out )
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


: lex-until ( n string token -- n/f string obj )
    lex-until' drop ; inline

ERROR: unexpected-eof n string expected ;

: nth-check-eof ( n string ch -- nth )
    2over ?nth [ 2nip nip ] [ unexpected-eof ] if* ;

: read-bracket ( n string last -- n' string seq )
    2over "]" nth-check-eof {
        { [ dup "=[" member? ] [ read-long-bracket ] } ! double bracket, read [==[foo]==]
        [ drop [ "]" lex-until' ] dip -1 modify-to bracket-literal make-literal ] ! bracket[ word
    } cond ;

: read-brace ( n string seq -- n' string seq )
    2over "}" nth-check-eof {
        { [ dup "={" member? ] [ read-long-brace ] } ! double brace, read {=={foo}==}
        [ drop [ "}" lex-until' ] dip -1 modify-to brace-literal make-literal ] ! brace{ word
    } cond ;

: read-paren ( n string seq -- n' string seq )
    2over ")" nth-check-eof {
        { [ dup "=(" member? ] [ read-long-paren ] } ! double paren, read (==(foo)==)
        [ drop [ ")" lex-until' ] dip -1 modify-to paren-literal make-literal ] ! paren( word
    } cond ;

: read-backtick ( n string opening -- n' string obj )
    [ take-until-whitespace drop ] dip
    backtick-literal new
        swap but-last-slice >>tag
        swap >>payload
        dup [ tag>> from>> ]
            [ payload>> [ to>> ] [ seq>> ] bi ] bi <slice> >>underlying ;

ERROR: string-expected-got-eof n string ;
: read-string' ( n string -- n' string )
    over [
        { CHAR: \ CHAR: " } take-including-separator {
            { f [ drop ] }
            { CHAR: " [ drop ] }
            { CHAR: \ [ take-next-char 2drop read-string' ] }
        } case
    ] [
        string-expected-got-eof
    ] if ;

:: read-string ( n string name -- n' string seq )
    n string read-string' :> ( n' seq' )
    n' string
    n n' 1 - string <slice>
    n' [ 1 - n' ] [ string length [ 2 - ] [ 1 - ] bi ] if* string <slice>
    name [ from>> ] [ to>> 1 - ] [ seq>> ] tri <slice>
    string-literal make-literal ;

: take-comment ( n string slice -- n' string comment )
    2over ?nth CHAR: [ = [
        1 modify-to
        [ 1 + ] 2dip 2over ?nth read-long-bracket
    ] [
        drop take-til-eol drop eol-comment make-comment
    ] if ;

! Words like append! and suffix! are allowed for now.
: read-exclamation ( n string slice -- n' string obj )
    dup { [ "!" sequence= ] [ "#!" sequence= ] } 1||
    [ take-comment ] [ complete-lex ] if ;

ERROR: backslash-unexpected-eof slice n string ;
: read-backslash' ( n string ch -- n' string obj )
    [ skip-space-after skip-blank ] dip ! skip at least one space, then skip all blanks after that
    [ take-until-whitespace drop ] dip over length 0 > [
        backslash-literal new
            swap >>payload
            swap >>tag
     ] [
        backslash-unexpected-eof
     ] if ;

: read-backslash ( n string ch -- n' string obj )
    dup "\\" head? [
        read-backslash'
    ] [
        complete-lex
    ] if ;

: read-colon ( ch n string -- n' string obj )
    complete-lex
    colon-lexed new
        swap >>underlying ;

: read-closing ( n string tok -- n string tok )
    dup length 1 = [
    ] [
        -1 modify-to [ 1 - ] 2dip
    ] if ;

! If we got more than 1 char, we got a real token, return it.
! 1 char or fewer, we got a whitespace, try token again.
: read-token-whitespace ( n string tok -- n string tok )
    dup length 0 = [
        drop [ 1 + ] dip lex
    ] when ;

: lex ( n/f string -- n'/f string token )
    over [
        ! XXX: order matteres, specifically comments vs #[[ ]]
        ! seq n string ch
        "!`()[]{}\"\s\r\n\\:" take-until-either {
            { f [ f like ] }
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
            { CHAR: : [ read-colon ] }
            { CHAR: \s [ read-token-whitespace ] }
            { CHAR: \r [ read-token-whitespace ] }
            { CHAR: \n [ read-token-whitespace ] }
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


TUPLE: new-word name ;
TUPLE: new-class name ;
TUPLE: existing-word name ;
TUPLE: existing-class name ;

: dip-inc ( n seq -- n' seq )
    [ dup [ 1 + ] when ] dip ; inline

: token ( n seq -- n' seq token/f )
    2dup ?nth [
        dup comment? [
            drop dip-inc token
        ] [
            [ dip-inc ] dip
        ] if
    ] [
        dip-inc f
    ] if* ;

ERROR: token-expected got expected ;
: check-expect ( n seq obj quot -- n seq obj )
    2dup call [ drop ] [ token-expected ] if ; inline

DEFER: transform-literal
: expect-token ( n seq quot -- n seq obj )
    [ token ] dip check-expect transform-literal ; inline

DEFER: tokens-until
: expect-tokens-until ( n seq str quot -- n seq obj )
    [ tokens-until drop ] dip check-expect ; inline

: expect-name ( n seq -- n seq slice ) [ slice? ] expect-token ;
: expect-paren-literal ( n seq -- n seq slice ) [ paren-literal? ] expect-token ;
: expect-body ( n seq -- n seq array ) ";" [ array? ] expect-tokens-until ;

: drop-token ( n seq -- n' seq ) token drop ;
: new-word ( n seq -- slice n seq ) [ slice? ] expect-token -rot ;
: existing-word ( n seq -- slice n seq ) [ slice? ] expect-token -rot ;
: stack-effect ( n seq -- slice n seq ) [ paren-literal? ] expect-token -rot ;
: body ( n seq -- array n seq ) ";" [ array? ] expect-tokens-until -rot ;

ERROR: expected-more-tokens n string expected ;    
: tokens-until ( n seq token -- n seq out last )
    pick [
        3dup '[
            [
                token dup , [
                    dup slice? [
                        _ sequence= not
                    ] [
                        drop t  ! loop again?
                    ] if
                ] [
                    _ _ _ expected-more-tokens
                ] if*
            ] loop
        ] { } make unclip-last
    ] [
        expected-more-tokens
    ] if ;


! core-bootstrap-vocabs [ dup [ vocab>literals ] [ drop ] recover ] { } map>assoc values [ string? ] filter .
: parse-vocabs ( vocabs -- assoc )
    [ dup [ vocab>literals ] [ drop ] recover ] { } map>assoc ;

: vocabs-with-parse-errors ( vocabs -- seq )
    parse-vocabs values [ string? ] filter ;


TUPLE: array-brace obj ;
TUPLE: hashtable-brace obj ;
TUPLE: word-definition-colon obj array ;

SYMBOL: brackets
brackets [
    H{
    } clone
] initialize

SYMBOL: braces
braces [
    H{
        { "" array-brace }
        { "H" hashtable-brace }
    } clone
] initialize

SYMBOL: parens
parens [
    H{
    } clone
] initialize

SYMBOL: colons
colons [
    H{
        { "" word-definition-colon }
    } clone
] initialize

SYMBOL: parsers
parsers [
    H{
        ! { ":" word-definition-colon }
    } clone
] initialize

ERROR: unknown-syntax obj tag ;

GENERIC: transform-literal ( n string obj -- n' string obj )
GENERIC: on-parse ( n string obj -- n' string obj )
M: object on-parse ;
GENERIC: on-literal ( n string tuple -- n' string obj )

: literals>parsed ( seq -- obj )
    [ 0 ] dip [
        token [
            transform-literal
            on-parse
            on-literal
        ] [
            f
        ] if*
    ] loop>array 2nip ;

M: slice transform-literal
    dup string>number [
    ] [
        >string dup parsers get ?at [ new swap >>obj ] [ nip ] if
    ] ?if ;

M: brace-literal transform-literal
    [ ] [ tag>> >string ] bi
    braces get ?at [ new swap >>obj ] [ unknown-syntax ] if ;

M: colon-lexed transform-literal
    [ ] [ underlying>> ":" ?tail drop >string ] bi
    colons get ?at [ new swap >>obj ] [ unknown-syntax ] if ;

M: paren-literal transform-literal
    payload>>
    [ >string ] map { "--" } split1 <effect> ;


M: word-definition-colon on-parse
    [ [ new-word stack-effect body ] 2 output>array-n ] dip roll >>array ;




M: number on-literal ;

M: array-brace on-literal
    obj>> payload>> [ transform-literal ] map ;

M: hashtable-brace on-literal
    obj>> payload>> [ transform-literal ] map >hashtable ;

M: word-definition-colon on-literal ;
