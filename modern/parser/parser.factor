! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs bootstrap.syntax
combinators.short-circuit combinators.smart continuations
effects fry hashtables kernel make math math.parser modern
multiline namespaces quotations sequences
sequences.extras shuffle splitting stack-checker strings vectors
vocabs.parser words ;
QUALIFIED: parser
IN: modern.parser

TUPLE: number payload ;
CONSTRUCTOR: <number> number ( payload -- number ) ;
TUPLE: base-token payload ;
TUPLE: base-parsed payload ;

TUPLE: new-word < base-token ;
TUPLE: new-class < base-token ;
TUPLE: existing-word < base-token ;
TUPLE: existing-class < base-token ;

: dip-inc ( n/f seq -- n'/f seq )
    [ dup [ 1 + ] when ] dip ; inline


: token ( n seq -- n' seq token/f )
    2dup ?nth [
        dup line-comment-literal? [
            drop dip-inc token
        ] [
            [ dip-inc ] dip
        ] if
    ] [
        dip-inc f
    ] if* ;

: parse ( n seq -- n' seq parsed/f )
    2dup ?nth [
        dup line-comment-literal? [
            drop dip-inc parse
        ] [
            [ dip-inc ] dip
        ] if
    ] [
        dip-inc f
    ] if* ;

: tokens-until ( n seq token -- n' seq out last )
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

: parse-until ( n seq token -- n' seq out last )
        pick [
        3dup '[
            [
                parse dup , [
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


<<
: define-inferred ( word def -- ) dup infer define-declared ;
>>
<<    
SYNTAX: PARSER:
    parser:scan-new-word mark-top-level-syntax \ ; parser:parse-until >quotation
    define-inferred ;
>>

<<
PARSER: `USING: ";" tokens-until ;
PARSER: `USE: token ;
PARSER: `IN: token ;
>>

! keep index of known parsing words that don't parse?
TUPLE: manifest' dictionary ;

: base-dictionary ( -- hashtable )
    H{
        ! { ":" word-definition-parser }
        { "IN:" `IN: }
        { "USING:" `USING: }
        { "USE:" `USE: }
    } clone "loader-syntax" associate ; inline

: base-manifest ( -- manifest' )
    manifest' new
        base-dictionary >>dictionary ; inline

: lookup-word-in-manifest ( string -- word/string ? )
    manifest' get dictionary>> ?at ;

GENERIC: handle-next ( n string obj -- n' string obj progress? )

M: object handle-next f ;

M: tag-literal handle-next
    dup tag>> string>number [
        <number> f
    ] [
        tag>> lookup-word-in-manifest drop dup parsing-word? [
            
        ] [
        ] if
    ] ?if ;

: literals>parsed-step ( seq -- seq ? )
    [ f 0 ] dip [
        parser-next [
            handle-next -roll [ or ] 3dip  ! progress?
        ] [
            f ! loop again?
        ] if*
    ] loop>array [ 2nip ] dip swap ;

: literals>parsed ( seq -- obj )
    base-manifest manifest' [
        [ literals>parsed-step ] loop
    ] with-variable ; inline

    

/*


TUPLE: parser seq ;

: push-seq ( parser obj -- parser )
    over seq>> push ;

! TUPLE: array-brace obj ;
! TUPLE: hashtable-brace obj ;
! TUPLE: word-definition-parser obj array ;
! TUPLE: constant-parser < parser name value ;
! TUPLE: using-parser < parser names ;
! TUPLE: use-parser < parser name ;


GENERIC: transform-literal ( n string obj -- n' string obj )
GENERIC: on-parse ( n string obj -- n' string obj )
M: object on-parse ;
GENERIC: on-literal ( n string tuple -- n' string obj )

M: brace-literal transform-literal
    [ ] [ tag>> >string ] bi
    braces get ?at [ new swap >>obj ] [ unknown-syntax ] if ;

M: paren-literal transform-literal
    payload>>
    [ >string ] map { "--" } split1 <effect> ;

! constant:2 a 3
! constant: 2: a 3
! 2:CONSTANT a 3

! self-contained lexed things:  [[ ]]  foo`bar  til-eol  lookahead-n whitespace-word
! Don't necessarily want to allow compound strings back to back...
! in fact, only one literal in a row? literal is literal"" literal[[]] literal`foo
! and any amount of code? literal[ ][ ][ ]?
! then we can have triple strings? backtick strings? ` ' matched strings?

M: word-definition-parser on-parse
    [ [ new-word stack-effect body ] 2 output>array-n ] dip roll >>array ;



M: number on-literal ;

M: string on-literal ;

M: array-brace on-literal
    obj>> payload>> [ transform-literal ] map ;

M: hashtable-brace on-literal
    obj>> payload>> [ transform-literal ] map >hashtable ;

M: word-definition-parser on-literal ;

M: using-parser on-literal
    ;

M: use-parser on-literal
    ;

ERROR: token-expected got expected ;
: check-expect ( n seq obj quot -- n seq obj )
    2dup call [ drop ] [ token-expected ] if ; inline

DEFER: transform-literal
: expect-token ( n seq quot -- n seq obj )
    [ token ] dip check-expect transform-literal ; inline

DEFER: tokens-until
: expect-tokens-until ( n seq str quot -- n seq obj )
    [ tokens-until drop ] dip check-expect ; inline

: paren-literal? ( obj -- ? ) { [ single-matched-literal? ] [ opening>> "(" = ] } 1&& ;
: brace-literal? ( obj -- ? ) { [ single-matched-literal? ] [ opening>> "{" = ] } 1&& ;
: bracket-literal? ( obj -- ? ) { [ single-matched-literal? ] [ opening>> "[" = ] } 1&& ;

: expect-name ( n seq -- n seq slice ) [ slice? ] expect-token ;
: expect-paren-literal ( n seq -- n seq slice ) [ paren-literal? ] expect-token ;
: expect-body ( n seq -- n seq array ) ";" [ array? ] expect-tokens-until ;

: drop-token ( n seq -- n' seq ) token drop ;
: new-word ( n seq -- slice n' seq ) [ slice? ] expect-token -rot ;
: existing-word ( n seq -- slice n' seq ) [ slice? ] expect-token -rot ;
: stack-effect ( n seq -- slice n' seq ) [ paren-literal? ] expect-token -rot ;
: body ( n seq -- array n' seq ) ";" [ array? ] expect-tokens-until -rot ;
*/
