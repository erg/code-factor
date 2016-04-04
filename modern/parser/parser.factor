! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators.smart continuations
effects fry hashtables kernel make math math.parser modern
namespaces sequences sequences.extras shuffle splitting strings ;
IN: modern.parser

TUPLE: new-word name ;
TUPLE: new-class name ;
TUPLE: existing-word name ;
TUPLE: existing-class name ;

: dip-inc ( n/f seq -- n'/f seq )
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
: new-word ( n seq -- slice n' seq ) [ slice? ] expect-token -rot ;
: existing-word ( n seq -- slice n' seq ) [ slice? ] expect-token -rot ;
: stack-effect ( n seq -- slice n' seq ) [ paren-literal? ] expect-token -rot ;
: body ( n seq -- array n' seq ) ";" [ array? ] expect-tokens-until -rot ;

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


! constant:2 a 3
! constant: 2: a 3
! 2:CONSTANT a 3

! self-contained lexed things:  [[ ]]  foo`bar  til-eol  lookahead-n whitespace-word
