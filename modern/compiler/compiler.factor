! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators definitions effects
effects.parser fry kernel lexer modern multiline parser
sequences sequences.extras stack-checker unicode words ;
IN: modern.compiler

<<
 SYNTAX: STRING-DISPATCH:
    [
        scan-new-word scan-effect
        H{ } clone over [ in>> but-last ] [ out>> ] bi <effect>
        '[ _ ?at [ throw ] unless _ call-effect ]
        swap
    ] with-definition define-declared ;

SYNTAX: STRING-M:
    [
        scan-token scan-word parse-definition
        over changed-definition
        swap def>> first swapd set-at
    ] with-definition ;    
>>


STRING-DISPATCH: string>definers ( literal string -- strings )
STRING-M: singleton string>definers
    payload>> [ tag>> ] map dup [ "?" append ] map append ;
STRING-M: singletons string>definers
    payload>> [ tag>> ] map dup [ "?" append ] map append ;
STRING-M: symbol string>definers payload>> [ tag>> ] map ;
STRING-M: symbols string>definers payload>> [ tag>> ] map ;
STRING-M: mixin string>definers
    payload>> [ tag>> ] map dup [ "?" append ] map append ;
STRING-M: using string>definers drop f ;
STRING-M: in string>definers drop f ;
STRING-M: word string>definers payload>> first tag>> 1array ;
STRING-M: m string>definers drop f ;
STRING-M: generic string>definers payload>> first tag>> 1array ;
STRING-M: generic# string>definers payload>> first tag>> 1array ;
STRING-M: constant string>definers payload>> first tag>> 1array ;
STRING-M: tuple string>definers
    payload>> first tag>> dup "?" append 2array ;
STRING-M: error string>definers
    payload>> first tag>> dup "?" append 2array ;
STRING-M: instance string>definers drop f ;

GENERIC: decorator-only-compound? ( obj -- ? )
M: compound-literal decorator-only-compound?
    sequence>> rest [ decorator-literal? ] all? ;

M: object decorator-only-compound? drop f ;


GENERIC: literal>string-name ( class -- class string/f )

M: line-comment-literal literal>string-name f ;

M: uppercase-colon-literal literal>string-name
    dup tag>> [ "word" ] [ >lower ] if-empty ;


ERROR: unhandleable-compound-literal obj ;
M: compound-literal literal>string-name
    dup decorator-only-compound? [ unhandleable-compound-literal ] unless
    sequence>> first literal>string-name ;

M: decorator-literal literal>string-name f ;

: literals>definers ( seq -- assoc )
    [ literal>string-name [ string>definers ] [ drop f ] if* ] map-zip ;
