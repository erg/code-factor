! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators definitions effects
effects.parser fry io.pathnames kernel lexer modern multiline
parser sequences sequences.extras splitting stack-checker
unicode words sets ;
IN: modern.compiler

: vocab>core2-path ( vocab -- path )
    ".private" ?tail drop
    "." split "/" join
    [ "resource:core2/" prepend-path ] [ file-name ".factor" append append-path ] bi ; 

: filter-using ( using -- using' )
    { "accessors" "threads.private" "threads" } diff ;

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

STRING-DISPATCH: identifier-pass ( literal string -- assoc )
STRING-M: using identifier-pass drop f ;
STRING-M: use identifier-pass drop f ;
STRING-M: in identifier-pass drop f ;
STRING-M: qualified-with identifier-pass drop f ;
STRING-M: qualified identifier-pass drop f ;


STRING-M: script identifier-pass drop f ;

STRING-M: m identifier-pass drop f ;
STRING-M: instance identifier-pass drop f ;

STRING-M: word identifier-pass payload>> first tag>> 1array ;
STRING-M: generic identifier-pass payload>> first tag>> 1array ;
STRING-M: generic# identifier-pass payload>> first tag>> 1array ;
STRING-M: primitive identifier-pass payload>> first tag>> 1array ;
STRING-M: math identifier-pass payload>> first tag>> 1array ;
STRING-M: constant identifier-pass payload>> first tag>> 1array ;
STRING-M: c identifier-pass payload>> first tag>> 1array ;
STRING-M: hook identifier-pass payload>> first tag>> 1array ;
STRING-M: defer identifier-pass payload>> first tag>> 1array ;
STRING-M: initialize identifier-pass payload>> first tag>> 1array ;
STRING-M: startup-hook identifier-pass payload>> first tag>> 1array ;
STRING-M: shutdown-hook identifier-pass payload>> first tag>> 1array ;
    
STRING-M: symbols identifier-pass "symbol" identifier-pass ;
STRING-M: symbol identifier-pass
    payload>> [ tag>> ] map ;
    
STRING-M: slot identifier-pass
    payload>> [ tag>> ] map ;

STRING-M: mixin identifier-pass
    payload>> [ tag>> ] map
    dup [ "?" append ] map append ;

STRING-M: singletons identifier-pass "singleton" identifier-pass ;    
STRING-M: singleton identifier-pass
    payload>> [ tag>> ] map dup [ "?" append ] map append ;

STRING-M: tuple identifier-pass
    payload>> first tag>> dup "?" append 2array ;
STRING-M: error identifier-pass
    payload>> first tag>> dup "?" append 2array ;
STRING-M: builtin identifier-pass
    payload>> first tag>> dup "?" append 2array ;
STRING-M: predicate identifier-pass
    payload>> first tag>> dup "?" append 2array ;
STRING-M: union identifier-pass
    payload>> first tag>> dup "?" append 2array ;



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

: literals>identifiers ( seq -- assoc )
    [ literal>string-name [ identifier-pass ] [ drop f ] if* ] map-zip ;

TUPLE: manifest2 identifiers ;

: <manifest2> ( -- manifest2 )
    manifest2 new
        H{ } clone >>identifiers ; inline

/*
populate with existing primitives, builtins, bootstrap-only words
identifiers pass
resolve pass
stack check pass
compile pass ! cross-compile?
replace words in global dict, update existing words

! tool - word rename
! only compile words that have actually changed
! recompile lexing-words
*/

: literals>manifest ( seq -- manifest )
    [ <manifest2> ] dip {
        [ literals>identifiers swap identifiers>> '[ [ _ push-at ] with each ] assoc-each ]
        [ drop ]
    } 2cleave ;


/*
core-vocabs [ vocab-words ] map-zip { length-test<=> } sort-values-by reverse

"/Users/erg/factor/core2/vocabs/parser/parser.factor"
utf8 file-contents
string>literals
literals>definers values concat natural-sort

"vocabs.parser" [ ".private" append ] [ ] bi [ vocab-words ] bi@ append [ name>> ] map
swap diff

clear
"arrays" vocab>core2-path path>literals
[ { [ uppercase-colon-literal? ] [ tag>> "USING" = ] } 1&& ] filter
[ payload>> ] map concat [ tag>> ] map
filter-using
[ vocab>core2-path path>literals literals>manifest ] map-zip


clear
core-bootstrap-vocabs
[ vocab>core2-path path>literals
[ { [ uppercase-colon-literal? ] [ tag>> "USING" = ] } 1&& ] filter
[ payload>> ] map concat [ tag>> ] map
filter-using
[ dup . flush vocab>core2-path path>literals literals>manifest ] map-zip
] map-zip
*/