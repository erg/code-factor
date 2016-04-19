! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit definitions effects effects.parser fry
graphs io.pathnames kernel lexer math.statistics memoize modern
multiline parser sequences sequences.extras sets splitting
stack-checker unicode words ;
IN: modern.compiler

: vocab>core2-path ( vocab -- path )
    ".private" ?tail drop
    "." split "/" join
    [ "resource:core2/" prepend-path ]
    [ file-name ".factor" append append-path ] bi ;

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


TUPLE: holder literal ;
TUPLE: comment' < holder ;
TUPLE: using' < holder ;
TUPLE: use' < holder ;
TUPLE: in' < holder ;
TUPLE: qualified-with' < holder ;
TUPLE: qualified' < holder ;
TUPLE: script' < holder ;
TUPLE: m' < holder ;
TUPLE: instance' < holder ;
TUPLE: word' < holder ;
TUPLE: generic' < holder ;
TUPLE: generic#' < holder ;
TUPLE: hook' < holder ;
TUPLE: math' < holder ;
TUPLE: constant' < holder ;
TUPLE: c' < holder ;
TUPLE: initialize' < holder ;
TUPLE: startup-hook' < holder ;
TUPLE: shutdown-hook' < holder ;
TUPLE: primitive' < holder ;
TUPLE: defer' < holder ;
TUPLE: symbols' < holder ;
TUPLE: symbol' < holder ;
TUPLE: slot' < holder ;
TUPLE: mixin' < holder ;
TUPLE: singletons' < holder ;
TUPLE: singleton' < holder ;
TUPLE: tuple' < holder ;
TUPLE: error' < holder ;
TUPLE: builtin' < holder ;
TUPLE: predicate' < holder ;
TUPLE: union' < holder ;

: make-holder ( literal class -- obj )
    new
        swap >>literal ; inline


GENERIC: literal>tag ( class -- string/f )
M: line-comment-literal literal>tag drop f ;
M: uppercase-colon-literal literal>tag
    tag>> [ "word" ] [ >lower ] if-empty ;
M: compound-literal literal>tag
    sequence>> [ decorator-literal? not ] find nip literal>tag ;

: literal>holder ( literal -- obj )
    [ ] [
        literal>tag [ "'" append "modern.compiler" lookup-word ]
        [ \ comment' ] if*
    ] bi
    '[ _ make-holder ] call( obj -- obj ) ;

: literals>holders ( literals -- holders )
    [ literal>holder ] map ;

GENERIC: holder>identifier ( literal -- assoc )
M: using' holder>identifier drop f ;
M: use' holder>identifier drop f ;
M: in' holder>identifier drop f ;
M: qualified-with' holder>identifier drop f ;
M: qualified' holder>identifier drop f ;
M: script' holder>identifier drop f ;
M: m' holder>identifier drop f ;
M: instance' holder>identifier drop f ;

! Single words
M: word' holder>identifier payload>> first tag>> ;
M: generic' holder>identifier payload>> first tag>> ;
M: generic#' holder>identifier payload>> first tag>> ;
M: hook' holder>identifier payload>> first tag>> ;
M: math' holder>identifier payload>> first tag>> ;
M: constant' holder>identifier payload>> first tag>> ;
M: c' holder>identifier payload>> first tag>> ;
M: initialize' holder>identifier payload>> first tag>> ;
M: startup-hook' holder>identifier payload>> first tag>> ;
M: shutdown-hook' holder>identifier payload>> first tag>> ;
M: primitive' holder>identifier payload>> first tag>> ;
M: defer' holder>identifier payload>> first tag>> ;

! Multiple words
M: symbols' holder>identifier  payload>> [ tag>> ] map ;
M: symbol' holder>identifier payload>> [ tag>> ] map ;
M: slot' holder>identifier payload>> [ tag>> ] map ;

! these also make class predicate? words
M: tuple' holder>identifier payload>> first tag>> ;
M: error' holder>identifier payload>> first tag>> ;
M: builtin' holder>identifier payload>> first tag>> ;
M: predicate' holder>identifier payload>> first tag>> ;
M: union' holder>identifier payload>> first tag>> ;

! Multiple and class predicates
M: mixin' holder>identifier payload>> [ tag>> ] map ;
M: singletons' holder>identifier payload>> [ tag>> ] map ;
M: singleton' holder>identifier payload>> [ tag>> ] map ;


TUPLE: manifest2 name using in literals identifiers definitions ;

: <manifest2> ( literals -- manifest2 )
    manifest2 new
        swap >>literals
        V{ } clone >>using
        V{ } clone >>in
        H{ } clone >>identifiers
        H{ } clone >>definitions ; inline


: literals>manifest ( seq -- manifest )
    [ <manifest2> ] keep {
        ! [ literals>identifiers over identifiers>> '[ [ _ push-at ] with each ] assoc-each ]
    } cleave ;

: manifests>namespace ( manifests -- namespace )
    [
        [ name>> ] [ identifiers>> keys ] bi
        [ "." glue ] with map-zip
    ] collect-by ;

: holders>using ( holders -- using )
    [ { [ using'? ] [ use'? ] } 1|| ] filter
    [ literal>> payload>> [ tag>> ] map ] map concat ;

: holders>in ( holders -- using )
    [ in'? ] filter
    [ literal>> payload>> [ tag>> ] map ] map concat ;

MEMO: load-modern ( vocab -- literals )
    vocab>core2-path path>literals literals>holders ;

: load-modern-closure ( vocab -- manifests )
    \ load-modern reset-memoized
    load-modern [ holders>using [ load-modern ] map ] closure ;
