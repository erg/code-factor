! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit definitions effects effects.parser fry
graphs io.pathnames kernel lexer math.statistics memoize modern
parser sequences sequences.extras sets splitting strings unicode
words multiline ;
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

! generated definitions
TUPLE: define' holder name ;
TUPLE: generate-accessor' < define' ;
TUPLE: generate-predicate' < define' ;

: make-holder ( literal class -- obj )
    new
        swap >>literal ; inline

GENERIC: split-decorators ( seq -- base left right )
M: compound-literal split-decorators
    sequence>>
    [ decorator-literal? not ] partition
    [ first ] dip
    [ left-decorator-literal? ] partition ;
M: object split-decorators f f ;

! GENERIC: apply-decorator ( base decorator -- )
! : apply-decorators ( obj seq -- obj ) ;

GENERIC: base-literal ( obj -- obj )
M: compound-literal base-literal
    sequence>> [ decorator-literal? not ] find nip ;
M: object base-literal ;


GENERIC: literal>tag ( class -- string/f )
M: line-comment-literal literal>tag drop f ;
M: uppercase-colon-literal literal>tag
    tag>> [ "word" ] [ >lower ] if-empty ;
M: compound-literal literal>tag
    base-literal literal>tag ;

: literal>holder ( literal -- obj )
    [ ] [
        literal>tag [ "'" append "modern.compiler" lookup-word ]
        [ \ comment' ] if*
    ] bi
    '[ _ make-holder ] call( obj -- obj ) ;

: literals>holders ( literals -- holders )
    [ literal>holder ] map ;

GENERIC: holder>definitions' ( literal -- assoc )
M: comment' holder>definitions' drop f ;
M: using' holder>definitions' drop f ;
M: use' holder>definitions' drop f ;
M: in' holder>definitions' drop f ;
M: qualified-with' holder>definitions' drop f ;
M: qualified' holder>definitions' drop f ;
M: script' holder>definitions' drop f ;
M: m' holder>definitions' drop f ;
M: instance' holder>definitions' drop f ;

! Single words
M: word' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: generic' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: generic#' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: hook' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: math' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: constant' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: c' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: initialize' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: startup-hook' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: shutdown-hook' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: primitive' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;
M: defer' holder>definitions'
    dup literal>> base-literal payload>> first tag>> define' boa ;

! Multiple words
M: symbols' holder>definitions'
    dup literal>> base-literal payload>> [ tag>> ] map [ define' boa ] with map ;
M: symbol' holder>definitions'
    dup literal>> base-literal payload>> [ tag>> ] map [ define' boa ] with map ;
M: slot' holder>definitions'
    dup literal>> base-literal payload>> [ tag>> ] map
    [ generate-accessor' boa ] with map ;

! these also make class predicate? words

GENERIC: slot-accessor-name ( obj -- string )
M: single-matched-literal slot-accessor-name
    payload>> first tag>> ">>" append ;
M: tag-literal slot-accessor-name tag>> ">>" append ;

M: tuple' holder>definitions'
    [ dup literal>> base-literal payload>> first tag>> define' boa ]
    [ dup literal>> base-literal payload>> first tag>> "?" append generate-predicate' boa ]
    [
        dup literal>> base-literal payload>> rest
        [ slot-accessor-name generate-accessor' boa ] with map
    ] tri [ 2array ] dip append ;

M: error' holder>definitions'
    [ dup literal>> base-literal payload>> first tag>> define' boa ]
    [ dup literal>> base-literal payload>> first tag>> "?" append generate-predicate' boa ]
    [
        dup literal>> base-literal payload>> rest
        [ slot-accessor-name generate-accessor' boa ] with map
    ] tri [ 2array ] dip append ;

M: builtin' holder>definitions'
    [ dup literal>> base-literal payload>> first tag>> define' boa ]
    [ dup literal>> base-literal payload>> first tag>> "?" append generate-predicate' boa ] bi 2array ;
M: predicate' holder>definitions'
    [ dup literal>> base-literal payload>> first tag>> define' boa ]
    [ dup literal>> base-literal payload>> first tag>> "?" append generate-predicate' boa ] bi 2array ;
M: union' holder>definitions'
    [ dup literal>> base-literal payload>> first tag>> define' boa ]
    [ dup literal>> base-literal payload>> first tag>> "?" append generate-predicate' boa ] bi 2array ;

! Multiple and class predicates
M: mixin' holder>definitions'
    [ dup literal>> base-literal payload>> [ tag>> ] map [ define' boa ] with map ]
    [ dup literal>> base-literal payload>> [ tag>> "?" append ] map [ generate-predicate' boa ] with map ] bi append ;

M: singletons' holder>definitions'
    [ dup literal>> base-literal payload>> [ tag>> ] map [ define' boa ] with map ]
    [ dup literal>> base-literal payload>> [ tag>> "?" append ] map [ generate-predicate' boa ] with map ] bi append ;

M: singleton' holder>definitions'
    [ dup literal>> base-literal payload>> [ tag>> ] map [ define' boa ] with map ]
    [ dup literal>> base-literal payload>> [ tag>> "?" append ] map [ generate-predicate' boa ] with map ] bi append ;

: holder>definitions ( obj -- seq )
    holder>definitions' dup sequence? [ 1array ] unless ;

: holders>definitions ( holders -- seq )
    [ holder>definitions ] map concat ;


: holders>using ( holders -- using )
    [ { [ using'? ] [ use'? ] } 1|| ] filter
    [ literal>> payload>> [ tag>> ] map ] map concat ;

: holders>in ( holders -- using )
    [ in'? ] filter
    [ literal>> payload>> [ tag>> ] map ] map concat ;


GENERIC: lookup-literal ( namespace literal -- obj )


GENERIC: definition>quotation ( namespace definition -- quot )
M: define' definition>quotation
    2drop [ ]
    ;

: manifest>scoped-words ( manifest -- seq )
    [ name>> ] [ definition-assoc>> keys ] bi
    [ ":" glue ] with map ;

: manifest>own-namespace ( manifest -- namespace )
    [ definition-assoc>> keys ] [ manifest>scoped-words [ 1array ] map ] bi
    zip ;

: manifest>using ( manifest -- seq )
    holders>> holders>using ;

DEFER: load-modern
: manifest>combined-namespace ( manifest -- namespaces )
    [ manifest>using [ load-modern manifest>own-namespace ] map sift members H{ } clone [ assoc-union ] reduce ]
    [ manifest>own-namespace ] bi assoc-union ;

: manifest>quotations ( manifest -- quots )
    [ manifest>combined-namespace ] [ definitions>> ] bi
    [ definition>quotation ] with { } map-as ;

GENERIC: add-predicates ( obj -- seq )
M: string add-predicates dup "?" append 2array ;
M: sequence add-predicates [ add-predicates ] map concat ;

TUPLE: manifest2 name literals holders definitions definition-assoc namespaces ;

: <manifest2> ( name literals holders definitions  -- manifest2 )
    manifest2 new
        swap >>definitions
        dup definitions>> [ [ name>> ] keep ] { } map>assoc >>definition-assoc
        swap >>holders
        swap >>literals
        swap ".private" ?tail drop >>name ; inline

: manifest>definitions ( manifest -- namespace )
    [ name>> ]
    [ definitions>> [ name>> ] map ] bi
    [ ":" glue ] with map ;

: manifests>namespace ( manifests -- namespace )
    [
        [ name>> ] [ identifiers>> keys ] bi
        [ "." glue ] with map-zip
    ] collect-by ;


MEMO: load-modern ( name -- literals )
    dup vocab>core2-path path>literals
    dup literals>holders
    dup holders>definitions <manifest2> ;

: load-modern-closure ( vocab -- manifests )
    \ load-modern reset-memoized
    load-modern [ holders>using [ load-modern ] map ] closure ;


/*
"sequences" load-modern
[ holder>definitions ] map sift
[ dup array? [ [ name>> ] map ] [ name>> ] if ] map flatten
describe


clear
"sequences" load-modern
definitions>> [ define'? ] filter
[ holder>> word'? ] filter
first
*/
