! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs classes compiler.units
constructors hash-sets hashtables io kernel make math
modern.paths modern.quick-parser modern.syntax multiline
namespaces prettyprint sequences sequences.deep sets sorting
strings words.private fry combinators quotations words.symbol
math.parser words.constant sequences.extras splitting effects
classes.builtin combinators.short-circuit modern.vocabs assocs.private ;
QUALIFIED-WITH: modern.syntax modern
FROM: syntax => f inline ;
QUALIFIED: words
QUALIFIED: vocabs
IN: modern.compiler


/*
: path>parsers ( name -- seq )
    quick-parse-path
    [ [ class-of , ] deep-each ] { } make members ;

: paths>parsers ( names -- seq )
    [ path>parsers ] map concat members ;

: path>lexers ( name -- seq )
    quick-parse-path
    [ [ dup class-of array = [ , ] [ drop ] if ] deep-each ] { } make rest members ;

: paths>lexers ( names -- seq )
    [ path>lexers ] map concat [ first ] map >out members ;


: paths>top-level-forms ( paths -- seq )
    [ dup quick-parse-path [ slice? ] filter ] { } map>assoc harvest-values ;
    ! values concat members natural-sort ;

: paths>top-level-forms. ( paths -- )
    paths>top-level-forms [ first2 [ print ] [ >out members natural-sort . ] bi* ] each ;

: core-parsers ( -- seq ) core-source-files paths>parsers ;
: core-lexers ( -- seq ) core-source-files paths>lexers ;
: basis-parsers ( -- seq ) basis-source-files paths>parsers core-parsers diff ;
: basis-lexers ( -- seq ) basis-source-files paths>lexers ;
: extra-parsers ( -- seq ) extra-source-files paths>parsers core-parsers diff basis-parsers diff ;
: extra-lexers ( -- seq ) extra-source-files paths>lexers ;

: print-parsers ( seq -- )
    members natural-sort
    [ name>> "M: modern:" " precompile ;" surround print ] each ;




: current-dict ( -- qvocab )
    linear-state get dict>> ;

: current-qvocab ( -- qvocab )
    linear-state get [ dict>> ] [ in>> ] bi of ;

: make-qvocab ( name -- )
    linear-state get dict>> ?at [
        drop
    ] [
        [ H{ } clone ] dip linear-state get dict>> set-at
    ] if ;

: lookup-qvocab ( name -- qvocab )
    [ linear-state get dict>> ] dip of ;

: add-old-to-namespace ( name -- )
    vocabs:lookup-vocab words>> linear-state get
    [ assoc-union ] change-namespace drop ;

: hashtables>hashtable ( hashtables -- hashtable )
    [ H{ } clone ] dip [ over [ push-at ] with-assoc assoc-each ] each ; inline

: make-using-namespace ( linear-state -- )
    [
        using>> members [ vocabs:lookup-vocab words>> ] map hashtables>hashtable
    ] keep using-namespace<< ;
    

: add-using ( name -- )
    linear-state get using>> adjoin ;
    
: set-in ( in -- ) >string linear-state get in<< ;
ERROR: no-in-form ;
: get-in ( -- obj ) linear-state get in>> [ no-in-form ] unless* ;
: get-dict ( -- obj ) linear-state get dict>> ;
: set-private ( ? -- ) linear-state get private?<< ;
: get-private ( -- obj ) linear-state get private?>> ;
: set-compilation-unit ( ? -- ) linear-state get compilation-unit?<< ;
: get-compilation-unit ( -- obj ) linear-state get compilation-unit?>> ;
: set-last-word ( name -- ) linear-state get last-word<< ;
: get-last-word ( -- obj ) linear-state get last-word>> ;
: add-decorator ( obj -- ) linear-state get decorators>> push ;
: transfer-decorators ( -- )
    linear-state get [
        get-last-word [ decorators<< ] [ drop ] if*
        V{ } clone
    ] change-decorators drop ;

: set-word-in ( -- )
    get-in [ get-last-word [ in<< ] [ drop ] if* ] when* ;
: set-word-private ( -- )
    get-private [ get-last-word [ private?<< ] [ drop ] if* ] when* ;
: set-word-compilation-unit ( -- )
    get-compilation-unit [ get-last-word [ compilation-unit?<< ] [ drop ] if* ] when* ;

DEFER: name-of
: name-first ( object -- string ) object>> first >string ;
: name-second ( object -- string ) object>> first >string ;
: name-sequence ( object -- strings ) object>> first [ name-of ] map ;

GENERIC: name-of ( obj -- name )
M: modern:function name-of name-first ;
M: modern:constructor name-of name-first ;
M: modern:generic name-of name-first ;
M: modern:generic# name-of name-first ;
M: modern:math name-of name-first ;
M: modern:mixin name-of name-first ;
M: modern:tuple name-of name-first ;
M: modern:error name-of name-first ;
M: modern:builtin name-of name-first ;
M: modern:primitive name-of name-first ;
M: modern:union name-of name-first ;
M: modern:intersection name-of name-first ;
M: modern:predicate name-of name-first ;
M: modern:slot name-of name-first ;
M: modern:hook name-of name-first ;
M: modern:method name-of object>> first2 2array ;
M: modern:constant name-of name-first ;
M: modern:singleton name-of name-first ;
M: modern:singletons name-of name-sequence ;
M: modern:symbol name-of name-first ;
M: modern:symbols name-of name-sequence ;
M: modern:defer name-of name-first ;
M: modern:alias name-of name-first ;
M: modern:c-function name-of name-first ;
M: modern:gl-function name-of name-first ;
M: modern:macro name-of name-second ;
M: object name-of drop f ;

: transfer-state ( -- )
    set-word-compilation-unit
    set-word-private
    set-word-in transfer-decorators ;

GENERIC: meta-pass ( obj -- )
M: object meta-pass
   dup name-of [
       transfer-state
       set-last-word
   ] [ drop ] if ;

M: modern:using meta-pass object>> first [ >string add-using ] each ;
M: modern:use meta-pass object>> first >string add-using ;
M: modern:in meta-pass object>> first >string [ set-in ] [ make-qvocab ] bi ;
M: modern:private-begin meta-pass drop t set-private ;
M: modern:private-end meta-pass drop f set-private ;

M: modern:final meta-pass add-decorator ;
M: modern:foldable meta-pass add-decorator ;
M: modern:flushable meta-pass add-decorator ;
M: modern:inline meta-pass add-decorator ;
M: modern:recursive meta-pass add-decorator ;
M: modern:deprecated meta-pass add-decorator ;
M: modern:delimiter meta-pass add-decorator ;



ERROR: class-already-exists name vocaab ;
ERROR: word-already-exists name vocab ;
ERROR: symbol-already-exists name vocab ;

ERROR: no-vocab name vocab ;
: check-class ( name vocab -- name vocab )
    dup [ no-vocab ] unless
    2dup lookup-qvocab key? [
        symbol-already-exists
    ] when ;

: check-word ( name vocab -- name vocab )
    dup [ no-vocab ] unless
    2dup lookup-qvocab key? [
        symbol-already-exists
    ] when ;


! XXX: module repository
: word-hashcode ( name vocab -- hashcode )
    [ hashcode ] bi@ hash-combine >fixnum ;

: make-word ( name vocab -- word )
    2dup word-hashcode (word) ; ! XXX: add to compilation-unit

: add-class-prop ( word -- word' )
    dup t "class" words:set-word-prop ;

: record-word ( word -- )
    dup name>> current-qvocab set-at ;

! : record-class ( word -- ) dup name>> current-qvocab classes>> set-at ;

: record-namespace ( word -- )
    dup name>> current-qvocab set-at ;

ERROR: identifier-not-found name ;
: lookup-in-linear-state ( name -- word )
    current-qvocab ?at [ identifier-not-found ] unless ;

: make-class ( name vocab -- class )
    make-word add-class-prop ;

: create-word ( string vocab -- word )
    check-class check-word
    make-word [ record-word ] [ record-namespace ] [ ] tri ;

: create-class ( string vocab -- class )
    check-class check-word
    make-class [ record-namespace ] [ ] bi ;

: create-class-word ( string vocab -- class )
    check-class check-word
    make-class { [ record-word ] [ record-namespace ] [ ] } cleave ;

GENERIC: create-pass ( obj -- )

! M: object create-pass drop ;
M: modern:use create-pass drop ;
M: modern:using create-pass drop ;
M: modern:in create-pass drop ;

M: modern:singleton create-pass object>> first >string get-in create-class drop ;
M: modern:singletons create-pass object>> first get-in '[ >string _ create-class drop ] each ;
M: modern:symbol create-pass object>> first >string get-in create-word drop ;
M: modern:symbols create-pass object>> first get-in '[ >string _ create-word drop ] each ;
M: modern:constant create-pass object>> first >string get-in create-word drop ;
M: modern:function create-pass object>> first >string get-in create-word drop ;

M: run-time-long-string-literal create-pass drop ;

! M: modern:builtin create-pass     object>> first >string get-in words:lookup-word ;


: get-strings-from ( obj -- seq )
    object>> first [ >string ] map ;

GENERIC: lookup-modern-word ( object -- word )

: lookup-in-using ( string -- word/f )
    linear-state get
    [ make-using-namespace ] [ using-namespace>> ] bi at ;

: lookup-in-namespace ( string -- word/f )
    linear-state get
    namespace>> at ;

ERROR: symbol-conflicts using ;
ERROR: unhandled-symbol-conflict using namespace ;
: validate-lookup ( using namespace -- word )
    {
        { [ 2dup [ length 1 = ] [ empty? ] bi* and ] [ drop first ] }
        { [ over length 1 > ] [ drop symbol-conflicts ] }
        [ unhandled-symbol-conflict ]
    } cond ;

ERROR: unknown-word word ;
M: slice lookup-modern-word >string lookup-modern-word ;
M: string lookup-modern-word
    [ lookup-in-using ] [ lookup-in-namespace ] bi
    validate-lookup ;

DEFER: quick-compile-string
ERROR: unknown-long-string payload tag ;
: handle-long-string ( payload tag -- obj )
    {
        { "module" [ quick-compile-string ] }
        { "vocab" [ quick-compile-string ] }
        [ unknown-long-string ]
    } case ;


GENERIC: string>parsed ( object -- number/string/obj )
M: slice string>parsed >string string>parsed ;
M: string string>parsed
    dup string>number [ nip ] [ lookup-modern-word ] if* ;

M: run-time-long-string-literal string>parsed
    [ object>> >string ] [ opening>> >string ] bi handle-long-string ;



GENERIC: define-pass ( obj -- )

ERROR: top-level-form-not-implemented slice string ;
M: slice create-pass
    [ ] [ >string ] bi top-level-form-not-implemented ;

M: modern:in define-pass drop ;
M: modern:use define-pass drop ;
M: modern:using define-pass drop ;
M: modern:symbol define-pass name-first lookup-in-linear-state define-symbol ;
M: modern:symbols define-pass object>> first [ >string lookup-in-linear-state define-symbol ] each ;
M: modern:private-begin define-pass drop ; !  define-pass ;
M: modern:constant define-pass
    object>> first2
    [ >string lookup-in-linear-state ]
    [ string>parsed ] bi* define-constant ;
M: modern:function define-pass
    object>> first3 swap
    [ >string lookup-in-linear-state ]
    [ [ >string string>parsed ] map ]
    [ object>> [ >string ] map { "--" } split1 <effect> ] tri* words:define-declared ;

M: run-time-long-string-literal define-pass drop ;


: quick-compile ( seq -- vocabs )
    [
        [
            [ [ meta-pass ] [ create-pass ] [ define-pass ] tri ] each
            current-qvocab values
            [ "compiling words:" print . ]
            [ compile ] bi
            linear-state get dict>>
        ] with-linear-state
    ] with-compilation-unit ;

: quick-compile-vocab ( name -- vocabs )
    qparse-vocab quick-compile ;

: quick-compile-string ( string -- vocabs )
    qparse quick-compile ;

: linear-state>words ( linear-state -- words )
    dict>> values [ words>> values ] map-concat ;

: qcompile>words ( string -- linear-state words )
    quick-compile-string dup linear-state>words ;

: vocab>printable ( hashtable -- hashtable )
    [ keys ] assoc-map ;

: vocabs>printable ( seq -- seq' )
    [ vocab>printable ] map ;

*/

/*
! Find all primitives/builtin classes
core-bootstrap-vocabs [ prepopulate-vocab ] map concat
[ nip assoc-empty? not ] assoc-filter .


*/
