! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs combinators constructors kernel make
modern.parser namespaces nested-comments sequences ;
IN: modern.parser.factor

ERROR: string-expected got separator ;
! TUPLE: mstring < parsed class string ;
! CONSTRUCTOR: <mstring> mstring ( class string -- mstring ) ;

TUPLE: text < parsed string from to ;
CONSTRUCTOR: <text> text ( string from to -- text ) ;

! TUPLE: comment < parsed text ;
! CONSTRUCTOR: <comment> comment ( text -- comment ) ;

TUPLE: nested-comment < parsed comment ;
CONSTRUCTOR: <nested-comment> nested-comment ( comment -- nested-comment ) ;
: parse-nested-comment ( -- nested-comment )
    "*)" parse-comment-until <nested-comment> ;

TUPLE: signature < parsed in out ;
CONSTRUCTOR: <signature> signature ( in out -- signature ) ;

TUPLE: typed-argument < parsed name signature ;
CONSTRUCTOR: <typed-argument> typed-argument ( name signature -- typed ) ;

DEFER: parse-signature(--)
DEFER: parse-nested-signature(--)
DEFER: parse-signature-in
DEFER: parse-signature-in'

: parse-signature-in'' ( -- )
    raw dup ":" tail? [
        parse-nested-signature(--) <typed-argument> ,
        parse-signature-in''
    ] [
        dup "--" = [
            drop
        ] [
            , parse-signature-in''
        ] if
    ] if ;

: parse-signature-in' ( -- out )
    [ parse-signature-in'' ] { } make ;

: parse-signature-in ( -- in )
    "(" expect parse-signature-in' ;

: parse-signature-out' ( -- )
    raw dup ":" tail? [
        parse-nested-signature(--) <typed-argument> ,
        parse-signature-out'
    ] [
        dup ")" = [
            drop
        ] [
            , parse-signature-out'
        ] if
    ] if ;

: parse-signature-out ( -- out )
    [ parse-signature-out' ] { } make ;

: parse-nested-signature(--) ( -- signature )
    raw dup "(" = [
        drop
        parse-signature-in' parse-signature-out <signature>
    ] when ;

: parse-signature(--) ( -- signature )
    parse-signature-in parse-signature-out <signature> ;

: parse-signature--) ( -- signature )
    parse-signature-in' parse-signature-out <signature> ;

TUPLE: syntax < parsed name body ;
CONSTRUCTOR: <syntax> syntax ( name body -- syntax ) ;
: parse-syntax ( -- syntax )
    raw body <syntax> ;

TUPLE: function < parsed name signature body ;
CONSTRUCTOR: <function> function ( name signature body -- function ) ;
: parse-function ( -- function )
    token
    parse-signature(--)
    body <function> ;

TUPLE: locals-function < parsed name signature body ;
CONSTRUCTOR: <locals-function> locals-function ( name signature body -- function ) ;
: parse-locals-function ( -- function )
    token
    parse-signature(--)
    body <locals-function> ;

TUPLE: typed < parsed name signature body ;
CONSTRUCTOR: <typed> typed ( name signature body -- typed ) ;
: parse-typed ( -- function )
    token
    parse-signature(--)
    body <typed> ;

TUPLE: locals-typed < parsed name signature body ;
CONSTRUCTOR: <locals-typed> locals-typed ( name signature body -- typed ) ;
: parse-locals-typed ( -- function )
    token
    parse-signature(--)
    body <locals-typed> ;


TUPLE: memo < parsed name signature body ;
CONSTRUCTOR: <memo> memo ( name signature body -- memo ) ;
: parse-memo ( -- function )
    token
    parse-signature(--)
    body <memo> ;

TUPLE: locals-memo < parsed name signature body ;
CONSTRUCTOR: <locals-memo> locals-memo ( name signature body -- memo ) ;
: parse-locals-memo ( -- function )
    token
    parse-signature(--)
    body <locals-memo> ;


TUPLE: predicate < parsed name superclass body ;
CONSTRUCTOR: <predicate> predicate ( name superclass body -- predicate ) ;
: parse-predicate ( -- predicate )
    token
    "<" expect
    token
    body <predicate> ;

TUPLE: slot < parsed name ;
CONSTRUCTOR: <slot> slot ( name -- slot ) ;
: parse-slot ( -- slot )
    token <slot> ;

TUPLE: specialized-array < parsed class ;
CONSTRUCTOR: <specialized-array> specialized-array ( class -- speicialized-array ) ;
: parse-specialized-array ( -- slot )
    token <specialized-array> ;

TUPLE: specialized-arrays < parsed classes ;
CONSTRUCTOR: <specialized-arrays> specialized-arrays ( classes -- speicialized-arrays ) ;
: parse-specialized-arrays ( -- slot )
    ";" strings-until <specialized-arrays> ;

TUPLE: postpone < parsed  name ;
CONSTRUCTOR: <postpone> postpone ( name -- postpone ) ;
: parse-postpone ( -- postpone )
    raw <postpone> ;

TUPLE: mixin < parsed  name ;
CONSTRUCTOR: <mixin> mixin ( name -- mixin ) ;
: parse-mixin ( -- mixin )
    token <mixin> ;

TUPLE: singleton < parsed name ;
CONSTRUCTOR: <singleton> singleton ( name -- singleton ) ;
: parse-singleton ( -- singleton )
    token <singleton> ;

TUPLE: singletons < parsed names ;
CONSTRUCTOR: <singletons> singletons ( names -- singletons ) ;
: parse-singletons ( -- singletons )
    body <singletons> ;

TUPLE: instance < parsed class mixin ;
CONSTRUCTOR: <instance> instance ( class mixin -- instance ) ;
: parse-instance ( -- instance )
    token token <instance> ;

TUPLE: use < parsed strings ;
CONSTRUCTOR: <use> use ( strings -- use ) ;
: parse-use ( -- use ) token <use> ;

TUPLE: using < parsed strings ;
CONSTRUCTOR: <using> using ( strings -- use ) ;
: parse-using ( -- using ) ";" strings-until <using> ;

TUPLE: author < parsed name ;
CONSTRUCTOR: <author> author ( name -- author ) ;
: parse-author ( -- author )
    string-until-eol [ " " member? ] trim <author> ;

TUPLE: block < parsed body ;
CONSTRUCTOR: <block> block ( body -- block ) ;
: parse-block ( -- block )
    "]" parse-until <block> ;

TUPLE: parsetime-block < parsed body ;
CONSTRUCTOR: <parsetime-block> parsetime-block ( body -- parsetime-block ) ;
: parse-parsetime-block ( -- block )
    "]" parse-until <parsetime-block> ;

TUPLE: locals-block < parsed body ;
CONSTRUCTOR: <locals-block> locals-block ( body -- block ) ;
: parse-locals-block ( -- block )
    "]" parse-until <locals-block> ;

TUPLE: single-bind < parsed target ;
TUPLE: multi-bind < parsed targets ;
CONSTRUCTOR: <single-bind> single-bind ( target -- bind ) ;
CONSTRUCTOR: <multi-bind> multi-bind ( targets -- bind ) ;
: parse-bind ( -- bind )
    raw dup "(" = [
        drop ")" strings-until <multi-bind>
    ] [
        <single-bind>
    ] if ;

TUPLE: fry < parsed body ;
CONSTRUCTOR: <fry> fry ( body -- block ) ;
: parse-fry ( -- block )
    "]" parse-until <fry> ;

TUPLE: marray < parsed elements ;
CONSTRUCTOR: <marray> marray ( elements -- block ) ;
: parse-marray ( -- block )
    "}" parse-until <marray> ;

TUPLE: mvector < parsed elements ;
CONSTRUCTOR: <mvector> mvector ( elements -- block ) ;
: parse-mvector ( -- block )
    "}" parse-until <mvector> ;

TUPLE: mhashtable < parsed elements ;
CONSTRUCTOR: <mhashtable> mhashtable ( elements -- block ) ;
: parse-mhashtable ( -- block )
    "}" parse-until <mhashtable> ;

TUPLE: tuple-literal-assoc < parsed name slots ;
TUPLE: tuple-literal-boa < parsed name slots ;
CONSTRUCTOR: <tuple-literal-assoc> tuple-literal-assoc ( name slots -- tuple-literal ) ;
CONSTRUCTOR: <tuple-literal-boa> tuple-literal-boa ( name slots -- tuple-literal ) ;

ERROR: malformed-tuple-literal ;
: parse-tuple-literal ( -- block )
    token
    token dup dup [ name>> ] when {
        { "f" [ drop "}" parse-until <tuple-literal-boa> ] }
        { "{" [
                  drop parse-marray
                  "}" parse-until swap prefix <tuple-literal-assoc>
              ]
        }
        { "}" [ drop f <tuple-literal-boa> ] }
        [ malformed-tuple-literal ]
    } case ;

TUPLE: char < parsed n ;
CONSTRUCTOR: <char> char ( n -- char ) ;
: parse-char ( -- char )
    raw <char> ;

TUPLE: in < parsed name ;
CONSTRUCTOR: <in> in ( name -- in ) ;
: parse-in ( -- in )
    token <in> ;

TUPLE: main < parsed name ;
CONSTRUCTOR: <main> main ( name -- main ) ;
: parse-main ( -- main )
    token <main> ;

TUPLE: escaped < parsed name ;
CONSTRUCTOR: <escaped> escaped ( name -- escaped ) ;
: parse-escaped ( -- escaped )
    raw <escaped> ;

TUPLE: execute( < parsed signature ;
CONSTRUCTOR: <execute(> execute( ( signature -- execute ) ;
: parse-execute( ( -- execute( )
    parse-signature--) <execute(> ;

TUPLE: call( < parsed signature ;
CONSTRUCTOR: <call(> call( ( signature -- call ) ;
: parse-call( ( -- call( )
    parse-signature--) <call(> ;

TUPLE: data-map( < parsed signature ;
CONSTRUCTOR: <data-map(> data-map( ( signature -- data-map ) ;
: parse-data-map( ( -- call( )
    parse-signature--) <data-map(> ;

TUPLE: data-map!( < parsed signature ;
CONSTRUCTOR: <data-map!(> data-map!( ( signature -- data-map! ) ;
: parse-data-map!( ( -- call( )
    parse-signature--) <data-map!(> ;

TUPLE: hints < parsed name sequence ;
CONSTRUCTOR: <hints> hints ( name sequence -- hints ) ;
: parse-hints ( -- generic )
    token body <hints> ;

TUPLE: mgeneric < parsed name signature ;
CONSTRUCTOR: <mgeneric> mgeneric ( name signature -- generic ) ;
: parse-mgeneric ( -- generic )
    token parse-signature(--) <mgeneric> ;

TUPLE: hook < parsed name symbol signature ;
CONSTRUCTOR: <hook> hook ( name symbol signature -- hook ) ;
: parse-hook ( -- hook )
    token token parse-signature(--) <hook> ;

TUPLE: mgeneric# < parsed name n signature ;
CONSTRUCTOR: <mgeneric#> mgeneric# ( name n signature -- generic ) ;
: parse-mgeneric# ( -- generic )
    token token parse-signature(--) <mgeneric#> ;

TUPLE: mmethod < parsed class name body ;
CONSTRUCTOR: <mmethod> mmethod ( class name body -- method ) ;
: parse-mmethod ( -- method )
    parse token body <mmethod> ;

TUPLE: locals-mmethod < parsed class name body ;
CONSTRUCTOR: <locals-mmethod> locals-mmethod ( class name body -- locals-method ) ;
: parse-locals-mmethod ( -- method )
    parse token body <locals-mmethod> ;

TUPLE: constructor < parsed name class ;
CONSTRUCTOR: <constructor> constructor ( name class -- constructor ) ;
: parse-constructor ( -- constructor )
    token token <constructor> ;

! TUPLE: private < parsed body ;
! CONSTRUCTOR: <private> private ( body -- private ) ;
! : parse-private ( -- private )
!    "PRIVATE>" parse-until <private> ;
    
TUPLE: private-begin < parsed ;
CONSTRUCTOR: <private-begin> private-begin ( -- obj ) ;
: parse-private-begin ( -- private-begin ) <private-begin> ;

TUPLE: private-end < parsed ;
CONSTRUCTOR: <private-end> private-end ( -- obj ) ;
: parse-private-end ( -- private-end ) <private-end> ;


TUPLE: from < parsed module functions ;
CONSTRUCTOR: <from> from ( module functions -- from ) ;
: parse-from ( -- from )
    token ";" strings-until <from> ;

TUPLE: qualified < parsed name ;
CONSTRUCTOR: <qualified> qualified ( name -- qualified ) ;
: parse-qualified ( -- qualified )
    token <qualified> ;

TUPLE: qualified-with < parsed name prefix ;
CONSTRUCTOR: <qualified-with> qualified-with ( name prefix -- qualified-with ) ;
: parse-qualified-with ( -- qualified-with )
    token token <qualified-with> ;

TUPLE: constant < parsed name object ;
CONSTRUCTOR: <constant> constant ( name object -- constant ) ;
: parse-constant ( -- constant )
    token parse <constant> ;

TUPLE: mtuple < parsed name body ;
CONSTRUCTOR: <mtuple> mtuple ( name body -- tuple ) ;
: parse-mtuple ( -- mtuple )
    token body <mtuple> ;

TUPLE: merror < parsed name body ;
CONSTRUCTOR: <merror> merror ( name body -- error ) ;
: parse-merror ( -- merror )
    token body <merror> ;

! TUPLE: mparser < parsed name start slots body ;
! CONSTRUCTOR: mparser ( name start slots body -- mparser ) ;
! : parse-mparser ( -- mparser )
    ! get-string parse parse body <mparser> ;

TUPLE: mprimitive < parsed name signature ;
CONSTRUCTOR: <mprimitive> mprimitive ( name signature -- package ) ;
: parse-mprimitive ( -- mprimitive )
    parse parse-signature(--) <mprimitive> ;

TUPLE: package < parsed name ;
CONSTRUCTOR: <package> package ( name -- package ) ;
: parse-package ( -- package )
    get-string <package> ;

TUPLE: import < parsed name ;
CONSTRUCTOR: <import> import ( name -- package ) ;
: parse-import ( -- import )
    get-string <import> ;

TUPLE: imports < parsed names ;
CONSTRUCTOR: <imports> imports ( names -- package ) ;
: parse-imports ( -- import )
    ";" strings-until <imports> ;

TUPLE: foldable < parsed ;
CONSTRUCTOR: <foldable> foldable ( -- obj ) ;
: parse-foldable ( -- foldable ) <foldable> ;

TUPLE: inline < parsed ;
CONSTRUCTOR: <inline> inline ( -- obj ) ;
: parse-inline ( -- inline ) <inline> ;

TUPLE: final < parsed ;
CONSTRUCTOR: <final> final ( -- obj ) ;
: parse-final ( -- final ) <final> ;

TUPLE: recursive < parsed ;
CONSTRUCTOR: <recursive> recursive ( -- obj ) ;
: parse-recursive ( -- recursive ) <recursive> ;

TUPLE: union < parsed name strings ;
CONSTRUCTOR: <union> union ( name strings -- obj ) ;
: parse-union ( -- recursive )
    token body <union> ;

TUPLE: flushable < parsed ;
CONSTRUCTOR: <flushable> flushable ( -- obj ) ;
: parse-flushable ( -- flushable ) <flushable> ;

TUPLE: mbuiltin < parsed name body ;
CONSTRUCTOR: <mbuiltin> mbuiltin ( name body -- obj ) ;
: parse-mbuiltin ( -- builtin )
    token body <mbuiltin> ;

TUPLE: math < parsed name body ;
CONSTRUCTOR: <math> math ( name body -- obj ) ;
: parse-math ( -- builtin )
    token parse-signature(--) <math> ;

TUPLE: functor < parsed name signature definitions ;
CONSTRUCTOR: <functor> functor ( name signature definitions -- functor ) ;
: parse-functor ( -- functor )
    token parse-signature(--) ";FUNCTOR" parse-until <functor> ;

TUPLE: functor-syntax < parsed name body ;
CONSTRUCTOR: <functor-syntax> functor-syntax ( name body -- functor ) ;
: parse-functor-syntax ( -- functor )
    token body <functor-syntax> ;

TUPLE: name < parsed name target ;
CONSTRUCTOR: <name> name ( name target -- object ) ;
: parse-name ( -- name )
    token token <name> ;

TUPLE: ebnf < parsed text ;
CONSTRUCTOR: <ebnf> ebnf ( text -- ebnf ) ;
: parse-ebnf ( -- ebnf )
    ";EBNF" strings-until <ebnf> ;

TUPLE: defer < parsed name ;
CONSTRUCTOR: <defer> defer ( name -- defer ) ;
: parse-defer ( -- defer )
    token <defer> ;

TUPLE: symbol < parsed name ;
CONSTRUCTOR: <symbol> symbol ( name -- symbol ) ;
: parse-symbol ( -- symbol )
    token <symbol> ;

TUPLE: symbols < parsed names ;
CONSTRUCTOR: <symbols> symbols ( names -- symbols ) ;
: parse-symbols ( -- symbols )
    ";" parse-until <symbols> ;

TUPLE: compilation-unit < parsed code ;
CONSTRUCTOR: <compilation-unit> compilation-unit ( code -- compilation-unit ) ;
: parse-compilation-unit ( -- compilation-unit )
    ">>" parse-until <compilation-unit> ;

TUPLE: rename < parsed function module name ;
CONSTRUCTOR: <rename> rename ( function module name -- rename ) ;
: parse-rename ( -- renamed )
    token token "=>" expect token <rename> ;

TUPLE: typedef < parsed old new ;
CONSTRUCTOR: <typedef> typedef ( old new -- typedef ) ;
: parse-typedef ( -- typedef )
    token token <typedef> ;

TUPLE: library < parsed name ;
CONSTRUCTOR: <library> library ( name -- library ) ;
: parse-library ( -- library )
    token <library> ;

TUPLE: c-function < parsed return-value name arguments ;
CONSTRUCTOR: <c-function> c-function ( return-value name arguments -- c-function ) ;
: parse-c-function ( -- c-function )
    token token ";" parse-until <c-function> ;

TUPLE: x-function < parsed return-value name arguments ;
CONSTRUCTOR: <x-function> x-function ( return-value name arguments -- c-function ) ;
: parse-x-function ( -- c-function )
    token token ";" parse-until <x-function> ;

TUPLE: c-function-alias < parsed aliased-name return-value name arguments ;
CONSTRUCTOR: <c-function-alias> c-function-alias ( aliased-name return-value name arguments -- c-function ) ;
: parse-c-function-alias ( -- c-function )
    token token token ";" parse-until <c-function-alias> ;

TUPLE: gl-function < parsed return-value name arguments ;
CONSTRUCTOR: <gl-function> gl-function ( return-value name arguments -- gl-function ) ;
: parse-gl-function ( -- gl-function )
    token token ";" parse-until <gl-function> ;

TUPLE: c-type < parsed name ;
CONSTRUCTOR: <c-type> c-type ( name -- c-type ) ;
: parse-c-type ( -- c-type )
    token <c-type> ;

TUPLE: macro < parsed name signature body ;
CONSTRUCTOR: <macro> macro ( name signature body -- macro ) ;
: parse-macro ( -- macro )
    token parse-signature(--) ";" parse-until <macro> ;

TUPLE: locals-macro < parsed name signature body ;
CONSTRUCTOR: <locals-macro> locals-macro ( name signature body -- macro ) ;
: parse-locals-macro ( -- macro )
    token parse-signature(--) ";" parse-until <locals-macro> ;

TUPLE: struct < parsed name slots ;
CONSTRUCTOR: <struct> struct ( name slots -- struct ) ;
: parse-struct ( -- struct )
    token ";" parse-until <struct> ;

TUPLE: packed-struct < parsed name slots ;
CONSTRUCTOR: <packed-struct> packed-struct ( name slots -- struct ) ;
: parse-packed-struct ( -- struct )
    token ";" parse-until <packed-struct> ;

TUPLE: alias < parsed name target ;
CONSTRUCTOR: <alias> alias ( name target -- alias ) ;
: parse-alias ( -- alias )
    token parse <alias> ;

TUPLE: registers < parsed names ;
CONSTRUCTOR: <registers> registers ( names -- obj ) ;
: parse-registers ( -- obj )
    ";" parse-until <registers> ;

TUPLE: hi-registers < parsed names ;
CONSTRUCTOR: <hi-registers> hi-registers ( names -- obj ) ;
: parse-hi-registers ( -- obj )
    ";" parse-until <hi-registers> ;

TUPLE: about < parsed name ;
CONSTRUCTOR: <about> about ( name -- obj ) ;
: parse-about ( -- obj )
    token <about> ;

TUPLE: article < parsed name body ;
CONSTRUCTOR: <article> article ( name body -- obj ) ;
: parse-article ( -- obj )
    token ";" parse-until <article> ;

TUPLE: c-global < parsed name ;
CONSTRUCTOR: <c-global> c-global ( name -- obj ) ;
: parse-c-global ( -- obj )
    token <c-global> ;

TUPLE: protocol < parsed name functions ;
CONSTRUCTOR: <protocol> protocol ( name functions -- obj ) ;
: parse-protocol ( -- obj )
    token ";" parse-until <protocol> ;

TUPLE: tr < parsed name body ;
CONSTRUCTOR: <tr> tr ( name body -- obj ) ;
: parse-tr ( -- obj )
    token ";" parse-until <tr> ;

TUPLE: exclude < parsed name body ;
CONSTRUCTOR: <exclude> exclude ( name body -- obj ) ;
: parse-exclude ( -- obj )
    token "=>" expect ";" parse-until <exclude> ;

TUPLE: foldable-insn < parsed name body ;
CONSTRUCTOR: <foldable-insn> foldable-insn ( name body -- obj ) ;
: parse-foldable-insn ( -- obj )
    token ";" parse-until <foldable-insn> ;

TUPLE: flushable-insn < parsed name body ;
CONSTRUCTOR: <flushable-insn> flushable-insn ( name body -- obj ) ;
: parse-flushable-insn ( -- obj )
    token ";" parse-until <flushable-insn> ;

TUPLE: vreg-insn < parsed name body ;
CONSTRUCTOR: <vreg-insn> vreg-insn ( name body -- obj ) ;
: parse-vreg-insn ( -- obj )
    token ";" parse-until <vreg-insn> ;

TUPLE: insn < parsed name body ;
CONSTRUCTOR: <insn> insn ( name body -- obj ) ;
: parse-insn ( -- obj )
    token ";" parse-until <insn> ;

TUPLE: codegen < parsed name1 name2 ;
CONSTRUCTOR: <codegen> codegen ( name1 name2 -- obj ) ;
: parse-codegen ( -- obj )
    token token <codegen> ;

TUPLE: conditional < parsed name1 name2 ;
CONSTRUCTOR: <conditional> conditional ( name1 name2 -- obj ) ;
: parse-conditional ( -- obj )
    token token <conditional> ;

TUPLE: simd-128 < parsed name ;
CONSTRUCTOR: <simd-128> simd-128 ( name -- obj ) ;
: parse-simd-128 ( -- obj )
    token <simd-128> ;

TUPLE: simd-128-cord < parsed name1 name2 ;
CONSTRUCTOR: <simd-128-cord> simd-128-cord ( name1 name2 -- obj ) ;
: parse-simd-128-cord ( -- obj )
    token token <simd-128-cord> ;

TUPLE: simd-intrinsic < parsed name body ;
CONSTRUCTOR: <simd-intrinsic> simd-intrinsic ( name body -- obj ) ;
: parse-simd-intrinsic ( -- obj )
    token ";" parse-until <simd-intrinsic> ;

TUPLE: locals-simd-intrinsic < parsed name body ;
CONSTRUCTOR: <locals-simd-intrinsic> locals-simd-intrinsic ( name body -- obj ) ;
: parse-locals-simd-intrinsic ( -- obj )
    token ";" parse-until <locals-simd-intrinsic> ;

TUPLE: enum < parsed name slots ;
CONSTRUCTOR: <enum> enum ( name slots -- obj ) ;
: parse-enum ( -- obj )
    token ";" parse-until <enum> ;

TUPLE: forget < parsed name ;
CONSTRUCTOR: <forget> forget ( name -- obj ) ;
: parse-forget ( -- obj )
    token <forget> ;

TUPLE: pointer < parsed to ;
CONSTRUCTOR: <pointer> pointer ( to -- obj ) ;
: parse-pointer ( -- obj )
    token <pointer> ;

TUPLE: help < parsed name body ;
CONSTRUCTOR: <help> help ( name body -- obj ) ;
: parse-help ( -- help )
    token
    ";" parse-until <help> ;

TUPLE: long-string < parsed name text ;
CONSTRUCTOR: <long-string> long-string ( name text -- long-string ) ;
: parse-long-string ( -- long-string )
    token ";" parse-comment-until <long-string> ;

! \ parse-mparser "PARSER:" register-parser
\ parse-package "PACKAGE:" register-parser
\ parse-import "IMPORT:" register-parser
\ parse-imports "IMPORTS:" register-parser
\ parse-author "AUTHOR:" register-parser
\ parse-from "FROM:" register-parser
\ parse-qualified "QUALIFIED:" register-parser
\ parse-qualified-with "QUALIFIED-WITH:" register-parser
\ parse-use "USE:" register-parser
\ parse-using "USING:" register-parser
\ parse-in "IN:" register-parser
\ parse-main "MAIN:" register-parser
\ parse-mbuiltin "BUILTIN:" register-parser
\ parse-math "MATH:" register-parser
\ parse-union "UNION:" register-parser
\ parse-char "CHAR:" register-parser
\ parse-escaped "\\" register-parser
\ parse-execute( "execute(" register-parser
\ parse-call( "call(" register-parser
\ parse-data-map( "data-map(" register-parser
\ parse-data-map!( "data-map!(" register-parser
\ parse-private-begin "<PRIVATE" register-parser
\ parse-private-end "PRIVATE>" register-parser
\ parse-constant "CONSTANT:" register-parser
\ parse-mtuple "TUPLE:" register-parser
\ parse-merror "ERROR:" register-parser
\ parse-mprimitive "PRIMITIVE:" register-parser
\ parse-foldable "foldable" register-parser
\ parse-flushable "flushable" register-parser
\ parse-inline "inline" register-parser
\ parse-final "final" register-parser
\ parse-recursive "recursive" register-parser
\ parse-block "[" register-parser
\ parse-parsetime-block "$[" register-parser
\ parse-locals-block "[|" register-parser
\ parse-bind ":>" register-parser
\ parse-fry "'[" register-parser
\ parse-marray "{" register-parser
\ parse-mvector "V{" register-parser
\ parse-mhashtable "H{" register-parser
\ parse-tuple-literal "T{" register-parser
\ parse-mgeneric "GENERIC:" register-parser
\ parse-mgeneric# "GENERIC#" register-parser
\ parse-mmethod "M:" register-parser
\ parse-locals-mmethod "M::" register-parser
\ parse-constructor "C:" register-parser
\ parse-function ":" register-parser
\ parse-locals-function "::" register-parser
\ parse-typed "TYPED:" register-parser
\ parse-locals-typed "TYPED::" register-parser
\ parse-memo "MEMO:" register-parser
\ parse-locals-memo "MEMO::" register-parser
\ parse-instance "INSTANCE:" register-parser
\ parse-predicate "PREDICATE:" register-parser
\ parse-slot "SLOT:" register-parser
\ parse-mixin "MIXIN:" register-parser
\ parse-singleton "SINGLETON:" register-parser
\ parse-singletons "SINGLETONS:" register-parser
\ parse-comment "!" register-parser
\ parse-comment "#!" register-parser
\ parse-nested-comment "(*" register-parser
\ parse-postpone "POSTPONE:" register-parser
\ parse-hints "HINTS:" register-parser
\ parse-specialized-array "SPECIALIZED-ARRAY:" register-parser
\ parse-specialized-arrays "SPECIALIZED-ARRAYS:" register-parser
\ parse-syntax "SYNTAX:" register-parser
\ parse-functor "FUNCTOR:" register-parser
\ parse-functor-syntax "FUNCTOR-SYNTAX:" register-parser
\ parse-name "NAME:" register-parser
\ parse-ebnf "EBNF:" register-parser
\ parse-symbol "SYMBOL:" register-parser
\ parse-symbols "SYMBOLS:" register-parser
\ parse-defer "DEFER:" register-parser
\ parse-hook "HOOK:" register-parser
\ parse-compilation-unit "<<" register-parser
\ parse-rename "RENAME:" register-parser
\ parse-typedef "TYPEDEF:" register-parser
\ parse-c-function "FUNCTION:" register-parser
\ parse-x-function "X-FUNCTION:" register-parser
\ parse-c-function-alias "FUNCTION-ALIAS:" register-parser
\ parse-gl-function "GL-FUNCTION:" register-parser
\ parse-library "LIBRARY:" register-parser
\ parse-c-type "C-TYPE:" register-parser
\ parse-macro "MACRO:" register-parser
\ parse-locals-macro "MACRO::" register-parser
\ parse-struct "STRUCT:" register-parser
\ parse-packed-struct "PACKED-STRUCT:" register-parser
\ parse-alias "ALIAS:" register-parser
\ parse-registers "REGISTERS:" register-parser
\ parse-hi-registers "HI-REGISTERS:" register-parser
\ parse-about "ABOUT:" register-parser
\ parse-c-global "C-GLOBAL:" register-parser
\ parse-article "ARTICLE:" register-parser
\ parse-protocol "PROTOCOL:" register-parser
\ parse-exclude "EXCLUDE:" register-parser
\ parse-foldable-insn "FOLDABLE-INSN:" register-parser
\ parse-flushable-insn "FLUSHABLE-INSN:" register-parser
\ parse-insn "INSN:" register-parser
\ parse-vreg-insn "VREG-INSN:" register-parser
\ parse-codegen "CODEGEN:" register-parser
\ parse-conditional "CONDITIONAL:" register-parser
\ parse-simd-128 "SIMD-128:" register-parser
\ parse-simd-128-cord "SIMD-128-CORD:" register-parser
\ parse-simd-intrinsic "SIMD-INTRINSIC:" register-parser
\ parse-locals-simd-intrinsic "SIMD-INTRINSIC::" register-parser
\ parse-enum "ENUM:" register-parser
\ parse-forget "FORGET:" register-parser
\ parse-pointer "pointer:" register-parser
\ parse-long-string "STRING:" register-parser
\ parse-help "HELP:" register-parser

! qw{ CODEGEN: CATEGORY: ENUM: SIMD-INTRINSIC:
! CLASS: FUNCTION-ALIAS: M\\ TAGS: COM-INTERFACE:

! "EUC:" "GIR:" "8-BIT:" "CLASS:" "ENUM:" "RENAMING:" "TEST:" "CHLOE:" "FORGET:" "XML-NS:"
! "BEFORE:" "AFTER:" "IMPLEMENT-STRUCTS:" "DESTRUCTOR:" "FOREIGN-RECORD-TYPE:" "FOREIGN-ENUM-TYPE:" "FORWARD-ANALYSIS:"
! "COM-INTERFACE:" "INTERSECTION:" "LOG:" "CFSTRING:" "CONSULT:" "PEG:" "BACKWARD-ANALYSIS:" "MACRO::"
! "UNION-STRUCT:" "PACKED-STRUCT:" "SLOT-PROTOCOL:" "ICON:" "TAGS:" "TAG:" "ROMAN-OP:"
! "STRING:" "SIMD-128:" "MATCH-VARS:" "CALLBACK:" "CATEGORY:" "PIXEL-FORMAT-ATTRIBUTE-TABLE:" "pointer:"
! "CATEGORY-NOT:" "FUNCTOR-SYNTAX:" "IDENTITY-MEMO:" "XML-ERROR:" "RULE:" "COMPONENT:" "X509_V_:"
! "X-FUNCTION:" "SIMD-INTRINSIC:" "SIMD-INTRINSIC::"

! "MAIN-WINDOW:" "GIR:" "TUPLE-ARRAY:" "VARIANT:" "SPECIALIZED-VECTOR:" "FOREIGN-RECORD-TYPE:" "FOREIGN-ATOMIC-TYPE:"
! "SOLUTION:" "CUDA-LIBRARY:" "CUDA-FUNCTION:" "HOLIDAY:" "HOLIDAY-NAME:" "FRAMEWORK:" "GAME:"
! "INTERSECTION:" "DESTRUCTOR:" "FORWARD-ANALYSIS:" "RENAMING:" "MACRO::" "XML-NS:" "CALLBACK:" "GLSL-SHADER:"
! "UNIFORM-TUPLE:" "GLSL-PROGRAM:" "LITERAL-PARSER:" "CONSTRUCTOR:" "STRING:" "ENUM:" "GLSL-SHADER-FILE:"
! "AFTER:" "system-attachment:" "initial:" "color-attachment:" "CFSTRING:" "DERIVATIVE:" "FUNCTOR-SYNTAX:"
! "VERTEX-FORMAT:" "SELECTOR:" "REGISTER:" "METHOD:" "TODO:" "LAZY:"
! "ROLE:" "UNION-STRUCT:" "INSTRUCTION:" "SUBROUTINE:"

! "resource:core" vocabs-in-root [ vocab? ] filter [ vocab-files ] map concat
! "resource:core" vocabs-in-root [ vocab? ] filter [ vocab-files ] map concat
! [ "-docs.factor" tail? not ] filter
! [ "-tests.factor" tail? not ] filter [ parse-file ] map

! "resource:basis" vocabs-in-root [ vocab? ] filter [ vocab-files ] map concat
! [ "-docs.factor" tail? not ] filter
! [ "-tests.factor" tail? not ] filter
! { "resource:basis/tools/completion/completion.factor"
! "resource:basis/functors/functors.factor" "resource:basis/ui/tools/listener/completion/completion.factor"
! "resource:basis/simple-tokenizer/simple-tokenizer.factor" } diff
! [ dup . flush parse-file drop ] map

(*
"resource:core" root-name>modules [ path>files ] map concat
{ "/home/erg/factor/core/vocabs/loader/test/a/a.factor"
 "/home/erg/factor/core/vocabs/loader/test/c/c.factor"
"/home/erg/factor/core/vocabs/loader/test/b/b.factor" } diff
[ dup . flush parse-file ] map
*)
