! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays io kernel modern.quick-parser multiline ;
IN: modern.syntax
QUALIFIED: sequences
QUALIFIED: strings


/*
! Can go away:
LEXER: heredoc HEREDOC: token pick strings:>string multiline-string-until ;
! string literals: P" URL" SBUF" DLL"

LEXER: compilation-unit-begin << ; ! going away
LEXER: compilation-unit-end >> ; ! going away

LEXER: regexp-/ R/ "/" multiline-string-until ;
LEXER: regexp-# R# "#" multiline-string-until ;
LEXER: regexp-' R' "'" multiline-string-until ;
LEXER: regexp-( R( "(" multiline-string-until ;
LEXER: regexp-@ R@ "@" multiline-string-until ;
LEXER: regexp-| R| "|" multiline-string-until ;
LEXER: regexp-! R! "!" multiline-string-until ;

! words[
! LEXER: block [ "]" parse-until ;
! LEXER: fry '[ "]" parse-until ;
! LEXER: block-eval $[ "]" parse-until ;
! LEXER: set-quot set[ "]" parse-until ;
! LEXER: get-quot get[ "]" parse-until ;
! LEXER: slots-quot slots[ "]" parse-until ;
! LEXER: set-slots-quot set-slots[ "]" parse-until ;
! LEXER: memo-block MEMO[ "]" parse-until ;

LEXER: block-locals [| "]" parse-until ;
LEXER: pipe-separator | ;

! words{
! LEXER: array { "}" parse-until ;
! LEXER: vector V{ "}" parse-until ;
! LEXER: bitset ?{ "}" parse-until ;
! LEXER: eval-dollar-array ${ "}" parse-until ; ! going away
! LEXER: byte-array B{ "}" parse-until ;
! LEXER: byte-vector BV{ "}" parse-until ;
! LEXER: hashtable H{ "}" parse-until ;
! LEXER: hash-set HS{ "}" parse-until ;
! LEXER: tuple-literal T{ existing-class "}" parse-until ;
! LEXER: callstack-literal CS{ "}" parse-until ;
! LEXER: complex-literal C{ "}" parse-until ;
! LEXER: dlist-literal DL{ "}" parse-until ;
! LEXER: wrapper-literal W{ "}" parse-until ;
! LEXER: struct-literal S{ "}" parse-until ;
! LEXER: identity-hash-set IHS{ "}" parse-until ;
! LEXER: identity-hashtable IH{ "}" parse-until ;
! LEXER: set-array set{ "}" parse-until ;
! LEXER: get-array get{ "}" parse-until ;
! LEXER: slots-array slots{ "}" parse-until ;
! LEXER: set-slots-array set-slots{ "}" parse-until ;
! LEXER: copy-slots-array copy-slots{ "}" parse-until ;
! LEXER: flags flags{ "}" parse-until ;
! LEXER: union-array union{ "}" parse-until ;
! LEXER: intersection-array intersection{ "}" parse-until ;
! LEXER: maybe maybe{ "}" parse-until ;
! LEXER: not not{ "}" parse-until ;
! LEXER: c-array c-array{ "}" parse-until ;

! LEXER: shaped-array sa{ "}" parse-until ;
! LEXER: avl AVL{ "}" parse-until ;
! LEXER: splay SPLAY{ "}" parse-until ;
! LEXER: tree TREE{ "}" parse-until ;
! LEXER: suffix-array SA{ "}" parse-until ;
! LEXER: valist VA{ "}" parse-until ;
! LEXER: vlist VL{ "}" parse-until ;
! LEXER: number-hash-set NHS{ "}" parse-until ;
! LEXER: number-hashtable NH{ "}" parse-until ;
! LEXER: nibble-array N{ "}" parse-until ;
! LEXER: persistent-hashtable PH{ "}" parse-until ;
! LEXER: persistent-vector PV{ "}" parse-until ;
! LEXER: sequence-hash-set SHS{ "}" parse-until ;
! LEXER: sequence-hashtable SH{ "}" parse-until ;
! LEXER: quote-word qw{ "}" parse-until ;
! LEXER: bit-vector ?V{ "}" parse-until ;
! LEXER: poker-hand HAND{ "}" parse-until ;
! LEXER: hex-array HEX{ "}" parse-until ;

! words(
! LEXER: signature ( ")" parguments typed-raw-until ;
! LEXER: execute-parens execute( (parse-psignature) ;
! LEXER: call-parens call( (parse-psignature) ;
! LEXER: eval-parens eval( (parse-psignature) ;
! LEXER: data-map-parens data-map( (parse-psignature) ;
! LEXER: data-map!-parens data-map!( (parse-psignature) ;
! LEXER: shuffle-parens shuffle( (parse-psignature) ;

! LEXER: c-comment /* "* /" multiline-string-until ;

! BEGIN REGULAR WORDS
LEXER: f f ;
LEXER: private-begin <PRIVATE ;
LEXER: private-end PRIVATE> ;
LEXER: BAD-ALIEN BAD-ALIEN ; ! alien.syntax
LEXER: delimiter delimiter ; ! XXX: not really used?
LEXER: deprecated deprecated ;
LEXER: final final ;
LEXER: flushable flushable ;
LEXER: foldable foldable ;
LEXER: inline inline ;
LEXER: recursive recursive ;
LEXER: breakpoint B ;
LEXER: call-next-method call-next-method ;
LEXER: no-compile no-compile ; ! extra/benchmark/raytracer-simd/raytracer-simd.factor

! Compiler
LEXER: d-register D: token ;
LEXER: r-register R: token ;

! opengl break
LEXER: gb GB ;

! XXX: cpu.8080
LEXER: instruction INSTRUCTION: ";" raw-until ;
LEXER: cycles cycles: token ;
LEXER: opcode opcode: token ;

! Single token parsers that need rename (?)
LEXER: in IN: token ;
LEXER: use USE: token ;

LEXER: unuse UNUSE: token ; ! XXX: remove this
LEXER: exclude EXCLUDE: token "=>" expect ";" raw-until ; ! XXX: remove
LEXER: rename RENAME: raw raw "=>" expect raw ; ! XXX: remove
LEXER: from FROM: token "=>" expect ";" raw-until ; ! XXX: remove
LEXER: qualified QUALIFIED: token ; ! XXX: make implicit
LEXER: qualified-with QUALIFIED-WITH: token token ; ! XXX: remove

LEXER: forget FORGET: token ; ! repl only (?)

LEXER: guid GUID: raw ;

LEXER: selector SELECTOR: token ; ! Smalltalk
LEXER: storage STORAGE: token ; ! units

! Nice, regular uppercase read til ; parsers
LEXER: using USING: ";" parse-until ;
LEXER: syntax-word SYNTAX: raw body ; ! needs \
LEXER: parser LEXER: raw raw body ; ! needs \

! Upper case but not explicit end
LEXER: c-function FUNCTION: token new-word parse ;
LEXER: function-alias FUNCTION-ALIAS: token token new-word parse ;
LEXER: x-function X-FUNCTION: token new-word parse ;
LEXER: gl-function GL-FUNCTION: token new-word parse parse ;
LEXER: cuda-function CUDA-FUNCTION: new-word parse ; ! no return value
LEXER: cuda-global CUDA-GLOBAL: new-word ;
LEXER: cuda-library CUDA-LIBRARY: new-word existing-class token ; ! XXX: token might have spaces...
LEXER: c-callback CALLBACK: token token parse ;
LEXER: subroutine SUBROUTINE: token parse ;

! words[ funky
LEXER: let-block [let "]" parse-until ;
LEXER: interpolate [I "I]" multiline-string-until ;
LEXER: xml-bracket [XML "XML]" multiline-string-until ;
LEXER: infix [infix "infix]" multiline-string-until ;
LEXER: morse [MORSE "MORSE]" multiline-string-until ;
LEXER: ebnf-bracket [EBNF token "EBNF]" multiline-string-until ; ! going away
LEXER: ebnf-acute <EBNF token "EBNF>" multiline-string-until ; ! going away
LEXER: literate <LITERATE "LITERATE>" multiline-string-until ;
LEXER: xml-acute <XML "XML>" multiline-string-until ;

LEXER: applescript APPLESCRIPT: new-word ";APPLESCRIPT" multiline-string-until ;
LEXER: long-string STRING: token "\n;" multiline-string-until ;
LEXER: glsl-shader GLSL-SHADER: token token "\n;" multiline-string-until ;
LEXER: ebnf EBNF: token ";EBNF" multiline-string-until ;

! words@
LEXER: struct-literal-at S@ token parse ; ! [[ ]]
LEXER: c-array@ c-array@ parse parse parse ; ! [[ ]]

! words:
LEXER: function : new-word parse body ;
LEXER: function-locals :: new-word parse body ;
LEXER: alias ALIAS: new-word existing-word ;
LEXER: typed TYPED: new-word parse body ;
LEXER: typed-locals TYPED:: new-word parse body ;
LEXER: memo MEMO: new-word parse body ;
LEXER: memo-locals MEMO:: new-word parse body ;
LEXER: identity-memo IDENTITY-MEMO: new-word parse body ;
LEXER: identity-memo-locals IDENTITY-MEMO:: new-word parse body ;
LEXER: macro MACRO: new-word parse body ;
LEXER: macro-locals MACRO:: new-word parse body ;
LEXER: peg PEG: new-word parse body ;
LEXER: descriptive DESCRIPTIVE: new-word parse body ;
LEXER: descriptive-locals DESCRIPTIVE:: new-word parse body ;
LEXER: constructor-new CONSTRUCTOR: token token parse body ;
LEXER: primitive PRIMITIVE: new-word parse ;
! XXX: new def, can't use yet
! LEXER: functor FUNCTOR: token parse ";FUNCTOR" parse-until ;
LEXER: functor FUNCTOR: token ";FUNCTOR" multiline-string-until ;
LEXER: functor-syntax FUNCTOR-SYNTAX: token body ;
LEXER: generic GENERIC: new-class parse ;
LEXER: generic# GENERIC# new-class token parse ;
LEXER: hook HOOK: new-class existing-word parse ;
LEXER: method M: existing-class existing-word body ;
LEXER: method-locals M:: existing-class existing-word body ;
LEXER: math MATH: new-word parse ;
LEXER: pair-generic PAIR-GENERIC: new-class parse ;
LEXER: pair-m PAIR-M: existing-class existing-class existing-word body ;
LEXER: tags TAGS: new-word parse ;
LEXER: tag TAG: token existing-word body ;
LEXER: rule RULE: new-word ";" raw-until ;
LEXER: roman ROMAN: token ;
LEXER: roman-op ROMAN-OP: raw parse ;
LEXER: lazy LAZY: new-word parse body ;
LEXER: infix-locals INFIX:: new-word parse body ;


LEXER: constant CONSTANT: token parse ;
LEXER: symbol SYMBOL: raw ;
LEXER: symbols SYMBOLS: new-words ;
LEXER: postpone POSTPONE: raw ;
LEXER: defer DEFER: token ;
LEXER: char CHAR: raw ;
LEXER: alien ALIEN: token ;

LEXER: tuple TUPLE: new-class body ;
LEXER: struct STRUCT: new-class body ;
LEXER: packed-struct PACKED-STRUCT: new-class body ;
LEXER: le-packed-struct LE-PACKED-STRUCT: new-class body ;
LEXER: be-packed-struct BE-PACKED-STRUCT: new-class body ;
LEXER: le-struct LE-STRUCT: new-class body ;
LEXER: be-struct BE-STRUCT: new-class body ;
LEXER: union-struct UNION-STRUCT: new-class body ;
LEXER: error ERROR: new-class body ;
LEXER: slot SLOT: token ;
LEXER: constructor C: token token ;

LEXER: com-interface COM-INTERFACE: existing-word new-word parse ";" parse-until ;
LEXER: typedef TYPEDEF: token token ;
LEXER: library LIBRARY: token ;
LEXER: c-type C-TYPE: token ;
LEXER: c-global C-GLOBAL: token token ;
LEXER: hints HINTS: parse body ;
LEXER: builtin BUILTIN: existing-class body ;
LEXER: main MAIN: existing-word ;

LEXER: destructor DESTRUCTOR: existing-word ;
LEXER: predicate PREDICATE: new-class "<" expect parse body ;
LEXER: mixin MIXIN: new-class ;
LEXER: instance INSTANCE: existing-class existing-class ;
LEXER: singleton SINGLETON: new-class ;
LEXER: singletons SINGLETONS: body ;
LEXER: import IMPORT: token ;
LEXER: imports IMPORTS: ";" raw-until ;
LEXER: special-object SPECIAL-OBJECT: token parse ;
LEXER: union UNION: new-class body ;
LEXER: intersection INTERSECTION: token body ;
LEXER: unicode-category CATEGORY: token body ;
LEXER: unicode-category-not CATEGORY-NOT: token body ;

LEXER: specialized-array SPECIALIZED-ARRAY: token ;
LEXER: specialized-arrays SPECIALIZED-ARRAYS: ";" raw-until ;
LEXER: specialized-vector SPECIALIZED-VECTOR: token ;
LEXER: specialized-vectors SPECIALIZED-VECTORS: ";" raw-until ;
LEXER: vectored-struct VECTORED-STRUCT: existing-class ;

LEXER: glsl-program GLSL-PROGRAM: token body ;
LEXER: uniform-tuple UNIFORM-TUPLE: token body ;

LEXER: test TEST: token ;
LEXER: registers REGISTERS: body ;
LEXER: hi-registers HI-REGISTERS: body ;
LEXER: color COLOR: token ;
LEXER: hexcolor HEXCOLOR: token ;
LEXER: flexhexcolor FLEXHEXCOLOR: token ;

LEXER: about ABOUT: token ;
LEXER: article ARTICLE: token body ;
LEXER: rotocol PROTOCOL: token body ;

LEXER: insn INSN: new-word body ;
LEXER: vreg-insn VREG-INSN: token body ;
LEXER: flushable-insn FLUSHABLE-INSN: new-word body ;
LEXER: foldable-insn FOLDABLE-INSN: new-word body ;

LEXER: codegen CODEGEN: token token ;
LEXER: conditional CONDITIONAL: token token ;
LEXER: simd-128 SIMD-128: token ;
LEXER: simd-128-cord SIMD-128-CORD: token token ;
LEXER: simd-instrinsic SIMD-INTRINSIC: token body ;
LEXER: simd-instrinsic-locals SIMD-INTRINSIC:: token body ;
LEXER: enum ENUM: token body ;
LEXER: pointer pointer: token ;
LEXER: help HELP: raw body ;
LEXER: name NAME: token token ;
LEXER: tr TR: token body ;

LEXER: backward-analysis BACKWARD-ANALYSIS: token ;
LEXER: forward-analysis FORWARD-ANALYSIS: token ;
LEXER: register REGISTER: token ;

LEXER: log LOG: token token ;
LEXER: nan NAN: token ;
LEXER: broadcast BROADCAST: existing-word existing-class parse ;
LEXER: consult CONSULT: new-word existing-class body ;

LEXER: mirc IRC: token parse ";" raw-until ;
LEXER: main-window MAIN-WINDOW: token parse body ;
LEXER: game GAME: token parse body ;
LEXER: solution SOLUTION: token ;

LEXER: 8-bit 8-BIT: token token token ;
LEXER: euc EUC: new-class parse ;

LEXER: breakpoint-parse-time B: raw ; ! going away

LEXER: icon ICON: new-word token ;
LEXER: match-vars MATCH-VARS: body ;
LEXER: pixel-format-attribute-table PIXEL-FORMAT-ATTRIBUTE-TABLE: new-word parse parse ;
LEXER: rect RECT: parse parse ;

LEXER: c-global-literal &: token ;
LEXER: slot-constructor SLOT-CONSTRUCTOR: token ;
LEXER: slot-protocol SLOT-PROTOCOL: ";" raw-until ;
LEXER: tip TIP: ";" parse-until ;
LEXER: tokenizer TOKENIZER: existing-word ; ! hmm
LEXER: x509 X509_V_: new-word token ;
LEXER: role ROLE: ";" raw-until ;
LEXER: role-tuple ROLE-TUPLE: ";" raw-until ;
LEXER: variant VARIANT: ";" raw-until ;
LEXER: variant-member VARIANT-MEMBER: ";" raw-until ;
LEXER: decimal DECIMAL: token ;
LEXER: after AFTER: existing-class existing-word body ;
LEXER: before BEFORE: existing-class existing-word body ;
LEXER: chloe CHLOE: new-word body ;
LEXER: component COMPONENT: token ;
LEXER: derivative DERIVATIVE: existing-word body ;

LEXER: tuple-array TUPLE-ARRAY: token ;
LEXER: glsl-shader-file GLSL-SHADER-FILE: new-word existing-class parse ;

! gobject-instrospection
LEXER: gif GIR: token ;
LEXER: foreign-atomic-type FOREIGN-ATOMIC-TYPE: token token ;
LEXER: foreign-enum-type FOREIGN-ENUM-TYPE: token token ;
LEXER: foreign-record-type FOREIGN-RECORD-TYPE: token token ;
LEXER: implement-structs IMPLEMENT-STRUCTS: ";" raw-until ;

LEXER: holiday HOLIDAY: new-word body ;
LEXER: holiday-name HOLIDAY-NAME: existing-word existing-class parse ;

LEXER: pool POOL: existing-class token ;
LEXER: mdbtuple MDBTUPLE: ";" parse-until ;

LEXER: py-from PY-FROM: new-word "=>" expect ";" parse-until ;
LEXER: py-methods PY-METHODS: new-word "=>" expect ";" parse-until ;
LEXER: py-qualified-from PY-QUALIFIED-FROM: new-word "=>" expect ";" parse-until ;
LEXER: renaming RENAMING: new-word parse parse parse ;
LEXER: roll ROLL: token ;

LEXER: singletons-union SINGLETONS-UNION: new-class ";" parse-until ;
! slides
LEXER: strip-tease STRIP-TEASE: ";" parse-until ;
LEXER: use-rev USE-REV: token token ;
LEXER: vertext-format VERTEX-FORMAT: new-word ";" parse-until ;
LEXER: vertext-struct VERTEX-STRUCT: token token ;
LEXER: xkcd XKCD: token ;
LEXER: xml-error XML-ERROR: new-class ";" raw-until ;
LEXER: xml-ns XML-NS: new-word token ;
LEXER: feedback-format feedback-format: token ;
LEXER: geometry-shader-vertices-out geometry-shader-vertices-out: parse ;

! funky
: parse-bind ( n string -- seq n'/f string )
    raw pick "(" sequences:sequence= [
        ")" raw-until [ swap sequences:prefix ] 2dip
    ] when ;
LEXER: bind :> parse-bind ;

! words\
LEXER: escaped \ raw ;
LEXER: method-literal M\ existing-class existing-word ;

! funky readahead one, need this for things like CONSTANT: foo $ init-foo
! XXX: Maybe call it $: instead.
LEXER: eval-dollar $ parse ;

! Cocoa
LEXER: cfstring CFSTRING: new-word parse ;

LEXER: class CLASS: new-class "<" expect ";" parse-until ;
LEXER: cocoa-method METHOD: token ";" parse-until ;
LEXER: cocoa-protocol COCOA-PROTOCOL: token ;

LEXER: framework FRAMEWORK: parse ;
LEXER: SEL: SEL: token ;
LEXER: super-selector SUPER-> token ;
! LEXER: cocoa-selector -> token ;

*/