! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators combinators.short-circuit
combinators.smart continuations fry io io.encodings.utf8
io.files io.streams.string kernel modern modern.paths
modern.slices multiline namespaces prettyprint sequences sets
splitting strings arrays ;
IN: modern.out

SYMBOL: last-slice

: write-whitespace ( obj -- )
    [ last-slice get [ swap slice-between ] [ slice-before ] if* io:write ]
    [ last-slice namespaces:set ] bi ;

DEFER: write-literal
GENERIC: write-literal ( obj -- )
! M: object write-literal lexed-underlying write ;
M: string write-literal write ;
M: slice write-literal [ write-whitespace ] [ write ] bi ;

M: array write-literal [ write-literal ] each ;

M: tag-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> write ]
    } cleave ;

M: single-matched-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> write ]
        [ payload>> [ write-literal ] each ] ! don't need write-whitespace here, the recursion does it
        [ seq>> 3 swap nth lexed-underlying write-whitespace ]
        [ closing-tag>> write ]
    } cleave ;

M: double-matched-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> io:write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ seq>> 2 swap nth write-whitespace ]
        [ payload>> io:write ]
        [ seq>> 3 swap nth write-whitespace ]
        [ delimiter>> matching-delimiter-string io:write ]
    } cleave ;

M: dquote-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> io:write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ seq>> 2 swap nth write-whitespace ]
        [ payload>> io:write ]
        [ seq>> 3 swap nth write-whitespace ]
        [ delimiter>> matching-delimiter-string io:write ]
    } cleave ;

M: backtick-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> io:write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ seq>> 2 swap nth write-whitespace ]
        [ payload>> io:write ]
    } cleave ;

M: backslash-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> io:write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ seq>> 2 swap nth write-whitespace ]
        [ payload>> io:write ]
    } cleave ;

M: line-comment-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> io:write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ seq>> 2 swap nth write-whitespace ]
        [ payload>> io:write ]
    } cleave ;

M: uppercase-colon-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> write ]
        [ payload>> [ write-literal ] each ] ! don't need write-whitespace here, the recursion does it
        [ seq>> 3 swap nth lexed-underlying write-whitespace ]
        [ closing-tag>> write ]
    } cleave ;

M: lowercase-colon-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ tag>> io:write ]
        [ seq>> 1 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ payload>> write-literal ] ! don't need write-whitespace here, the recursion does it
    } cleave ;

M: left-decorator-literal write-literal
    {
        [ seq>> 0 swap nth write-whitespace ]
        [ delimiter>> io:write ]
        [ payload>> write-literal ] ! don't need write-whitespace here, the recursion does it
    } cleave ;

M: right-decorator-literal write-literal
    {
        [ payload>> write-literal ] ! don't need write-whitespace here, the recursion does it
        [ seq>> 0 swap nth write-whitespace ]
        [ delimiter>> io:write ]
    } cleave ;

M: compound-literal write-literal
    sequence>> [ write-literal ] each ;

M: compound-sequence-literal write-literal
    sequence>> [ write-literal ] each ;

! Swap in write-literal for renaming

: write-modern-loop ( quot -- )
    [ write-literal ] each ; inline

: write-modern-string ( seq -- string )
    [ write-modern-loop ] with-string-writer ; inline

: write-modern-path ( seq path -- )
    utf8 [ write-modern-loop nl ] with-file-writer ; inline

: map-literals ( obj quot: ( obj -- obj' ) -- seq )
    over single-matched-literal? [
        [ call drop ] [
            '[
                dup compound-sequence-literal? [ sequence>> ] when
                [ _ map-literals ] map
            ] change-payload
        ] 2bi
    ] [
        call
    ] if ; inline recursive

: rewrite-path ( path quot -- )
    ! dup print
    '[ [ path>literals [ _ map-literals ] map ] [ ] bi write-modern-path ]
    [ drop . ] recover ; inline

: rewrite-string ( string quot -- )
    ! dup print
    [ string>literals ] dip '[ _ map-literals ] map write-modern-string ; inline

: rewrite-paths ( seq quot -- ) '[ _ rewrite-path ] each ; inline
: lexable-core-paths ( -- seq ) core-source-paths ;
: lexable-basis-paths ( -- seq )
    basis-source-paths {
        "resource:basis/bit-arrays/bit-arrays.factor"
        "resource:basis/bit-vectors/bit-vectors.factor"
        "resource:basis/csv/csv.factor"
        "resource:basis/dlists/dlists.factor"
        "resource:basis/eval/eval.factor"
        "resource:basis/farkup/farkup.factor"
        "resource:basis/fry/fry.factor"
        "resource:basis/linked-assocs/linked-assocs.factor"
        "resource:basis/literals/literals.factor"
        "resource:basis/nibble-arrays/nibble-arrays.factor"
        "resource:basis/shuffle/shuffle.factor"
        "resource:basis/simple-tokenizer/simple-tokenizer.factor"
        "resource:basis/specialized-arrays/specialized-arrays.factor"
        "resource:basis/specialized-vectors/specialized-vectors.factor"
        "resource:basis/suffix-arrays/suffix-arrays.factor"
        "resource:basis/urls/urls.factor"
        "resource:basis/vlists/vlists.factor"
        "resource:basis/alien/data/data.factor"
        "resource:basis/alien/syntax/syntax.factor"
        "resource:basis/byte-arrays/hex/hex.factor"
        "resource:basis/classes/struct/struct.factor"
        "resource:basis/cocoa/messages/messages.factor"
        "resource:basis/db/postgresql/errors/errors.factor"
        "resource:basis/hash-sets/identity/identity.factor"
        "resource:basis/hash-sets/sequences/sequences.factor"
        "resource:basis/hashtables/identity/identity.factor"
        "resource:basis/hashtables/sequences/sequences.factor"
        "resource:basis/help/handbook/handbook.factor"
        "resource:basis/help/html/html.factor"
        "resource:basis/html/templates/fhtml/fhtml.factor"
        "resource:basis/http/parsers/parsers.factor"
        "resource:basis/io/encodings/iso2022/iso2022.factor"
        "resource:basis/json/reader/reader.factor"
        "resource:basis/json/writer/writer.factor"
        "resource:basis/math/complex/complex.factor"
        "resource:basis/math/vectors/simd/simd.factor"
        "resource:basis/math/vectors/simd/cords/cords.factor"
        "resource:basis/memoize/syntax/syntax.factor"
        "resource:basis/peg/ebnf/ebnf.factor"
        "resource:basis/peg/parsers/parsers.factor"
        "resource:basis/persistent/hashtables/hashtables.factor"
        "resource:basis/persistent/vectors/vectors.factor"
        "resource:basis/regexp/parser/parser.factor"
        "resource:basis/xml/autoencoding/autoencoding.factor"
        "resource:basis/xml/dtd/dtd.factor"
        "resource:basis/xml/elements/elements.factor"
        "resource:basis/xml/entities/entities.factor"
    } diff ;

: lexable-extra-paths ( -- seq )
    extra-source-paths {
        "resource:extra/brainfuck/brainfuck.factor"  ! EBNF: [[ ]] ;
        "resource:extra/cuesheet/cuesheet.factor"    ! CHAR: "
        "resource:extra/fjsc/fjsc.factor"            ! EBNF:
        "resource:extra/emojify/emojify.factor"      ! R/
        "resource:extra/gml/gml.factor"
        "resource:extra/metar/metar.factor"          ! R/
        "resource:extra/morse/morse.factor"
        "resource:extra/rosetta-code/balanced-brackets/balanced-brackets.factor"
        "resource:extra/flip-text/flip-text.factor"
        "resource:extra/ini-file/ini-file.factor"
        "resource:extra/poker/poker.factor"
        "resource:extra/qw/qw.factor"
        "resource:extra/svg/svg.factor"
        "resource:extra/text-to-pdf/text-to-pdf.factor"
        "resource:extra/tnetstrings/tnetstrings.factor"
        "resource:extra/trees/trees.factor"
        "resource:extra/alien/data/map/map.factor"
        "resource:extra/arrays/shaped/shaped.factor"
        "resource:extra/bunny/outlined/outlined.factor"
        "resource:extra/c/lexer/lexer.factor"
        "resource:extra/c/preprocessor/preprocessor.factor"
        "resource:extra/gml/parser/parser.factor"
        "resource:extra/gml/runtime/runtime.factor"
        "resource:extra/gpu/effects/blur/blur.factor"
        "resource:extra/hash-sets/numbers/numbers.factor"
        "resource:extra/hashtables/numbers/numbers.factor"
        "resource:extra/html/parser/parser.factor"
        "resource:extra/infix/parser/parser.factor"
        "resource:extra/infix/tokenizer/tokenizer.factor"
        "resource:extra/parser-combinators/simple/simple.factor"
        "resource:extra/pdf/values/values.factor"
        "resource:extra/peg/pl0/pl0.factor"
        "resource:extra/peg/javascript/parser/parser.factor"
        "resource:extra/peg/javascript/tokenizer/tokenizer.factor"
        "resource:extra/project-euler/011/011.factor"
        "resource:extra/rosetta-code/balanced-brackets/balanced-bracke..."
        "resource:extra/slots/syntax/syntax.factor"
        "resource:extra/smalltalk/parser/parser.factor"
        "resource:extra/talks/galois-talk/galois-talk.factor"
        "resource:extra/talks/google-tech-talk/google-tech-talk.factor"
        "resource:extra/talks/otug-talk/otug-talk.factor"
        "resource:extra/talks/vpri-talk/vpri-talk.factor"
        "resource:extra/trees/avl/avl.factor"
        "resource:extra/trees/splay/splay.factor"
        "resource:extra/yaml/conversion/conversion.factor"
    } diff ;

/*
! These work except they use pegs/ebnf, grep for [[ ]]
	modified:   basis/db/sqlite/errors/errors.factor
	modified:   basis/formatting/formatting.factor
	modified:   basis/globs/globs.factor
	modified:   extra/alien/fortran/fortran.factor
	modified:   extra/cpu/8080/emulator/emulator.factor
	modified:   extra/peg/expr/expr.factor
	modified:   extra/rosetta-code/arithmetic-evaluation/arithmetic-evaluation.factor
	modified:   extra/shell/parser/parser.factor
*/

: lexable-paths ( -- seq )
    [
        lexable-core-paths
        lexable-basis-paths
        lexable-extra-paths
    ] append-outputs ;

: paren-word>tick-word ( string -- string' )
    dup [ "(" ?head drop ")" ?tail drop "'" append ] [ ] if ;

: paren-word-name? ( string -- ? )
    { [ "(" head? ] [ ")" tail? ] } 1&& ;

: transform-paren-word>tick-word ( token -- token' )
    dup { [ tag-literal? ] [ tag>> paren-word-name? ] } 1&& [
        [ paren-word>tick-word ] change-tag
    ] when ;

: single-line-comment? ( token -- ? )
    { [ line-comment-literal? ] [ delimiter>> "!" sequence= ] } 1&& ;

: transform-single-line-comment>hash-comment ( token -- token' )
    dup single-line-comment? [
        [ drop "#" ] change-delimiter
    ] when ;

: transform-source ( quot -- )
    lexable-paths swap rewrite-paths ; inline

: transform-core ( quot -- )
    lexable-core-paths swap rewrite-paths ; inline