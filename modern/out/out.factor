! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators.short-circuit combinators.smart
continuations fry io io.encodings.string io.encodings.utf8
io.files io.streams.string kernel modern modern.paths
modern.slices namespaces prettyprint sequences sets splitting
strings ;
IN: modern.out

SYMBOL: last-slice

GENERIC: write-modern-literal ( obj -- )
M: token-literal write-modern-literal payload>> write ;
M: object write-modern-literal underlying write ;
! M: single-literal write-modern-literal drop ;
! M: double-literal write-modern-literal drop ;
! M: string-literal write-modern-literal drop ;
! M: backtick-literal write-modern-literal drop ;
! M: backslash-literal write-modern-literal drop ;
M: til-eol-literal write-modern-literal [ tag>> ] [ payload>> ] bi [ io:write ] bi@ ;
! M: standalone-only-literal write-modern-literal drop ;

: write-whitespace ( obj -- )
    last-slice get
    [ slice-between ] [ slice-before ] if*
    >string io:write ;

! Swap in write-modern-literal for renaming
: write-lexed ( lexed/slice -- )
    [ underlying write-whitespace ]
    [ write-modern-literal ]
    [ underlying last-slice namespaces:set ] tri ;

: with-last-slice ( quot -- )
    [ f last-slice ] dip with-variable ; inline

: write-modern-loop ( quot -- )
    [ [ write-lexed ] each nl ] with-last-slice ; inline

: write-modern-string ( seq -- string )
    [ write-modern-loop ] with-string-writer ; inline

: write-modern-path ( seq path -- )
    utf8 [ write-modern-loop ] with-file-writer ; inline

: rewrite-path ( path quot -- )
    ! dup print
    '[ [ path>literals _ map ] [ ] bi write-modern-path ]
    [ drop . ] recover ; inline

: rewrite-string ( string quot -- )
    ! dup print
    [ string>literals ] dip map write-modern-string ; inline

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
    dup { [ token-literal? ] [ payload>> paren-word-name? ] } 1&& [
        [ paren-word>tick-word ] change-payload
    ] when ;

: single-line-comment? ( token -- ? )
    { [ til-eol-literal? ] [ tag>> "!" tail? ] } 1&& ;

: transform-single-line-comment>hash-comment ( token -- token' )
    dup single-line-comment? [
        [ "!" ?tail drop "#" append ] change-tag
    ] when ;

: transform-source ( quot -- )
    lexable-paths swap rewrite-paths ; inline