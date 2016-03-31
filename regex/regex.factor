! Copyright (C) 2010 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators
combinators.short-circuit constructors fry interval-maps kernel
locals make math math.parser memoize namespaces regexp.classes
sequences sets splitting strings unicode unicode.script unicode.data ;
IN: regex

TUPLE: character-parser string { n integer initial: 0 } ;
CONSTRUCTOR: <character-parser> character-parser ( string -- obj ) ;

TUPLE: regex-tree
    string
    character-parser
    stack
    character-classes
    { character-class-level integer initial: 0 }
    regex-tree ;
CONSTRUCTOR: <regex-tree> regex-tree ( string -- obj )
    dup string>> <character-parser> >>character-parser
    V{ } clone >>stack
    V{ } clone >>character-classes ;
TUPLE: concatenation seq ;
CONSTRUCTOR: <concatenation> concatenation ( seq -- obj ) ;
TUPLE: alternation seq ;
CONSTRUCTOR: <alternation> alternation ( seq -- obj ) ;
TUPLE: maybe term ;
CONSTRUCTOR: <maybe> maybe ( term -- obj ) ;
TUPLE: plus term ;
CONSTRUCTOR: <plus> plus ( term -- obj ) ;
TUPLE: star term ;
CONSTRUCTOR: <star> star ( term -- obj ) ;
TUPLE: negation term ;
CONSTRUCTOR: <negation> negation ( term -- obj ) ;
TUPLE: tagged-epsilon tag ;
CONSTRUCTOR: <tagged-epsilon> tagged-epsilon ( tag -- obj ) ;
TUPLE: character-range from to ;
CONSTRUCTOR: <character-range> character-range ( from to -- obj ) ;
TUPLE: character ch ;
: <character> ( ch -- ch ) ;
! CONSTRUCTOR: <character> character ( ch -- obj ) ;
TUPLE: character-class classes ;
CONSTRUCTOR: <character-class> character-class ( classes -- obj ) ;
TUPLE: negated-character-class classes ;
CONSTRUCTOR: <negated-character-class> negated-character-class ( classes -- obj ) ;
TUPLE: capture-group term ;
CONSTRUCTOR: <capture-group> capture-group ( term -- obj ) ;
TUPLE: repetition m n term ;
CONSTRUCTOR: <repetition> repetition ( m n term -- obj ) ;
SINGLETONS: dot letter-class LETTER-class Letter-class digit-class
alpha-class non-newline-blank-class
ascii-class punctuation-class java-printable-class blank-class
control-character-class hex-digit-class java-blank-class c-identifier-class
unmatchable-class terminator-class word-boundary-class ;

SINGLETONS: beginning-of-input ^ end-of-input $ end-of-file
^unix $unix word-break ;

: character-parser ( -- obj ) \ regex-tree get character-parser>> ;
: still-parsing? ( ? -- ? )
    {
        [ ]
        [ drop character-parser [ n>> ] [ string>> length ] bi < ]
    } 1&& ;

: current ( -- ch ) character-parser [ n>> ] [ string>> ] bi nth ;
: advance ( -- ) character-parser [ 1 + ] change-n drop ;
: lookahead1 ( -- ch/f )
    character-parser [ n>> 1 + ] [ string>> ] bi 2dup bounds-check? [ nth ] [ 2drop f ] if ;
: lookahead2 ( -- ch/f )
    character-parser [ n>> 2 + ] [ string>> ] bi 2dup bounds-check? [ nth ] [ 2drop f ] if ;

: ?subseq ( from to seq -- subseq/f )
    2dup [ 1 - ] dip bounds-check? [ subseq ] [ nip swap tail f like ] if ;

: ?last ( seq -- elt/f ) [ length 1 - ] [ ?nth ] bi ;

: lookahead ( n -- string/f )
    [ character-parser n>> dup ] dip + character-parser string>> ?subseq ;

: advance-n ( n -- )
    [ character-parser ] dip '[ _ + ] change-n drop ;

ERROR: expected regex-parser string ;

:: expect ( string -- )
    string
    string length lookahead = [
        string length advance-n
    ] [
        character-parser string expected
    ] if ;

: take ( n -- string )
    [ lookahead ] [ advance-n ] bi ;

:: take? ( string -- string )
    string length lookahead string =
    [ string length advance-n t ] [ f ] if ;

: take-while ( quot -- string )
    '[
        [ @ [ [ current , advance ] when ] keep ] loop
    ] "" make ; inline

: take-integer ( -- integer/f )
    [ current digit? ] take-while string>number ;

: take-alpha ( -- integer/f )
    [ current Letter? ] take-while ;

: take-character-class ( -- string )
    "[:" expect [ current letter? ] take-while ":" expect ;

: regex-tree ( -- regex-tree ) \ regex-tree get ;
: stack ( -- seq ) regex-tree stack>> ;
: push-regex-tree ( obj -- ) stack push ;
: pop-regex-tree ( -- obj ) stack pop ;
: current-regex ( -- seq ) stack last ;
: push-current-regex ( obj -- ) current-regex push ;
: pop-current-regex ( -- obj ) current-regex pop ;
: peek1 ( -- obj/f ) current-regex [ length 1 - ] keep ?nth ;
: peek2 ( -- obj/f ) current-regex [ length 2 - ] keep ?nth ;
: unless-alternation ( quot -- )
    [ peek2 alternation = ] dip unless ; inline
: stack1 ( word -- )
    '[ pop-current-regex _ execute( obj -- obj ) push-current-regex ] unless-alternation ;
: stack2 ( word -- )
    '[
        current-regex length 1 > [
            pop-current-regex pop-current-regex swap
            _ execute( obj obj -- obj ) push-current-regex
        ] when
    ] unless-alternation ;
: new-nested-regex ( -- )
    V{ } clone push-regex-tree ;

: character-classes ( -- obj ) regex-tree character-classes>> ;
: pop-character-class ( -- obj ) character-classes pop ;
: push-character-class ( obj -- ) character-classes push ;
: peek-character-class ( -- obj ) character-classes ?last ;
: pop-class ( -- obj ) character-classes last classes>> pop ;
: peek-class ( -- obj ) character-classes last classes>> ?last ;
: push-class ( obj -- ) character-classes last classes>> push ;
: new-character-class ( -- )
    regex-tree [ 1 + ] change-character-class-level drop
    V{ } clone <character-class> push-character-class ;
: negate-character-class ( -- )
    pop-character-class dup negated-character-class?
    [ classes>> <character-class> ] [ classes>> <negated-character-class> ] if
    push-character-class ;
: still-parsing-character-class? ( -- ? )
    regex-tree character-class-level>> 0 > ;

: parse-character-class-first ( -- )
    current {
        { CHAR: ^ [ negate-character-class advance ] }
        [ drop ]
    } case ;

: parse-range-start ( -- )
    current {
        { CHAR: ] [ CHAR: ] <character> push-class advance ] }
        { CHAR: - [ CHAR: - <character> push-class advance ] }
        [ drop ]
    } case ;

ERROR: bad-range from to ;

: proper-range? ( -- ? )
    { [ lookahead1 CHAR: - = ] [ lookahead2 "-]" member? not ] } 0&& ;

TUPLE: union-class seq ;
CONSTRUCTOR: <union-class> union-class ( seq -- obj ) ;
TUPLE: intersection-class seq ;
CONSTRUCTOR: <intersection-class> intersection-class ( seq -- obj ) ;
TUPLE: symmetric-difference-class seq ;
CONSTRUCTOR: <symmetric-difference-class> symmetric-difference-class ( seq -- obj ) ;
TUPLE: difference-class seq ;
CONSTRUCTOR: <difference-class> difference-class ( seq -- obj ) ;

: finish-character-class ( -- )
    character-classes length 1 = [
        pop-character-class
    ] [
        character-classes <union-class>
        V{ } clone regex-tree character-classes<<
    ] if push-current-regex ;

: 2class-stack ( word -- )
    character-classes length 2 >= [
        [ pop-character-class pop-character-class swap 2array ] dip execute( obj -- obj )
        push-character-class
    ] [
        "2class-stack lol" throw
    ] if ;

: nested-character-class? ( -- ? )
    regex-tree character-class-level>> 1 > ;

DEFER: parse-character-class
DEFER: parse-nested-character-class
DEFER: parse-escape
: parse-range-rest ( -- )
    2 lookahead {
        { [ dup "[:" head? ] [ drop take-character-class push-class advance ] }
        { [ dup "[" head? ] [ drop parse-nested-character-class ] }
        { [ dup "]" head? ] [
                drop
                regex-tree [ 1 - ] change-character-class-level drop
                advance ] }
        { [ dup "||" head? ]
            [ drop 2 advance-n parse-nested-character-class \ <union-class> 2class-stack ] }
        { [ dup "&&" head? ]
            [ drop 2 advance-n parse-nested-character-class \ <intersection-class> 2class-stack ] }
        { [ dup "~~" head? ]
            [ drop 2 advance-n parse-nested-character-class \ <symmetric-difference-class> 2class-stack ] }
        { [ dup "--" head? ]
            [ drop 2 advance-n parse-nested-character-class \ <difference-class> 2class-stack ] }
        { [ dup "\\" head? ] [ drop parse-escape push-class ] }
        [
            drop proper-range? [
                current advance advance current
                2dup <= [ bad-range ] unless
                <character-range> push-class advance
            ] [
                current <character> push-class advance
            ] if
        ]
    } cond ;

: parse-nested-character-class ( -- )
    new-character-class
    "[" expect
    parse-character-class-first
    parse-range-start
    [ parse-range-rest still-parsing-character-class? ] loop ;

: parse-character-class ( -- )
    parse-nested-character-class
    finish-character-class ;

ERROR: invalid-repetition ;

: parse-repetition ( -- )
    advance
    take-integer
    current {
        { CHAR: , [
            advance
            take-integer
            current CHAR: } = [ invalid-repetition ] unless
            advance
        ] }
        { CHAR: } [ dup advance ] }
    } case
    pop-current-regex <repetition> push-current-regex ;

: finalize ( stack -- obj )
    { alternation } split
    [ dup length 1 = [ first ] [ <concatenation> ] if ] map
    dup length 1 = [ first ] [ <alternation> ] if ;

DEFER: parse-char

: finish-nested-regex ( -- )
    pop-regex-tree finalize
    stack empty? [
        push-regex-tree
    ] [
        push-current-regex
    ] if ;

: parse-new-regex ( -- )
    new-nested-regex
    [ current parse-char still-parsing? ] loop ;

! (?P<name>group)
! (?<first>group)(?'second'group
! a(?'digit'[0-5])|b(?'digit'[4-7])
! (?:nocapture)

! (?!.) negative lookahead noncapturing can use capture group inside
! (?=..) lookahead noncapturing
! (?<...) lookbehind
! (?<!...) lookbehind
! (?=(capturing))
! lookaround is atomic

! (?>group) atomic

! (?ifthen|else)
! (?(?=regex)then|else)
! (?(?=condition)(then1|then2|then3)|(else1|else2|else3))
! (?<test>a)?b(?(test)c|d).
! (?=regex)then|(?!regex)else)

! (?idmsux-idmsux:X) atomic group with flags on/off

TUPLE: options string ;
CONSTRUCTOR: <options> options ( string -- obj ) ;
TUPLE: negated-options string ;
CONSTRUCTOR: <negated-options> negated-options ( string -- obj ) ;

TUPLE: positive-lookbehind term ;
CONSTRUCTOR: <positive-lookbehind> positive-lookbehind ( term -- obj ) ;
TUPLE: negative-lookbehind term ;
CONSTRUCTOR: <negative-lookbehind> negative-lookbehind ( term -- obj ) ;

TUPLE: positive-lookahead term ;
CONSTRUCTOR: <positive-lookahead> positive-lookahead ( term -- obj ) ;
TUPLE: negative-lookahead term ;
CONSTRUCTOR: <negative-lookahead> negative-lookahead ( term -- obj ) ;

TUPLE: named-group term ;
CONSTRUCTOR: <named-group> named-group ( term -- obj ) ;

TUPLE: noncapturing term ;
CONSTRUCTOR: <noncapturing> noncapturing ( term -- obj ) ;
TUPLE: atomic term ;
CONSTRUCTOR: <atomic> atomic ( term -- obj ) ;
TUPLE: capture term ;
CONSTRUCTOR: <capture> capture ( term -- obj ) ;

TUPLE: comment string ;
CONSTRUCTOR: <comment> comment ( string -- obj ) ;

: parse-loop ( -- )
    [ current parse-char still-parsing? ] loop ;

: parse-nested-regex ( -- )
    new-nested-regex
    3 lookahead {
        { [ dup "?<=" head? ] [ drop 3 advance-n parse-loop \ <positive-lookbehind> stack1 ] } ! positive lookbehind
        { [ dup "?<!" head? ] [ drop 3 advance-n parse-loop \ <negative-lookbehind> stack1 ] } ! negative lookbehind
        { [ dup "?<" head? ] [ drop 2 advance-n parse-loop \ <named-group> stack1 ] } ! named group
        { [ dup "?=" head? ] [ drop 2 advance-n parse-loop \ <positive-lookahead> stack1 ] } ! lookahead
        { [ dup "?!" head? ] [ drop 2 advance-n parse-loop \ <negative-lookahead> stack1 ] } ! negative lookahead
        ! { [ dup "?~" head? ] [ drop 2 advance-n parse-loop ] } ! negation of parentheses group
        { [ dup "?#" head? ] [ drop 2 advance-n [ current CHAR: ) = not ] take-while <comment> push-current-regex ] } ! comment
        { [ dup "?:" head? ] [ drop 2 advance-n parse-loop \ <noncapturing> stack1 ] } ! noncapturing
        { [ dup "?>" head? ] [ drop 2 advance-n parse-loop \ <atomic> stack1 ] } ! atomic options (independent/non-capture)
        { [ dup "?-" head? ] [ drop 2 advance-n parse-loop \ <negated-options> stack1 ] } ! minus options
        { [ dup "?" head? ] [ drop 1 advance-n parse-loop \ <options> stack1 ] } ! options
        [ drop ]
    } cond ;

ERROR: extra-right-parenthesis regex ;

ERROR: unbalanced-regex regex ;

: check-balance ( -- )
    stack length 1 > [ regex-tree extra-right-parenthesis ] unless ;

:: at-error ( key assoc quot: ( key -- replacement ) -- value )
    key assoc at* [ drop key quot call ] unless ; inline

ERROR: bad-class name ;

: simple ( str -- simple )
    ! Alternatively, first collation key level?
    >case-fold [ " \t_" member? not ] filter ;

: simple-table ( seq -- table )
    [ [ simple ] keep ] H{ } map>assoc ;

MEMO: simple-script-table ( -- table )
    script-table interval-values members simple-table ;

MEMO: simple-category-table ( -- table )
    categories simple-table ;

: parse-unicode-class ( name -- class )
    {
        { [ dup { [ length 1 = ] [ first "clmnpsz" member? ] } 1&& ] [
            >upper first
            <category-range-class>
        ] }
        { [ dup >title categories member? ] [
            simple-category-table at <category-class>
        ] }
        { [ "script=" ?head ] [
            dup simple-script-table at
            [ <script-class> ]
            [ "script=" prepend bad-class ] ?if
        ] }
        [ bad-class ]
    } cond ;

: unicode-class ( name -- class )
    dup parse-unicode-class [ ] [ bad-class ] ?if ;

: name>class ( name -- class )
    >string simple {
        { "lower" letter-class }
        { "upper" LETTER-class }
        { "alpha" Letter-class }
        { "ascii" ascii-class }
        { "digit" digit-class }
        { "alnum" alpha-class }
        { "punct" punctuation-class }
        { "graph" java-printable-class }
        { "blank" non-newline-blank-class }
        { "cntrl" control-character-class }
        { "xdigit" hex-digit-class }
        { "space" java-blank-class }
    } [ unicode-class ] at-error ;

: lookup-escape ( char -- ast )
    {
        { CHAR: t [ CHAR: \t <character> ] }
        { CHAR: n [ CHAR: \n <character> ] }
        { CHAR: r [ CHAR: \r <character> ] }
        { CHAR: f [ 0xc <character> ] }
        { CHAR: a [ 0x7 <character> ] }
        { CHAR: e [ 0x1b <character> ] }
        { CHAR: \\ [ CHAR: \\ <character> ] }

        { CHAR: w [ c-identifier-class <primitive-class> ] }
        { CHAR: W [ c-identifier-class <primitive-class> <not-class> ] }
        { CHAR: s [ java-blank-class <primitive-class> ] }
        { CHAR: S [ java-blank-class <primitive-class> <not-class> ] }
        { CHAR: d [ digit-class <primitive-class> ] }
        { CHAR: D [ digit-class <primitive-class> <not-class> ] }

        { CHAR: z [ end-of-input <tagged-epsilon> ] }
        { CHAR: Z [ end-of-file <tagged-epsilon> ] }
        { CHAR: A [ beginning-of-input <tagged-epsilon> ] }
        { CHAR: b [ word-break <tagged-epsilon> ] }
        { CHAR: B [ word-break <not-class> <tagged-epsilon> ] }
        [ <character> ]
    } case ;

ERROR: bad-number ;

: ensure-number ( n -- n ) [ bad-number ] unless* ;

: parse-escape ( -- obj )
    advance
    2 lookahead {
        { "p{" [ 2 advance-n [ current CHAR: } = not ] take-while name>class <primitive-class> advance t ] }
        { "P{" [ 2 advance-n [ current CHAR: } = not ] take-while name>class <primitive-class> <not-class> advance t ] }
        [ f ]
    } case
    [
        drop
        current {
            { CHAR: Q [ advance [ 2 lookahead "\\E" = not ] take-while <concatenation> 2 advance-n ] }
            { CHAR: u [ advance 4 take hex> ensure-number <character> ] }
            { CHAR: x [ advance 2 take hex> ensure-number <character> ] }
            { CHAR: 0 [ advance 3 take oct> ensure-number <character> ] }
            [ lookup-escape advance ]
        } case
    ] unless ;

! # is a comment in free-space mode
: parse-char ( ch -- ? )
    {
        { CHAR: . [ dot push-current-regex t advance ] }
        { CHAR: \ [ parse-escape push-current-regex t ] }
        { CHAR: ? [ \ <maybe> stack1 t advance ] }
        { CHAR: * [ \ <star> stack1 t advance ] }
        { CHAR: + [ \ <plus> stack1 t advance ] }
        { CHAR: | [ \ alternation push-current-regex t advance ] }
        { CHAR: [ [ parse-character-class t ] }
        { CHAR: ( [ advance parse-nested-regex t ] }
        { CHAR: ) [ check-balance finish-nested-regex f advance ] }
        { CHAR: { [ parse-repetition t ] }
        { CHAR: ^ [ ^ <tagged-epsilon> push-current-regex t advance ] }
        { CHAR: $ [ $ <tagged-epsilon> push-current-regex t advance ] }
        [ <character> push-current-regex t advance ]
    } case ;

: parse-regex ( string -- tree )
    <regex-tree> \ regex-tree [
        parse-new-regex
        pop-regex-tree
        stack empty? [ regex-tree unbalanced-regex ] unless
        finalize regex-tree regex-tree<<
        regex-tree
    ] with-variable ;

: or-regex-trees ( seq -- seq' )
    [ stack>> first ] map <alternation> ;

! dup rules>> [ user-agents>> V{ "*" } = ] filter
! first allows>> [ present parse-string ] map or-regex-trees

