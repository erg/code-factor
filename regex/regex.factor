! Copyright (C) 2010 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs character-parser combinators
combinators.short-circuit constructors fry hashtables
interval-maps kernel locals make math math.parser memoize
namespaces regexp.classes sequences sets splitting strings
unicode unicode.data unicode.script ;
IN: regex

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

: regex-tree ( -- regex-tree ) \ regex-tree get ;
: stack ( -- seq ) regex-tree stack>> ;
: push-regex-tree ( obj -- ) stack push ;
: pop-regex-tree ( -- obj ) stack pop ;
: current-regex ( -- seq ) stack last ;
: push-current-regex ( obj -- ) current-regex push ;
: pop-current-regex ( -- obj ) current-regex pop ;
: peek1 ( -- obj/f ) current-regex [ length 1 - ] keep ?nth ;
: peek2 ( -- obj/f ) current-regex [ length 2 - ] keep ?nth ;
: stack1 ( word -- )
    [ pop-current-regex ] dip execute( obj -- obj ) push-current-regex ;
: stack2 ( word -- )
    [ current-regex length 1 > ] dip '[
        pop-current-regex pop-current-regex swap
        _ execute( obj obj -- obj ) push-current-regex
    ] when ;

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
        { char: ^ [ negate-character-class advance ] }
        [ drop ]
    } case ;

: parse-range-start ( -- )
    current {
        { char: ] [ char: ] <character> push-class advance ] }
        { char: - [ char: - <character> push-class advance ] }
        [ drop ]
    } case ;

ERROR: bad-range from to ;

: proper-range? ( -- ? )
    { [ lookahead1 char: - = ] [ lookahead2 "-]" member? not ] } 0&& ;

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
        { char: , [
            advance
            take-integer
            current char: } = [ invalid-repetition ] unless
            advance
        ] }
        { char: } [ dup advance ] }
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
        { [ dup "?#" head? ] [ drop 2 advance-n [ current char: ) = not ] take-while <comment> push-current-regex ] } ! comment
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
        { char: t [ char: \t <character> ] }
        { char: n [ char: \n <character> ] }
        { char: r [ char: \r <character> ] }
        { char: f [ 0xc <character> ] }
        { char: a [ 0x7 <character> ] }
        { char: e [ 0x1b <character> ] }
        { char: \\ [ char: \\ <character> ] }

        { char: w [ c-identifier-class <primitive-class> ] }
        { char: W [ c-identifier-class <primitive-class> <not-class> ] }
        { char: s [ java-blank-class <primitive-class> ] }
        { char: S [ java-blank-class <primitive-class> <not-class> ] }
        { char: d [ digit-class <primitive-class> ] }
        { char: D [ digit-class <primitive-class> <not-class> ] }

        { char: z [ end-of-input <tagged-epsilon> ] }
        { char: Z [ end-of-file <tagged-epsilon> ] }
        { char: A [ beginning-of-input <tagged-epsilon> ] }
        { char: b [ word-break <tagged-epsilon> ] }
        { char: B [ word-break <not-class> <tagged-epsilon> ] }
        [ <character> ]
    } case ;

ERROR: bad-number ;

: ensure-number ( n -- n ) [ bad-number ] unless* ;

: parse-escape ( -- obj )
    advance
    2 lookahead {
        { "p{" [ 2 advance-n [ current char: } = not ] take-while name>class <primitive-class> advance t ] }
        { "P{" [ 2 advance-n [ current char: } = not ] take-while name>class <primitive-class> <not-class> advance t ] }
        [ f ]
    } case
    [
        drop
        current {
            { char: Q [ advance [ 2 lookahead "\\E" = not ] take-while <concatenation> 2 advance-n ] }
            { char: u [ advance 4 take hex> ensure-number <character> ] }
            { char: x [ advance 2 take hex> ensure-number <character> ] }
            { char: 0 [ advance 3 take oct> ensure-number <character> ] }
            [ lookup-escape advance ]
        } case
    ] unless ;

! # is a comment in free-space mode
: parse-char ( ch -- )
    {
        { char: . [ dot push-current-regex advance ] }
        { char: \ [ parse-escape push-current-regex ] }
        { char: ? [ \ <maybe> stack1 advance ] }
        { char: * [ \ <star> stack1 advance ] }
        { char: + [ \ <plus> stack1 advance ] }
        { char: | [ \ alternation push-current-regex advance ] }
        { char: [ [ parse-character-class ] }
        { char: ( [ advance parse-nested-regex ] }
        { char: ) [ check-balance finish-nested-regex advance ] }
        { char: { [ parse-repetition ] }
        { char: ^ [ ^ <tagged-epsilon> push-current-regex advance ] }
        { char: $ [ $ <tagged-epsilon> push-current-regex advance ] }
        [ <character> push-current-regex advance ]
    } case ;

: parse-regex ( string -- tree )
    <regex-tree>
    [ \ regex-tree associate ]
    [ character-parser>> \ character-parser pick set-at ] bi
    [
        parse-new-regex
        pop-regex-tree
        stack empty? [ regex-tree unbalanced-regex ] unless
        finalize regex-tree regex-tree<<
        regex-tree
    ] with-variables ;

: or-regex-trees ( seq -- seq' )
    [ stack>> first ] map <alternation> ;

! dup rules>> [ user-agents>> V{ "*" } = ] filter
! first allows>> [ present parse-string ] map or-regex-trees

