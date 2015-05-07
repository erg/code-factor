! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators.short-circuit constructors fry
kernel locals make math sequences ;
IN: character-parser

TUPLE: character-parser string { n integer initial: 0 } ;
CONSTRUCTOR: <character-parser> character-parser ( string -- obj ) ;

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

ERROR: expected regexp-parser string ;

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

