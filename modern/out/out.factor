! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors io io.encodings.utf8 io.files io.streams.string
kernel namespaces sequences sequences.extras ;
IN: modern.out

SYMBOL: last-slice

: slice-between ( slice1 slice2 -- slice )
    ensure-same-underlying
    slice-order-by-from
    [ to>> ]
    [ [ from>> ] [ seq>> ] bi ] bi* <slice> ;

: slice-before ( slice -- slice' )
    [ drop 0 ] [ from>> ] [ seq>> ] tri <slice> ;

: write-whitespace ( obj -- )
    last-slice get [
        slice-between
    ] [
        slice-before
    ] if* write ;

GENERIC: underlying ( obj -- seq )
M: slice underlying seq>> ;
M: object underlying underlying>> ;

: write-underlying ( slice -- )
    [ write-whitespace ]
    [ io:write ]
    [ dup underlying [ drop ] [ throw ] if* last-slice set ] tri ;

GENERIC: write-modern ( obj -- )
M: slice write-modern write-underlying ;
M: object write-modern underlying>> write-underlying ;

: with-last-slice ( quot -- )
    [ f last-slice ] dip with-variable ; inline

: write-modern-loop ( quot -- )
    [ [ write-modern ] each nl ] with-last-slice ; inline

: write-modern-string ( seq -- string )
    [ write-modern-loop ] with-string-writer ; inline

: write-modern-file ( seq path -- )
    utf8 [ write-modern-loop ] with-file-writer ; inline
