! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors io io.encodings.utf8 io.files io.streams.string
kernel modern.slices namespaces sequences ;
IN: modern.out

SYMBOL: last-slice

GENERIC: underlying ( obj -- slice )
M: f underlying ;
M: slice underlying ;
M: object underlying underlying>> ;

: write-whitespace ( obj -- )
    last-slice get
    [ slice-between ] [ slice-before ] if* write ;

: write-lexed ( lexed/slice -- )
    underlying
    [ write-whitespace ]
    [ io:write ]
    [ last-slice set ] tri ;

: with-last-slice ( quot -- )
    [ f last-slice ] dip with-variable ; inline

: write-modern-loop ( quot -- )
    [ [ write-lexed ] each nl ] with-last-slice ; inline

: write-modern-string ( seq -- string )
    [ write-modern-loop ] with-string-writer ; inline

: write-modern-file ( seq path -- )
    utf8 [ write-modern-loop ] with-file-writer ; inline
