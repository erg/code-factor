! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors kernel kernel.private modern.quick-parser
sequences splitting ;
IN: modern.refactor

TUPLE: scoped-word vocab word ;
C: <scoped-word> scoped-word

TUPLE: rename-word old new ;
C: <rename-word> rename-word

: same-vocab? ( scoped-word scoped-word -- ? ) [ vocab>> ] bi@ = ;

GENERIC: parsed>slice ( parsed -- slice )
M: slice parsed>slice ;
M: qsequence parsed>slice slice>> ;

: rename-word-same-vocab ( scoped-word scoped-word -- )
    2drop
    ;

: rename-word ( vocab:word vocab:word -- )
    [ ":" split1 <scoped-word> ] bi@
    2dup same-vocab? [
        rename-word-same-vocab
    ] [
        unimplemented
    ] if ;

