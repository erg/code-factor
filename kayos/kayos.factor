! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators combinators.short-circuit
forestdb.lib io io.encodings.utf8 io.servers kernel namespaces
prettyprint sequences splitting stream.extras unicode ;
IN: kayos

TUPLE: kayos-server < threaded-server path topic ;
: <kayos-server> ( path insecure -- obj )
    utf8 kayos-server new-threaded-server
        swap internet-server >>insecure
        swap >>path
        "default" >>topic ; inline

: trim-whitespace ( string -- string' )
    [ blank? ] trim ; inline

: get-current-topic ( -- string )
    threaded-server get topic>> ;

: set-current-topic ( string -- )
    threaded-server get topic<< ;

: valid-topic? ( string -- ? )
    [ { [ Letter? ] [ digit? ] [ "-" member? ] } 1|| ] all? ;

: ok ( string -- )
    "ok: " prepend print-flush ;

: error ( string -- )
    "error: " prepend print-flush ;

: handle-topic ( seq -- )
    trim-whitespace
    dup valid-topic? [
        set-current-topic "topic" ok
    ] [
        "invalid topic: " prepend error
    ] if ;

: handle-get ( seq -- )
    trim-whitespace
    fdb-get-kv print-flush ;

: handle-set ( seq -- )
    trim-whitespace
    " " split1 [ trim-whitespace ] bi@
    fdb-set-kv fdb-commit-normal "set" ok ;

: handle-set2 ( seq -- )
    trim-whitespace
    " " split1 [ trim-whitespace ] bi@
    get-current-topic [
        [ fdb-set-kv ] with-kvs "set" ok
    ] [
        2drop "no topic" error
    ] if* ;

: handle-iterate ( seq -- )
    . flush
    ;

M: kayos-server handle-client* ( server -- )
    [ path>> ] [ topic>> ] bi [
        [
            readln trim-whitespace " " split1 swap >lower {
                ! { [ dup "topic" sequence= ] [ drop handle-topic t ] }
                { [ dup "get" sequence= ] [ drop handle-get t ] }
                { [ dup "set" sequence= ] [ drop handle-set t ] }
                { [ dup "iterate" sequence= ] [ drop handle-iterate t ] }
                { [ dup "quit" sequence= ] [ 2drop f ] }
                [ "unknown command: " prepend print-flush drop t ]
            } cond
        ] loop
    ] with-forestdb-kvs ;
