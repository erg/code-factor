! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: continuations fry io io.encodings.utf8 io.files.temp
io.files.unique io.servers io.sockets kayos kernel stream.extras
tools.test ;
IN: kayos.tests

CONSTANT: kayos-test-port 50666

: with-kayos-test-server ( quot -- )
    '[
        "kayos-test" "db" [
            kayos-test-port <kayos-server> start-server _ swap
            '[
                _
                [ _ stop-server ]
                [ ] cleanup
            ] call
        ] cleanup-unique-file
    ] with-temp-directory ; inline

: with-kayos-test-client ( quot -- )
    [ "localhost" kayos-test-port <inet> utf8 ] dip with-client ; inline

: with-kayos-test-server-client ( quot -- )
    '[ _ with-kayos-test-client ] with-kayos-test-server ; inline

{ "1" } [
    [
        "set a 1" print-flush readln drop
        "get a" print-flush readln
    ] with-kayos-test-server-client
] unit-test

{ "2" } [
    [
        "set a 1" print-flush readln drop
        "set a 2" print-flush readln drop
        "get a" print-flush readln
    ] with-kayos-test-server-client
] unit-test

{ "1" } [
    [
        [
            "set a 1" print-flush readln drop
        ] with-kayos-test-client

        [
            "get a" print-flush readln
        ] with-kayos-test-client
    ] with-kayos-test-server
] unit-test
