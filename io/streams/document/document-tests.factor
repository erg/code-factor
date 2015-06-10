! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: io io.streams.document io.streams.duplex kernel
io.streams.string tools.test ;
IN: io.streams.document.tests

[ T{ document-object { object 97 } { position T{ document-position f 0 0 } } } ]
[
    "asdf" <string-reader> <document-stream> [
        read1
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object 115 }
        { position T{ document-position { column 1 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        read1 drop
        read1
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "asdf" }
        { position T{ document-position { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa" }
        { position T{ document-position { line 1 } { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln drop
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object f }
        { position T{ document-position { line 2 } { column 0 } } }
    }
] [
    "asdf\nfdsa" <string-reader> <document-stream> [
        readln drop
        readln drop
        readln
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa\n1234\n5678\n" }
        { position T{ document-position { line 1 } { column 0 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        readln drop
        contents
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object "\nfds" }
        { position T{ document-position { line 0 } { column 4 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        4 read drop
        4 read
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "a" }
        { position T{ document-position { line 0 } { column 0 } } }
    }
    T{ document-object
        { position T{ document-position { column 1 } } }
        { object CHAR: s }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until
    ] with-input-stream
] unit-test


[
    T{ document-object
        { object "d" }
        { position T{ document-position { line 0 } { column 2 } } }
    }
    T{ document-object
        { object CHAR: f }
        { position T{ document-position { line 0 } { column 3 } } }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "f" read-until
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "df" }
        { position T{ document-position { line 0 } { column 2 } } }
    }
    T{ document-object
        { position T{ document-position { column 4 } } }
        { object CHAR: \n }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "\n" read-until
    ] with-input-stream
] unit-test

[
    T{ document-object
        { object "fdsa\n" }
        { position T{ document-position { line 1 } { column 0 } } }
    }
    T{ document-object
        { position T{ document-position { line 2 } } }
        { object CHAR: 1 }
    }
] [
    "asdf\nfdsa\n1234\n5678\n" <string-reader> <document-stream> [
        "sd" read-until 2drop
        "\n" read-until 2drop
        "1" read-until
    ] with-input-stream
] unit-test

[ f f ] [
"" [ input>document-stream "j" read-until ] with-string-reader
] unit-test

[ T{ document-object { object "asdf" } } f ] [
"asdf" [ input>document-stream "j" read-until ] with-string-reader
] unit-test