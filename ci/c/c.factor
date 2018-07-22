! Copyright (C) 2018 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: checksums checksums.sha combinators factsh io.backend
io.pathnames kernel make math math.parser modern modern.slices
multiline namespaces sequences sequences.extras splitting ;
IN: ci.c

<PRIVATE
! Crazy parsing, ugh. Example with newline in path: ``factor\nmaster``
! there is a newline after the last ``strings.h`` as well
![[
/dev/null: /home/erg/factor
master/vm/ffi_test.c \
  /home/erg/factor
master/vm/ffi_test.h \
  /usr/lib/clang/6.0.1/include/stdbool.h /usr/include/assert.h \
  /usr/include/bits/types/__locale_t.h /usr/include/strings.h

]]
: read-filename ( n string -- n' string )
    over [
        { CHAR: \\ CHAR: \s } slice-til-separator-inclusive {
            { f [ drop ] }
            { CHAR: \s [ drop ] }
            { CHAR: \\ [ drop next-char-from drop read-filename ] }
        } case
    ] [
        string-expected-got-eof
    ] if ;

: (read-filenames) ( string -- string' )
    "\\\n  " ?head drop
    0 swap read-filename swap dup [ cut ] [ drop "" ] if
    [ but-last , ] dip dup empty? [ (read-filenames) ] unless ;

: read-filenames ( string -- paths )
    [ (read-filenames) drop ] { } make ;

PRIVATE>

: parse-deps ( string -- paths )
    ":" split1 nip
    trim-head-blanks
    read-filenames ;

: clang-deps ( path.c -- paths )
    [ { "clang" "-c" "-o" "/dev/null" "-MMD" "-MF" "/dev/stdout" } ] dip
    normalize-path suffix run-utf8-process>string parse-deps ;

: clang-all-deps ( path.c -- paths )
    [ { "clang" "-c" "-o" "/dev/null" "-MD" "-MF" "/dev/stdout" } ] dip
    normalize-path suffix run-utf8-process>string parse-deps ;

: clang++-deps ( path.c -- paths )
    [ { "clang++" "-c" "-std=c++11" "-o" "/dev/null" "-MMD" "-MF" "/dev/stdout" } ] dip
    normalize-path suffix run-utf8-process>string parse-deps ;

: clang++-all-deps ( path.c -- paths )
    [ { "clang++" "-c" "-std=c++11" "-o" "/dev/null" "-MD" "-MF" "/dev/stdout" } ] dip
    normalize-path suffix run-utf8-process>string parse-deps ;

: resource-paths ( paths -- paths' ) [ resource-path ] map ;
: current-directory-paths ( paths -- paths' ) [ current-directory get prepend-path ] map ;

: cpp-file-deps ( paths -- assoc )
    [ clang++-deps ] map-zip ;
    
: file-checksums ( paths -- assoc )
    [ sha1 checksum-file bytes>hex-string ] map-zip ;
