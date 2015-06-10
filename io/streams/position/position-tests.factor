! Copyright (C) 2014 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: io io.streams.position io.streams.string kernel
tools.test ;
IN: io.streams.position.tests

[ 3 ]
[ "abcdefg" [ input>position-stream 3 read drop tell-input ] with-string-reader ] unit-test

[ 8 ]
[ "abcdefg\n" [ input>position-stream readln drop tell-input ] with-string-reader ] unit-test

[ 11 ]
[ "abcdefg\nhijklmno" [ input>position-stream readln drop 3 read drop tell-input ] with-string-reader ] unit-test