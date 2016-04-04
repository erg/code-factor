! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors modern sequences strings tools.test ;
IN: modern.tests

{ 0 } [ "" string>literals length ] unit-test
{ 1 } [ "a" string>literals length ] unit-test
{ 1 } [ " a" string>literals length ] unit-test
{ 1 } [ " a " string>literals length ] unit-test
{ 3 } [ "a b c" string>literals length ] unit-test

{ 1 } [ "`abc" string>literals length ] unit-test
{ 2 } [ "`abc `cba" string>literals length ] unit-test
{ 2 } [ "\"abc\" \"cba\"" string>literals length ] unit-test
{ 2 } [ "[[abc]] [[cba]]" string>literals length ] unit-test
{ 2 } [ "{{abc}} {{cba}}" string>literals length ] unit-test
{ 2 } [ "((abc)) ((cba))" string>literals length ] unit-test
{ 2 } [ "[=[abc]=] [=[cba]=]" string>literals length ] unit-test
{ 2 } [ "{={abc}=} {={cba}=}" string>literals length ] unit-test
{ 2 } [ "(=(abc)=) (=(cba)=)" string>literals length ] unit-test
{ 2 } [ "[==[abc]==] [==[cba]==]" string>literals length ] unit-test
{ 2 } [ "{=={abc}==} {=={cba}==}" string>literals length ] unit-test
{ 2 } [ "(==(abc)==) (==(cba)==)" string>literals length ] unit-test

{ 1 } [ "hex`abc" string>literals length ] unit-test
{ 2 } [ "hex`abc hex`cba" string>literals length ] unit-test
{ 2 } [ "hex\"abc\" hex\"cba\"" string>literals length ] unit-test
{ 2 } [ "hex[[abc]] hex[[cba]]" string>literals length ] unit-test
{ 2 } [ "hex{{abc}} hex{{cba}}" string>literals length ] unit-test
{ 2 } [ "hex((abc)) hex((cba))" string>literals length ] unit-test
{ 2 } [ "hex[=[abc]=] hex[=[cba]=]" string>literals length ] unit-test
{ 2 } [ "hex{={abc}=} hex{={cba}=}" string>literals length ] unit-test
{ 2 } [ "hex(=(abc)=) hex(=(cba)=)" string>literals length ] unit-test
{ 2 } [ "hex[==[abc]==] hex[==[cba]==]" string>literals length ] unit-test
{ 2 } [ "hex{=={abc}==} hex{=={cba}==}" string>literals length ] unit-test
{ 2 } [ "hex(==(abc)==) hex(==(cba)==)" string>literals length ] unit-test

{ 1 } [ "[ ]" string>literals length ] unit-test
{ 1 } [ "abc[ ]" string>literals length ] unit-test
{ 1 } [ "abc[ 1 ]" string>literals length ] unit-test
{ 1 } [ "{ }" string>literals length ] unit-test
{ 1 } [ "abc{ }" string>literals length ] unit-test
{ 1 } [ "abc{ 1 }" string>literals length ] unit-test

{ 1 } [ "( )" string>literals length ] unit-test
{ 1 } [ "abc( )" string>literals length ] unit-test
{ 1 } [ "abc( 1 )" string>literals length ] unit-test

{ 1 } [ "!omg" string>literals length ] unit-test
{ 1 } [ "! omg" string>literals length ] unit-test
{ 1 } [ "![[omg]]" string>literals length ] unit-test
{ 1 } [ "![[
    omg]]" string>literals length
] unit-test

{ 1 } [ "\\ a" string>literals length ] unit-test
{ 1 } [ "\\ \\" string>literals length ] unit-test
{ 1 } [ " \\   abcd " string>literals length ] unit-test

{ "omg" } [ "!omg" string>literals first payload>> >string ] unit-test

! Comment character should be #, and should not be allowed in word names
! For now, we have exclamation as comment character and words
! like suffix! which aren't allowed to start comments
{ 2 } [ "a!omg lol" string>literals length ] unit-test
{ 3 } [ "a! omg lol" string>literals length ] unit-test
{ 2 } [ "a![[omg]] lol" string>literals length ] unit-test

! This is broken.
! hex[[abc]] -> hex#[[abc]] ! commented out hex literal!
! $hex[[abc${0}]]           ! interpolate
! { 2 } [ "a![[
!    omg]] lol" string>literals length
! ] unit-test


! { { "1" "2" "+" } }
! [ "[1 2 +]" string>literals first payload>> [ >string ] map ] unit-test

! { { "1" "2" "+" } }
! [ "{1 2 +}" string>literals first payload>> [ >string ] map ] unit-test

! { { "1" "2" "+" } }
! [ "(1 2 +)" string>literals first payload>> [ >string ] map ] unit-test
