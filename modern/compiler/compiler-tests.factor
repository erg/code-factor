! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel modern.compiler tools.test ;
IN: modern.compiler.tests

{
    H{ { "scratchpad2" { "a" } } }
} [
    "IN: scratchpad2
    SYMBOL: a"
    quick-compile-string vocab>printable
] unit-test

{
    H{ { "scratchpad2" { "b" "c" } } }
} [
    "IN: scratchpad2
    SYMBOLS: b c ;"
    quick-compile-string vocab>printable
] unit-test

{
    H{ { "scratchpad2" { "add" } } }
} [
    "IN: scratchpad2
    USE: math
    : add ( a b -- c ) + ;"
    quick-compile-string vocab>printable
] unit-test


{
    H{ { "omg" { "a" } } { "lol" { "b" } } }
} [
    "IN: omg
    CONSTANT: a 1
    IN: lol
    CONSTANT: b 2 "
    quick-compile-string vocab>printable
] unit-test


{
    H{ { "omg" { "add-subtract" } } }
} [
    "IN: omg
    : add-subtract ( a b c -- d ) + - ;"
    quick-compile-string vocab>printable
] unit-test