! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators.short-circuit kernel modern
modern.out sequences tools.test ;
IN: modern.out.tests

: rewrite-same-string ( string -- ? )
    [ [ ] rewrite-string ] keep sequence= ;

: rename-backtick-delimiter ( string -- string' )
    [
        dup backtick-literal? [ [ drop "^" ] change-delimiter ] when
    ] rewrite-string ;

: rename-backslash-delimiter ( string -- string' )
    [
        dup backslash-literal? [ [ drop "^" ] change-delimiter ] when
    ] rewrite-string ;

{ t } [ "fixnum`33 ch`@" rewrite-same-string ] unit-test
{ t } [ "! omg" rewrite-same-string ] unit-test
{ t } [ "todo! omg" rewrite-same-string ] unit-test
{ t } [ "foo[ bar{ baz( ) } ]" rewrite-same-string ] unit-test

{ "fixnum^33 ch^@" } [ "fixnum`33 ch`@" rename-backtick-delimiter ] unit-test

{ "^ foo  ^    bar" } [ "\\ foo  \\    bar" rename-backslash-delimiter ] unit-test

{ ": asdf < '< > > ;" } [
    ": asdf [ '[ ] ] ;" [
        dup { [ single-matched-literal? ] [ delimiter>> "[" = ] } 1&& [
        [ drop "<" ] change-delimiter
        ] when
    ] rewrite-string
] unit-test

! lexable-paths [ transform-single-line-comment>hash-comment ] rewrite-paths