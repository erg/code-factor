! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators.short-circuit kernel modern
modern.out sequences tools.test multiline ;
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

{ t } [ " ARRAY: ;" rewrite-same-string ] unit-test
{ t } [ " ARRAY: 1 ;{ inline }" rewrite-same-string ] unit-test
{ t } [ " ARRAY: 1 ;[ 0 ]" rewrite-same-string ] unit-test

{ t } [ "   abc{ 1 2 3   abc}" rewrite-same-string ] unit-test
{ t } [ "  ABC: abc{ 1 2 3   abc}  ABC;" rewrite-same-string ] unit-test
{ t } [ " a{   a{   a{  a}   }   a}" rewrite-same-string ] unit-test

! Funky spaced decorator test
{ t } [
    " lol@  {    1   }@    {   2   }@    hi   @{   3    }   @{   4    }  @inline" rewrite-same-string
] unit-test
! Disable these for now.
! { t } [ " array: 1" rewrite-same-string ] unit-test
! { t } [ " {  array:  1  array:  2  }" rewrite-same-string ] unit-test



{ "fixnum^33 ch^@" } [ "fixnum`33 ch`@" rename-backtick-delimiter ] unit-test

{ "^ foo  ^    bar" } [ "\\ foo  \\    bar" rename-backslash-delimiter ] unit-test

/*
{ ": asdf < '< > > ;" } [
    ": asdf [ '[ ] ] ;" [
        dup { [ single-matched-literal? ] [ delimiter>> "[" = ] } 1&&
        [ [ drop "<" ] change-delimiter ] when
    ] rewrite-string
] unit-test
*/

! lexable-paths [ transform-single-line-comment>hash-comment ] rewrite-paths
