! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors kernel modern modern.out sequences tools.test ;
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
{ "fixnum^33 ch^@" } [ "fixnum`33 ch`@" rename-backtick-delimiter ] unit-test


{ "^ foo  ^    bar" } [ "\\ foo  \\    bar" rename-backslash-delimiter ] unit-test