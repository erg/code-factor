! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors kernel regex tools.test ;
IN: regex.tests

: regex-expect ( string quot -- ? )
    [ parse-regex regex-tree>> ] prepose call ; inline 

{ t } [ "abc" [ concatenation? ] regex-expect ] unit-test
{ t } [ "a|b|c" [ alternation? ] regex-expect ] unit-test
{ t } [ "[abc]" [ character-class? ] regex-expect ] unit-test
