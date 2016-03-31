! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs kernel modern.compiler
modern.quick-parser modern.vocabs multiline sequences strings
tools.test ;
IN: modern.quick-parser.tests

{ t } [ "(a)" qparse length 1 = ] unit-test
{ t } [ "((a))" qparse length 1 = ] unit-test
{ t } [ "( a )" qparse length 1 = ] unit-test
{ t } [ "( )" qparse length 1 = ] unit-test
{ t } [ "( a )" qparse length 1 = ] unit-test
{ t } [ "foo( a )" qparse length 1 = ] unit-test
{ t } [ "foo(a" qparse length 1 = ] unit-test
[ "(" qparse ] must-fail
[ "foo(" qparse ] must-fail


{ t } [ "{a}" qparse length 1 = ] unit-test
{ t } [ "{ }" qparse length 1 = ] unit-test
{ t } [ "{ a }" qparse length 1 = ] unit-test
{ t } [ "foo{ a }" qparse length 1 = ] unit-test
{ t } [ "foo{a" qparse length 1 = ] unit-test

{ t } [ "{{}}" qparse length 1 = ] unit-test
{ t } [ "a{{}}" qparse length 1 = ] unit-test
{ t } [ "a{{b}}" qparse length 1 = ] unit-test


[ "{" qparse ] must-fail
[ "foo{" qparse ] must-fail

{ t } [ "[a]" qparse length 1 = ] unit-test
{ t } [ "[ ]" qparse length 1 = ] unit-test
{ t } [ "[ a ]" qparse length 1 = ] unit-test
{ t } [ "foo[ a ]" qparse length 1 = ] unit-test
{ t } [ "foo[a" qparse length 1 = ] unit-test

{ t } [ "[[]]" qparse length 1 = ] unit-test
{ t } [ "a[[]]" qparse length 1 = ] unit-test
{ t } [ "a[[b]]" qparse length 1 = ] unit-test

{ t } [ "\"\"" qparse length 1 = ] unit-test
{ t } [ "foo\"\"" qparse length 1 = ] unit-test
{ t } [ "foo\"abc\"" qparse length 1 = ] unit-test

{ t } [ "''" qparse length 1 = ] unit-test
{ t } [ "'a'" qparse length 1 = ] unit-test
{ t } [ "'\a'" qparse length 1 = ] unit-test
{ t } [ "'\aasdf'" qparse length 1 = ] unit-test

{ "url" } [
    "url\"https://google.com\"" qparse first opening>> >string
] unit-test

{ "https://google.com" } [
    "url\"https://google.com\"" qparse first object>> >string
] unit-test


{ "url" } [
    "url[[https://google.com]]" qparse first opening>> >string
] unit-test

{ "https://google.com" } [
    "url[[https://google.com]]" qparse first object>> >string
] unit-test


{ "url" } [
    "url`https://google.com" qparse first opening>> >string
] unit-test

{ "https://google.com" } [
    "url`https://google.com" qparse first object>> >string
] unit-test


{ "" } [
    "[[https://google.com]]" qparse first opening>> >string
] unit-test

{ "https://google.com" } [
    "[[https://google.com]]" qparse first object>> >string
] unit-test



{ "module" } [
    "module[[IN: vocab0
    : foo ( -- a ) 1 ;]]"
    qparse first opening>> >string
] unit-test

{ "module" } [
    "  module[==[IN: vocab0
    : foo ( -- a ) 1 ;]==]"
    qparse first opening>> >string
] unit-test

{ "data" } [ "data{{1 2 3}}" qparse first opening>> >string ] unit-test
{ "data" } [ " data{{1 2 3}}" qparse first opening>> >string ] unit-test
{ "data" } [ " data{ 1 2 3 }" qparse first opening>> >string ] unit-test
{ "data" } [ " data[ 1 2 3 ]" qparse first opening>> >string ] unit-test

{ "module" } [
    "  module[==[IN: vocab0
    : foo ( -- a ) 1 ;]==]"
    qparse first opening>> >string
] unit-test


/*
{ } [
"IN: scratchpad

CONSTANT: vocab0 vocab[[
    IN: vocab0
    CONSTANT: a 0
]]

CONSTANT: vocab1 vocab[[
    IN: vocab1
    CONSTANT: a 1
]]

CONSTANT: vocab2 vocab[[
    IN: vocab2
    CONSTANT: a 2
]]

CONSTANT: vocab3 vocab[[
    IN: vocab3
    CONSTANT: a 3
]]
" quick-compile-string drop
] unit-test
*/

{
    { "vocab0" "vocab1" "vocab2" "vocab3" }
} [
"IN: vocab0
CONSTANT: a 0

IN: vocab1
CONSTANT: b 1" quick-compile-string

"IN: vocab2
CONSTANT: c 2

IN: vocab3
CONSTANT: d 3" quick-compile-string

2array join-vocabs keys
] unit-test