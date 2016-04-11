! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors kernel modern.quick-parser modern.syntax
multiline sequences tools.test ;
IN: modern.syntax.tests

/*
{ t } [ "( )" qparse length 1 = ] unit-test
{ t } [ "( a )" qparse length 1 = ] unit-test
{ t } [ "( a b c -- d )" qparse length 1 = ] unit-test

{ t } [ "{ }" qparse length 1 = ] unit-test
{ t } [ "{ a }" qparse length 1 = ] unit-test
{ t } [ "{ a b }" qparse length 1 = ] unit-test
{ t } [ "a{ }" qparse length 1 = ] unit-test
{ t } [ "a{ a }" qparse length 1 = ] unit-test
{ t } [ "a{ a b }" qparse length 1 = ] unit-test

{ t } [ "[ ]" qparse length 1 = ] unit-test
{ t } [ "[ a ]" qparse length 1 = ] unit-test
{ t } [ "[ a b ]" qparse length 1 = ] unit-test
{ t } [ "q[ ]" qparse length 1 = ] unit-test
{ t } [ "q[ a ]" qparse length 1 = ] unit-test
{ t } [ "q[ a b ]" qparse length 1 = ] unit-test

{ t } [ "q[[]]" qparse length 1 = ] unit-test
{ t } [ "q[[a]]" qparse length 1 = ] unit-test
{ t } [ "q[=[a]=]" qparse length 1 = ] unit-test
{ t } [ "q[==[a]==]" qparse length 1 = ] unit-test
{ t } [ "q[====[a]====]" qparse length 1 = ] unit-test

{ t } [ "q{{}}" qparse length 1 = ] unit-test
{ t } [ "q{{a}}" qparse length 1 = ] unit-test
{ t } [ "q{={a}=}" qparse length 1 = ] unit-test
{ t } [ "q{=={a}==}" qparse length 1 = ] unit-test
{ t } [ "q{===={a}====}" qparse length 1 = ] unit-test

! Mismatched but ok
{ t } [ "q[{a]]" qparse length 1 = ] unit-test
{ t } [ "q{[a]]" qparse length 1 = ] unit-test
{ t } [ "q{[a}]" qparse length 1 = ] unit-test

[ "q[[a]=]" qparse ] must-fail
[ "q[=[a]]" qparse ] must-fail
[ "q[==[a]]" qparse ] must-fail
[ "q[[a]==]" qparse ] must-fail
[ "q[==[a]===]" qparse ] must-fail
[ "q[===[a]==]" qparse ] must-fail

[ "q{{a}=}" qparse ] must-fail
[ "q{={a}}" qparse ] must-fail
[ "q{=={a}}" qparse ] must-fail
[ "q{{a}==}" qparse ] must-fail
[ "q{=={a}===}" qparse ] must-fail
[ "q{==={a}==}" qparse ] must-fail

! Mismatched
[ "q{{a}]" qparse ] must-fail
[ "q{{a}]" qparse ] must-fail
[ "q{{a]}" qparse ] must-fail

! Exclamation words
{ } [
    ": suffix! ( a b -- c )
          ; inline" qparse drop
] unit-test

! Incomplete word definitions
[ ": suffix ;" qparse ] must-fail

! Incomplete parse
[ "CONSTANT: a (" qparse ] must-fail
[ "CONSTANT: a {" qparse ] must-fail
[ "CONSTANT: a [" qparse ] must-fail
[ "CONSTANT: a \"" qparse ] must-fail
! Regression
[ "CONSTANT: a ( " qparse ] must-fail
[ "CONSTANT: a { " qparse ] must-fail
[ "CONSTANT: a [ " qparse ] must-fail
[ "CONSTANT: a \" " qparse ] must-fail

! Malformed comments, yes malformed comments..
[ "![" qparse ] must-fail
[ "![=" qparse ] must-fail
[ "![=[" qparse ] must-fail
[ "![=[]" qparse ] must-fail
[ "![=[]=" qparse ] must-fail
[ "![=[]=[" qparse ] must-fail

[ "![]]" qparse ] must-fail
[ "![=]=]" qparse ] must-fail
[ "![==]==]" qparse ] must-fail

[ "![]]" qparse ] must-fail
[ "![=[]]" qparse ] must-fail
[ "![==[]=]" qparse ] must-fail
[ "![[]" qparse ] must-fail
[ "![=[=]" qparse ] must-fail
[ "![==[==]" qparse ] must-fail
[ "![[]" qparse ] must-fail
[ "![=[]=" qparse ] must-fail
[ "![==[]==" qparse ] must-fail

! OK comments
{ { } } [ "!" qparse ] unit-test
{ { } } [ "! " qparse ] unit-test
{ { } } [ "!!!!!!" qparse ] unit-test
{ { } } [ "! ! ! ! ! !" qparse ] unit-test
{ { } } [ "!a" qparse ] unit-test
{ { } } [ "!abc" qparse ] unit-test
{ { } } [ "! a" qparse ] unit-test
{ { } } [ "! abc" qparse ] unit-test

{ { } } [ "#!" qparse ] unit-test
{ { } } [ "#! " qparse ] unit-test
{ { } } [ "#!!!!!!" qparse ] unit-test
{ { } } [ "#! ! ! ! ! !" qparse ] unit-test
{ { } } [ "#!a" qparse ] unit-test
{ { } } [ "#!abc" qparse ] unit-test
{ { } } [ "#! a" qparse ] unit-test
{ { } } [ "#! abc" qparse ] unit-test

{ { } } [ "![[]]" qparse ] unit-test
{ { } } [ "![=[]=]" qparse ] unit-test
{ { } } [ "![==[]==]" qparse ] unit-test

! Comments allow tokens or comments right after
{ t } [ "![==[]==]a" qparse length 1 = ] unit-test
{ { } } [ "![==[]==]!" qparse ] unit-test
{ { } } [ "![==[]==]![[]]" qparse ] unit-test

! Strings
[ "\"abc" qparse ] must-fail
[ "\"abc\"abc" qparse ] must-fail

{ } [ 0 0 ": suffix ( a b -- c ) ;" qparse-function 3drop ] unit-test

{ }
[ "[ H{ } clone callbacks set-global ] \"alien\" add-startup-hook" qparse drop ] unit-test


{ 1 } [ "SYMBOLS: ;" qparse first object>> length ] unit-test
{ 0 } [ "SYMBOLS: ;" qparse first object>> first length ] unit-test
{ 1 } [ "SYMBOLS: a b c ;" qparse first object>> length ] unit-test
{ 3 } [ "SYMBOLS: a b c ;" qparse first object>> first length ] unit-test

{ 1 } [ 0 0 "SYMBOLS: ;" qparse-symbols 2drop object>> length ] unit-test
{ 0 } [ 0 0 "SYMBOLS: ;" qparse-symbols 2drop object>> first length ] unit-test
{ 1 } [ 0 0 "SYMBOLS: a b c ;" qparse-symbols 2drop object>> length ] unit-test
{ 3 } [ 0 0 "SYMBOLS: a b c ;" qparse-symbols 2drop object>> first length ] unit-test

{ } [ "CONSTANT: a b" qparse drop ] unit-test
{ } [ "CONSTANT: a b " qparse drop ] unit-test
{ } [ "CONSTANT: a \"b\"" qparse drop ] unit-test
{ } [ "CONSTANT: a \"b\" " qparse drop ] unit-test
{ } [ "CONSTANT: a 3" qparse drop ] unit-test
{ } [ "CONSTANT: a 3 " qparse drop ] unit-test


{ t } [ "url\"factorcode.org\"" qparse first string-literal? ] unit-test
{ t } [ "url[[factorcode.org]]" qparse first run-time-long-string-literal? ] unit-test
{ t } [ "url{{factorcode.org}}" qparse first compile-time-long-string-literal? ] unit-test

{ t } [ "code[ a b ]" qparse first run-time-literal? ] unit-test
{ t } [ "code{ a b }" qparse first compile-time-literal? ] unit-test
{ t } [ "code( a b )" qparse first paren-literal? ] unit-test

{ 2 } [ "code[ a b ]" qparse first object>> length ] unit-test
{ 2 } [ "code{ a b }" qparse first object>> length ] unit-test
{ 2 } [ "code( a b )" qparse first object>> length ] unit-test

! Make sure we <slice> the entire object.
{ t } [ "url\"factorcode.org\"" [ qparse first slice>> ] keep sequence= ] unit-test
{ t } [ "url[[factorcode.org]]" [ qparse first slice>> ] keep sequence= ] unit-test
{ t } [ "url{{factorcode.org}}" [ qparse first slice>> ] keep sequence= ] unit-test
{ t } [ "code[ a b ]" [ qparse first slice>> ] keep sequence= ] unit-test
{ t } [ "code{ a b }" [ qparse first slice>> ] keep sequence= ] unit-test
{ t } [ "code( a b )" [ qparse first slice>> ] keep sequence= ] unit-test
*/