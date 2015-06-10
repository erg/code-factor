! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: nested-comments ;
IN: modern.parser.terse

PARSER: package { name } PACKAGE: get-string ;
PARSER: import { name } IMPORT: get-string ;
PARSER: imports { names } IMPORTS: ";" strings-until ;
PARSER: author { name } AUTHOR: get-string ;
PARSER: from { module functions } FROM: token ";" strings-until ;
PARSER: use { name } USE: get-string ;
PARSER: using { names } USING: ";" strings-until ;
PARSER: in { module } IN: token ;
PARSER: main { function } MAIN: get-string ;
PARSER: char { n } CHAR: token ;
PARSER: escaped { parse } \ parse ;
PARSER: private { body } <PRIVATE: "PRIVATE>" parse-until ;
PARSER: constant { name object } CONSTANT: identifier parse ;
PARSER: tuple { name body } TUPLE: identifier body ;
PARSER: error { name body } ERROR: identifier body ;
PARSER: block { body } [ "]" parse-until ;
PARSER: array { body } { "}" parse-until ;
PARSER: vector { body } V{ "}" parse-until ;
PARSER: hashtable { body } H{ "}" parse-until ;
PARSER: generic { body } GENERIC: token parse-signature ;
PARSER: method { class name body } M: parse parse-signature ;
PARSER: signature { in out } ( "--" strings-until ")" strings-until ;

: parse-signature ( -- signature )
    "--" strings-until ")" strings-until <signature> ;

PARSER: execute-parens { signature } execute( parse-signature ;
PARSER: call-parens { signature } call( parse-signature ;
PARSER: function { name signature body }
    : identifier parse-signature body ;

LITERAL-PARSER: inline
LITERAL-PARSER: foldable
LITERAL-PARSER: flushable
