! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs assocs.private classes.builtin
combinators.short-circuit constructors hash-sets hashtables
kernel locals modern.paths multiline namespaces sequences
splitting fry ;
QUALIFIED: words
QUALIFIED: vocabs
IN: modern.vocabs

! dict is all vocabs
TUPLE: linear-state { using hash-set }
{ using-namespace hashtable }
{ namespace hashtable }
in compilation-unit? private? last-word decorators dict ;
CONSTRUCTOR: <linear-state> linear-state ( -- obj )
    HS{ } clone >>using
    H{ } clone >>using-namespace
    H{ } clone >>namespace
    V{ } clone >>decorators
    10 <hashtable> >>dict ;

: with-linear-state ( quot -- )
    [ <linear-state> \ linear-state ] dip with-variable ; inline


: words>named-hashtable ( seq -- hashtable )
    [ { [ words:primitive? ] [ builtin-class? ] } 1|| ] filter
    [ [ name>> ] keep ] H{ } map>assoc ;

: prepopulate-vocab ( name -- public private )
    ".private" ?tail drop
    [ dup vocabs:vocab-words words>named-hashtable 2array ]
    [ ".private" append dup vocabs:vocab-words words>named-hashtable 2array ] bi 2array ;


: prepopulate-vocabs ( -- hashtable )
    core-bootstrap-vocabs [ prepopulate-vocab ] map concat
    [ nip assoc-empty? not ] assoc-filter ;

SYMBOL: prepopulated-vocabs

\ prepopulated-vocabs [
    prepopulate-vocabs >hashtable
] initialize

SYMBOL: modern-vocabs

: get-modern-vocab ( string -- vocab/f )
    modern-vocabs get ?at [
    ] [
        [ quick-compile-vocab ] keep \ modern-vocabs get [ set-at ] 3keep 2drop
    ] if ;


ERROR: key-already-exists value key assoc ;
: set-once-at ( value key assoc -- )
    2dup key? [ key-already-exists ] [ set-at ] if ;

! Make a new hashtable where keys are vocab names and values are hashtables of name,word pairs
:: join-vocabs ( vocabs -- vocabs' )
    H{ } clone :> out
    vocabs [
        [
            :> ( key value )
            key out ?at [
                :> vocab
                value [ swap vocab set-once-at ] assoc-each
            ] [
                H{ } clone dup :> vocab swap out set-at
                value [ swap vocab set-once-at ] assoc-each
            ] if
        ] assoc-each
    ] each out ;

: vocabs>namespace ( hashtable -- namespace )
    [ H{ } clone ] dip over '[
        nip [ swap _ push-at ] assoc-each
    ] assoc-each ;