! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs byte-arrays
combinators.short-circuit constructors kernel modern.paths
modern.quick-parser modern.syntax modern.tools multiline
quotations sequences strings ;
QUALIFIED: syntax
IN: modern.manifest

ERROR: unimplemented ;

/*
all-loose-source
dup random first2 [ print ] [ [ dup slice? [ >string ] when ] map describe ] bi*

all-loose-source
dup [ first2 [ print ] [ [ dup slice? [ >string ] when ] map describe ] bi* ] each

! files without any unit-tests
all-loose-source
[ nip [ dup slice? [ >string "unit-test" sequence= ] [ drop f ] if ] any? ] assoc-reject
dup [ first2 [ print ] [ [ dup slice? [ >string ] when ] map describe ] bi* ] each

! With add-startup-hook
all-loose-source
[ nip [ dup slice? [ >string "add-startup-hook" sequence= ] [ drop f ] if ] any? ] assoc-filter
dup [ first2 [ print ] [ [ dup slice? [ >string ] when ] map describe ] bi* ] each

! multiple IN:
all-parsed-alist
[ [ in? ] filter  ] assoc-map
[ nip second length 1 > ] assoc-filter


! passes
0) collect all using/use forms as first pass
1) define all symbols as second pass
2) output a quotation for each top-level thing left


! "Shuffleability"
1) <PRIVATE has to be moved as one, shuffability is still preserved inside
-- could use ``: foo ( -- ) ; private`` instead
-- or ``PRIVATE[ ]``

2) only one IN: form
-- or INTO: form
-- can get around it by nested modules

3) inline etc has to attach to definition
-- can parse ; til end -- "ugly rule" but exact same syntax
-- can put it between name and effect ``: foo inline ( -- ) ;``
--
4) no top-level loose code
-- here's a predicate, can just filter it and compile separately
CONSTANT: loose-code-quot
    [
        {
            [ slice? ]
            [ escaped? ]
            [ string-literal? ]
            [ run-time-literal? ]
            [ run-time-long-string-literal? ]
            [ compile-time-literal? ]
            [ compile-time-long-string-literal? ]
        } 1||
    ]
no UNUSE: -- check!

! find 10 largest factor files
! find . -name \*.factor -type f -print0 | xargs -0 du | sort -n | tail -10 | cut -f2 | xargs -I{} du -sh {}

! https://www.reddit.com/r/programming/comments/1vg4q0/which_programming_language_has_the_best_package/

! cryptosign
! yaml? factor? json?
! rest api
! any language
! containers docker
! cvs svn  git repo tags
! licenses
! semver
! distributed mirrors
! unregistering packages, aging
! source for dev, artifacts for deployment
! compare checksum on install
! easy to publish packages
! unit tests, pod docs
! nuget,cpan,npm
! which directory to download packages to
! bundle code as private package
! http://blog.versioneye.com/2015/06/22/introducing-pessimistic-mode-for-license-whitelist/
! http://spdx.org/licenses/, license whitelist

! opam http://opam.ocaml.org/
! Yum/Apt (C++ Libraries)
! Dub (D Programming Language)
! brew

! http://ed25519.cr.yp.to/


! TUPLE: module < identity-tuple
! name words
! main help
! source-loaded? docs-loaded? ;


/*
: <definitions> ( -- pair ) { HS{ } HS{ } } [ clone ] map ;

TUPLE: source-file
{ path string }
{ top-level-form quotation }
{ checksum byte-array }
definitions
main ;

TUPLE: module
{ name string }
{ syntax-contents string }
{ source-contents string }
{ docs-contents string }
{ tests-contents string } ;


*/


/*
TUPLE: loading path in using qualified ;


! CONSTRUCTOR: <catalog> catalog ( vocab-name -- catalog ) ;

: filter-in ( seq -- seq' ) [ { [ in? ] } 1|| ] filter ;

CONSTANT: using-quot
    [
        {
            [ in? ]
            [ using? ]
            [ use? ]
            [ qualified? ]
            [ qualified-with? ]
            [ from? ]
            [ exclude? ]
            [ rename? ]
        } 1||
    ]

CONSTANT: loose-code-quot
    [
        {
            [ char? ]
            [ slice? ]
            [ escaped? ]
            [ string-literal? ]
            [ run-time-literal? ]
            [ run-time-long-string-literal? ]
            [ compile-time-literal? ]
            [ compile-time-long-string-literal? ]
        } 1||
    ]

: filter-using ( seq -- seq' ) using-quot filter ;
: reject-using ( seq -- seq' ) using-quot reject ;

: filter-loose-code ( seq -- seq' ) loose-code-quot filter ;
: reject-loose-code ( seq -- seq' ) loose-code-quot reject ;

: partition-parsed ( seq -- loose defs meta )
    using-quot partition loose-code-quot [ not ] compose partition ;


GENERIC: obj>vocab-names ( obj -- names )

M: use obj>vocab-names object>> [ >string ] map ;
M: using obj>vocab-names object>> first [ >string ] map ;
M: in obj>vocab-names object>> first ;
M: from obj>vocab-names object>> first2 [ >string ] [ [ >string ] map ] bi* 2array 1array ;
M: exclude obj>vocab-names unimplemented ;

! XXX: SHADOWING BUG HERE, NAME IT QUALIFIED2 rawr
TUPLE: qualified2 name with ;
CONSTRUCTOR: <qualified2> qualified2 ( name -- obj ) dup name>> >>with ;
CONSTRUCTOR: <qualified2-with> qualified2 ( name with -- obj ) ;

M: qualified obj>vocab-names object>> [ >string <qualified2> ] map ;
M: qualified-with obj>vocab-names object>> first2 [ >string ] bi@ <qualified2-with> 1array ;

: handle-in ( meta -- obj )
    [ in? ] filter [ obj>vocab-names ] map concat ;

: handle-using ( meta -- obj )
    [ in? ] reject
    [ obj>vocab-names ] map concat ;

: handle-meta ( loading meta -- loading )
    [ handle-using >>using ] [ handle-in >>in ] bi ;

: handle-defs ( loading defs -- loading )
    drop
    ;
: handle-loose ( loading loose -- loading )
    drop
    ;

: load-modern-path' ( path -- loose defs meta )
    quick-parse-path partition-parsed rot swapd ;

: load-modern-path ( path -- obj )
    load-modern-path'
    ! loose defs meta
    loading new
        swap handle-meta
        swap handle-defs
        swap handle-loose ;

: load-modern-vocab' ( vocab-name -- loose defs meta )
    modern-source-path load-modern-path' ;

: load-modern-vocab ( vocab-name -- loading )
    modern-source-path load-modern-path ;

: remove-tests ( alist -- alist' ) [ drop "-tests.factor" tail? ] assoc-reject ;
: remove-docs ( alist -- alist' ) [ drop "-docs.factor" tail? ] assoc-reject ;

: all-loose-code ( -- seq )
    all-parsed-alist [ loose-code-quot filter ] assoc-map harvest-values ;

: all-loose-source ( -- seq )
    all-loose-code remove-tests remove-docs ;


: x64 ( -- obj )
    "/Users/erg/factor/basis/bootstrap/assembler/x86.64.factor" load-modern-path ;

: stage1 ( -- obj )
    "/Users/erg/factor/core/bootstrap/stage1.factor" load-modern-path ;

: stage2 ( -- obj )
    "/Users/erg/factor/basis/bootstrap/stage2.factor" load-modern-path ;

: html-entitites ( -- obj ) "html.entities" load-modern-vocab ;
: gl ( -- obj ) "opengl.gl" load-modern-vocab ;
: compression ( -- obj ) "compression.inflate" load-modern-vocab ;

*/