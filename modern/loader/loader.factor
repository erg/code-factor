! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs constructors continuations fry
io.directories io.directories.search io.files io.files.types
io.pathnames kernel modern.parser modern.parser.factor
namespaces parser sequences sets splitting ;
IN: modern.loader

SYMBOL: modules
modules [ H{ } clone ] initialize

TUPLE: loader ;

TUPLE: module name paths dictionary ;

CONSTRUCTOR: <module> module ( name -- module ) ;

SYMBOL: module-roots
module-roots [ V{ "resource:core/" "resource:basis/" "resource:extra/" } clone ] initialize

: replace-dots ( name -- name' )
    { { CHAR: . CHAR: / } }  substitute ;

: append-module ( module-root name -- path )
    replace-dots append-path ;

: ?directory-entries ( path -- seq/f )
    '[ _ directory-entries ] [ drop f ] recover ;

: filter-directories ( path -- seq/f )
    ?directory-entries [ type>> +directory+ = ] filter ;

: filter-files ( path -- seq/f )
    ?directory-entries [ type>> +regular-file+ = ] filter ;

: filter-factor ( seq -- seq' )
    [ ".factor" tail? ] filter ;

: root-name>files ( module-root name -- seq )
    append-module filter-files ;

: root-name>paths ( module-root name -- seq )
    append-module
    dup filter-files [ name>> append-path ] with map ; 

: root-name>factor-paths ( module-root name -- seq )
    root-name>paths filter-factor ;

: path>files ( path -- seq )
    qualified-directory-entries
    [ type>> +regular-file+ = ] filter
    [ name>> ] map ;

: path-is-module? ( path -- ? )
    directory-entries [ type>> +regular-file+ = ] filter
    empty? not ;

: root-name>potential-modules ( module-root -- seq )
    t recursive-directory-entries
    [ type>> +directory+ = ] filter
    [ name>> ] map ;

: root-name>modules ( root -- seq )
    root-name>potential-modules [ path-is-module? ] filter ;

: root-name>module-path? ( module-root name -- ? )
    root-name>files empty? not ;

: root-name-subpath>path ( module-root name subpath -- path )
    [ dup append-module ] dip "-" glue ".factor" append append-path ;

: root>main-path ( module-root name -- path )
    "" root-name-subpath>path ;

: root-name>docs-path ( module-root name -- path )
    "docs" root-name-subpath>path ;

: root-name>syntax-path ( module-root name -- path )
    "syntax" root-name-subpath>path ;

: root-name>tests-path ( module-root name -- path )
    "tests" root-name-subpath>path ;

: name>paths ( name -- paths )
    [ module-roots get ] dip
    '[ _ root-name>paths ] map-find drop ;

: name>all-paths ( name -- paths )
    [ module-roots get ] dip
    '[ _ root-name>paths ] map ;

: prune-non-modern-files ( paths -- paths' )
    dup [ "-modern.factor" tail? ] filter
    [ "-modern.factor" ?tail drop ".factor" append ] map diff ;

: name>factor-paths ( name -- paths )
    name>paths filter-factor prune-non-modern-files ;

: root>loadable-files ( root -- paths )
    root-name>modules
    [ path>files ] map concat prune-non-modern-files ;

: loadable-core-files ( -- paths )
    "resource:core" root>loadable-files
    {
        "/home/erg/factor/core/vocabs/loader/test/a/a.factor"
        "/home/erg/factor/core/vocabs/loader/test/b/b.factor"
        "/home/erg/factor/core/vocabs/loader/test/c/c.factor"
    } diff ;

: loadable-basis-files ( -- paths )
    "resource:basis" root>loadable-files ;

: loadable-extra-files ( -- paths )
    "resource:extra" root>loadable-files ;

ERROR: module-path-conflict paths ;

ERROR: module-not-found name ;

: module-main-path ( name -- path )
    dup name>all-paths harvest [
        module-not-found
    ] [
        nip
        dup length 1 = [ module-path-conflict ] unless
    ] if-empty ;

: parse-module ( name -- module )
    module-main-path [ [ parse-file ] map ] map concat ;

GENERIC: parsed ( parsed -- )
GENERIC: compile ( parsed -- quotation )

! M: using parsed strings>> [ parse-module ] map ;
