! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs classes fry io kernel modern.paths
modern.quick-parser sequences sequences.extras sorting ;
QUALIFIED-WITH: modern.syntax m
IN: modern.tools


/*
: all-parsed-alist ( -- alist )
    all-factor-files
    [ dup quick-parse-path ] map>alist ;

! Find slots named foo. e.g. "length" all-slots-subseq print-alist-results
: all-slots-subseq ( subseq -- seq )
    [
        all-parsed-alist
        [ [ class-of m:tuple = ] filter ] assoc-map
    ] dip '[
        [ ] [ object>> second >out [ _ swap subseq? ] any? ] map-filter f like
    ] assoc-map sift-values ;

: print-alist-results ( seq -- )
    sort-keys
    [ [ print ] dip >out [ print ] each nl ] assoc-each ;
*/