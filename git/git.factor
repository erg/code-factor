! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: compression.zlib io io.binary io.directories.search
io.encodings.binary io.encodings.utf8 io.files io.pathnames
kernel namespaces prettyprint sequences
sequences.generalizations tools.hexdump ;
IN: git

: find-git-directory ( path -- path' )
    [ ".git" tail? ] find-up-to-root ; inline

ERROR: not-a-git-directory path ;

: current-git-directory ( -- path )
    current-directory get find-git-directory [
        current-directory get not-a-git-directory
    ] unless* ;

: make-git-path ( str -- path )
    current-git-directory prepend-path ;

: make-refs-path ( str -- path )
    [ "refs/" make-git-path ] dip append-path ;

: make-object-path ( str -- path )
    [ "objects/" make-git-path ] dip 2 cut append-path append-path ;

: git-utf8-contents ( str -- contents )
    make-git-path utf8 file-contents ;

: git-binary-contents ( str -- contents )
    make-git-path binary file-contents ;

: list-refs ( -- seq )
    current-git-directory "refs/" append-path f recursive-directory-files ;

: ref-contents ( str -- contents )
    make-refs-path utf8 file-contents ;

: git-head-contents ( -- contents )
    "HEAD" git-utf8-contents ;



: git-stash-contents ( -- contents )
    "stash" git-utf8-contents ;

! : git-current-ref ( -- ref )
!    current-git-directory

: object-contents ( hash -- contents )
    make-object-path binary file-contents uncompress ;

ERROR: index-entry-expected ;

: read-index-entry ( -- seq )
    { 0 } read-until [ index-entry-expected ] unless
    3 read 0 prefix
    40 read
    20 read
    2 read 5 narray ;

: git-index-contents ( -- bytes )
    "index" make-git-path binary [
        4 read .
        4 read be> .
        4 read be>
        62 read hexdump.
        ! 512 read hexdump.
        [ read-index-entry ] replicate
        "rest" print
        contents length .
    ] with-file-reader ;

! : parse-index ( bytes -- index )
!    character-parser