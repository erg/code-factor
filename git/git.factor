! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: arrays checksums combinators compression.zlib
constructors io io.binary io.directories.search
io.encodings.binary io.encodings.string io.encodings.utf8
io.files io.pathnames kernel math namespaces prettyprint
sequences sequences.generalizations tools.hexdump ;
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

TUPLE: index-entry ctime mtime data sha1 flags name ;
CONSTRUCTOR: <index-entry> index-entry ( ctime mtime data sha1 flags name -- obj ) ;

ERROR: index-entry-expected ;

: read-index-entry-v2 ( -- seq )
    4 read
    4 read 2array

    4 read
    4 read 2array

    4 read
    4 read
    4 read
    4 read
    4 read
    4 read 6 narray

    20 read hex-string

    2 read
    { 0 } read-until [ index-entry-expected ] unless
    [ utf8 decode ] [ length ] bi
    7 + 8 mod dup zero? [ 8 swap - ] unless read drop
    <index-entry> ;

TUPLE: git-index magic version entries checksum ;
CONSTRUCTOR: <git-index> git-index ( magic version entries checksum -- obj ) ;

ERROR: unhandled-git-version n ;
ERROR: unhandled-git-index-trailing-bytes bytes ;

: git-index-contents ( -- bytes )
    "index" make-git-path binary [
        4 read utf8 decode
        4 read be>
        4 read be> over {
            { 2 [ [ read-index-entry-v2 ] replicate ] }
            [ unhandled-git-version ]
        } case
        20 read hex-string
        contents [ unhandled-git-index-trailing-bytes ] unless-empty
        <git-index>
    ] with-file-reader ;
