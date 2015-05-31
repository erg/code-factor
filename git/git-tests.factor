! Copyright (C) 2015 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: fry git io io.directories io.encodings.utf8
io.files.unique io.launcher kernel namespaces sequences
tools.test ;
IN: git.tests

: run-process-stdout ( process -- string )
    >process utf8 [ contents ] with-process-reader ;

: with-empty-test-git-repo ( quot -- )
    '[
        current-temporary-directory get [
            { "git" "init" } run-process drop
            @
        ] with-directory
    ] with-unique-directory drop ; inline

: with-zero-byte-file-repo ( quot -- )
    '[
        "empty-file" touch-file
        { "git" "add" "empty-file" } run-process drop
        { "git" "commit" "-m" "initial commit of empty file" } run-process drop
        @
    ] with-empty-test-git-repo ; inline

{ "refs/heads/master" } [
    [ git-head-ref ] with-empty-test-git-repo
] unit-test


{ } [
    [
        ! current-temporary-directory get t recursive-directory-files
        git-log [ commit. ] each
    ] with-zero-byte-file-repo
] unit-test

{ } [
    [
        { "git" "log" } run-process-stdout print
    ] with-zero-byte-file-repo
] unit-test
