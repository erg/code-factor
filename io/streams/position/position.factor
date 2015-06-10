! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors constructors destructors fry io kernel math
namespaces sequences ;
IN: io.streams.position

TUPLE: position-stream < disposable stream { n integer } ;

: new-position-stream ( stream class -- position-stream )
    new-disposable
        swap >>stream ; inline

: <position-stream> ( stream -- position-stream )
    \ position-stream new-position-stream ; inline

: with-advance-1 ( stream quot -- seq )
    [ call ] 2keep drop
    [ 1 + ] change-n drop ; inline

: with-advance ( stream quot -- seq )
    [ call ] 2keep drop over
    '[ _ dup sequence? [ length ] when + ] change-n drop ; inline

: with-advance-sep ( stream quot -- seq )
    [ call ] 2keep drop pick length pick [ 1 + ] when
    '[ _ + ] change-n drop ; inline

M: position-stream stream-readln
    [ stream>> stream-readln ] with-advance ;

M: position-stream stream-read1
    [ stream>> stream-read1 ] with-advance-1 ;

M: position-stream stream-contents*
    [ stream>> stream-contents* ] with-advance ;

M: position-stream stream-read-unsafe
    [ stream>> stream-read-unsafe ] with-advance ;

M: position-stream stream-read-until
    [ stream>> stream-read-until ] with-advance-sep ;

M: position-stream stream-element-type
    stream>> stream-element-type ;

M: position-stream dispose*
    stream>> dispose ;

M: position-stream stream-tell stream>> stream-tell ;


: input>position-stream ( -- )
    input-stream [ <position-stream> ] change ;

: input-position ( -- n ) input-stream get n>> ;

: output>position-stream ( -- )
    output-stream [ <position-stream> ] change ;

: output-position ( -- n ) output-stream get n>> ;
