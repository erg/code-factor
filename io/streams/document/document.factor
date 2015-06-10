! Copyright (C) 2013 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays combinators constructors destructors fry
io io.private io.streams.position kernel math math.order
namespaces sequences strings ;
IN: io.streams.document

TUPLE: document-stream < position-stream { line integer } { column integer } ;
: <document-stream> ( stream -- document-stream )
    \ document-stream new-disposable
        swap >>stream ; inline

TUPLE: document-position { line integer } { column integer } ;
CONSTRUCTOR: <document-position> document-position ( line column -- document-position ) ;

TUPLE: document-object { position document-position } object ;
CONSTRUCTOR: <document-object> document-object ( position object -- document-object ) ;

: add-lines ( stream n -- stream )
    '[ _ + ] change-line ; inline

: with-advance-line ( stream quot -- seq )
    [ call ] 2keep drop
    1 add-lines 0 >>column drop ; inline

: count-newlines ( string -- n )
    [ CHAR: \n = ] count ;

: find-last-newline ( string -- n ? )
    [ CHAR: \n = ] find-last >boolean ;

: advance-string ( string stream -- )
    [ [ count-newlines ] dip over 0 >
    [ swap add-lines 0 >>column drop ] [ 2drop ] if ]
    [
        swap
        [ length ] [ find-last-newline ] bi [
            - 1 - >>column drop
        ] [
            drop swap [ + ] change-column drop
        ] if
    ] 2bi ;

: advance-1 ( stream n -- )
    CHAR: \n =
    [ 0 >>column 1 add-lines drop ]
    [ [ 1 + ] change-column drop ] if ; inline

: with-advance-1 ( n stream quot -- n )
    [ call ] 2keep drop over object>> advance-1 ; inline

M: document-stream stream-element-type call-next-method ;

M: document-stream stream-read
    [ nip stream-tell ] [ call-next-method ] 2bi <document-object> ;

M: document-stream stream-contents*
    [ stream-tell ] [ call-next-method ] bi <document-object> ;

M: document-stream stream-readln
    [
        [ stream-tell ] [ call-next-method ] bi <document-object>
    ] with-advance-line ;

M: document-stream stream-read1
    [
        [ stream-tell ] [ call-next-method ] bi <document-object>
    ] with-advance-1 ;

M: document-stream stream-read-unsafe
    [ call-next-method ] 3keep
    rot drop pick 0 > [ advance-string ] [ 2drop ] if ;

M: document-stream stream-read-until
    [ nip stream-tell ]
    [ call-next-method ]
    [ nip ] 2tri

    ! pos seq sep stream
    {
        [ 2drop dup [ <document-object> ] [ 2drop f ] if ]
        [ nip advance-string ]
        [ over [ stream-tell swap <document-object> nip ] [ 3drop f ] if ]
        [ swap advance-1 drop ]
    } 3cleave ;

M: document-stream stream-tell
    [ line>> ] [ column>> ] bi <document-position> ;


: write-newlines ( document-position stream -- )
    [ [ line>> ] bi@ [-] CHAR: \n <string> ]
    [ nip [ dup length ] dip swap dup 0 > [ add-lines 0 >>column drop ] [ 2drop ] if ]
    [ nip stream>> ] 2tri stream-write ;

: write-spaces ( document-position stream -- )
    [ [ column>> ] bi@ [-] CHAR: \s <string> ]
    [ nip [ dup length ] dip swap dup 0 > [ '[ _ + ] change-column drop ] [ 2drop ] if ]
    [ nip stream>> ] 2tri stream-write ;

: write-object ( document-object stream -- )
    [ object>> ] [ stream>> ] bi* over integer? [ stream-write1 ] [ stream-write ] if ;

! Writing
M: document-stream stream-write ( document-object stream -- )
    [ [ position>> ] dip [ write-newlines ] [ write-spaces ] 2bi ]
    [ write-object ]
    [ [ object>> ] dip over integer? [ swap advance-1 ] [ advance-string ] if ] 2tri ;

: input>document-stream ( -- )
    input-stream [ <document-stream> ] change ;

: input-position ( -- n ) input-stream get n>> ;

: output>document-stream ( -- )
    output-stream [ <document-stream> ] change ;
