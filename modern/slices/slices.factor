! Copyright (C) 2016 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs fry kernel locals math math.private
modern sequences sequences.extras sequences.private unicode ;
IN: modern.slices

: matching-char ( ch -- ch' )
    H{
        { CHAR: ( CHAR: ) }
        { CHAR: [ CHAR: ] }
        { CHAR: { CHAR: } }
        { CHAR: < CHAR: > }
    } ?at drop ;

ERROR: unexpected-end n string ;
: nth-check-eof ( n string -- nth )
    2dup ?nth [ 2nip ] [ unexpected-end ] if* ;

! Allow eof
: next-char-from ( n/f string -- n'/f string ch/f )
    over [
        2dup ?nth [ [ 1 + ] 2dip ] [ f ] if*
    ] [
        [ 2drop f ] [ nip ] 2bi f
    ] if ;

: iterate-step ( i n quot -- i n quot )
    ! Apply quot to i, keep i and quot, hide n.
    [ nip call ] 3keep ; inline

: (find-integer*) ( ... i n quot: ( ... i -- ... ? ) -- ... i/f )
    [
        iterate-step iterate-rot
        [ 2drop ] [ iterate-next (find-integer*) ] if
    ] [ 2drop ] if-iterate? ; inline recursive

: (find-from*) ( n seq quot quot' -- i elt )
    [ 2dup bounds-check? ] 2dip
    [ (find) ] 2curry
    [ drop f ]
    if ; inline

: find-from* ( ... n seq quot: ( ... elt -- ... ? ) -- ... i elt )
    [ (find-integer*) ] (find-from*) ; inline


: skip-blank-from ( n string -- n' string )
    [ [ blank? not ] find-from* drop ] keep ; inline

: skip-til-eol-from ( n string -- n' string )
    [ [ "\r\n" member? ] find-from* drop ] keep ; inline

:: slice-til-eol-from ( n string -- n' string slice/f ch/f )
    n string '[ "\r\n" member? ] find-from :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

! Don't include the whitespace in the slice
:: slice-until-whitespace ( n string -- n' string slice/f ch/f )
    n string '[ "\s\r\n" member? ] find-from :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

:: slice-until-separator-inclusive ( n string tokens -- n' string slice/f ch/f )
    n string '[ tokens member? ] find-from [ dup [ 1 + ] when ] dip  :> ( n' ch )
    n' string
    n n' string ?<slice>
    ch ; inline

: slice-until-separator-exclusive ( n string tokens -- n' string slice/f ch/f )
    slice-until-separator-inclusive dup [
        [ [ 1 - ] change-to ] dip
    ] when ;

:: slice-until-either ( n string tokens -- n' string slice/f ch )
    n string '[ tokens member? ] find-from
    dup "\s\r\n" member? [
        :> ( n' ch )
        n' string
        n n' string ?<slice>
        ch
    ] [
        [ dup [ 1 + ] when ] dip :> ( n' ch )
        n' string
        n n' string ?<slice>
        ch
    ] if ; inline

: skip-one-space-after ( n string -- n' string )
    next-char-from [
        dup blank?
        [ drop ]
        [ whitespace-expected-after ] if
    ] when* ;

:: slice-until-string ( n string search --  n' string payload end-string )
    search string n start* :> n'
    n' [ n string search subseq-expected-but-got-eof ] unless
    n' search length +  string
    n n' string ?<slice>
    n' dup search length + string ?<slice> ;

: modify-to ( slice n -- slice' )
    [ [ from>> ] [ to>> ] [ seq>> ] tri ] dip
    swap [ + ] dip <slice> ;

! { CHAR: ] [ read-closing ] }
! { CHAR: } [ read-closing ] }
! { CHAR: ) [ read-closing ] }
: read-closing ( n string tok -- n string tok )
    dup length 1 = [
        -1 modify-to [ 1 - ] 2dip
    ] unless ;

: merge-slice-until-whitespace ( n string slice --  n' string slice' )
    [ slice-until-whitespace drop ] dip merge-slices ;
