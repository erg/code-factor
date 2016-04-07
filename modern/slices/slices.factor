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

: peek-from ( n/f string -- ch )
    over [ ?nth ] [ 2drop f ] if ;

! Allow eof
: next-char-from ( n/f string -- n'/f string ch/f )
    over [
        2dup ?nth [ [ 1 + ] 2dip ] [ f ] if*
    ] [
        [ 2drop f ] [ nip ] 2bi f
    ] if ;

: find-from* ( ... n seq quot: ( ... elt -- ... ? ) -- ... i elt ? )
    [ find-from ] keep
    pick [ drop t ] [ length -rot nip f ] if ; inline

: skip-blank-from ( n string -- n' string )
    [ [ blank? not ] find-from* 2drop ] keep ; inline

: skip-til-eol-from ( n string -- n' string )
    [ [ "\r\n" member? ] find-from* 2drop ] keep ; inline

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

:: slice-while-whitespace-backwards ( n string -- slice/f )
    n 1 - string [ "\s\r\n" member? not ] find-last-from :> ( n' ch )
    n' [
        n' 1 + n string ?<slice>
    ] [
        0 n string ?<slice>
    ] if ; inline

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

: slice-between ( slice1 slice2 -- slice )
    ensure-same-underlying
    slice-order-by-from
    [ to>> ]
    [ [ from>> ] [ seq>> ] bi ] bi* <slice> ;

: slice-before ( slice -- slice' )
    [ drop 0 ] [ from>> ] [ seq>> ] tri <slice> ;