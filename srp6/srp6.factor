! Copyright (C) 2009 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors byte-arrays checksums checksums.sha
combinators combinators.smart generalizations io io.binary
io.encodings.binary io.encodings.string io.streams.byte-array
kernel locals math math.functions math.parser multiline
namespaces prettyprint random sequences tools.continuations
unicode.case ;
IN: crypto.srp6

: stream-write-flush ( bytes stream -- )
    [ stream-write ] [ stream-flush ] bi ;

: send-packet ( bytes stream -- )
    [ [ length 8 >be ] dip stream-write ]
    [ stream-write-flush ] 2bi ;

: packet-read ( stream -- bytes )
    [ 8 swap stream-read be> ] [ stream-read ] bi ;

SYMBOL: srp6
SINGLETON: srp6-four

HOOK: cs-hello srp6 ( obj -- )
HOOK: sp-hello srp6 ( obj -- )
HOOK: sc-reply-hello srp6 ( obj -- )
HOOK: cp-reply-hello srp6 ( obj -- )
HOOK: cs-send-M1 srp6 ( obj -- )
HOOK: sp-send-M1 srp6 ( obj -- )
HOOK: sc-send-M2 srp6 ( obj -- )
HOOK: cp-send-M2 srp6 ( obj -- )

! All slots here are public knowledge
TUPLE: srp6-state
    stream
    username salt A B g N k M return ;

! All extra srp6-client/server-state slots are private
TUPLE: srp6-client-state < srp6-state password a K ;
TUPLE: srp6-server-state < srp6-state b v x K ;

! Client
HOOK: set-A srp6 ( state -- state )
HOOK: calculate-client-K srp6 ( state -- state )
HOOK: calculate-client-M srp6 ( state -- state )
HOOK: verify-server-M srp6 ( state -- state )

M: srp6-four set-A
    256 random-bits* >>a
    dup [ g>> ] [ a>> ] [ N>> ] tri ^mod >>A ;

! Server
HOOK: lookup-username srp6 ( state -- state )
HOOK: set-g srp6 ( state -- state )
HOOK: set-N srp6 ( state -- state )
HOOK: set-B srp6 ( state -- state )
HOOK: calculate-server-K srp6 ( state -- state )
HOOK: verify-client-M srp6 ( state -- state )
HOOK: calculate-server-M srp6 ( state -- state )

ERROR: sequence-too-long seq n ;

: max-length ( seq n -- seq )
    2dup [ length ] dip > [ sequence-too-long ] [ drop ] if ;

M: srp6-four cs-hello
    [ username>> 128 max-length ] [ stream>> send-packet ] bi ;

M: srp6-four sp-hello
    [ stream>> packet-read ] [ username<< ] bi ;

M: srp6-four sc-reply-hello
    [
        [
            {
                [ salt>> ]
                [ g>> 1 >le ]
                [ N>> 32 >le ]
                [ B>> 32 >le ]
            } cleave
        ] output>array B{ } concat-as 
    ] [
        stream>> send-packet
    ] bi ;

M: srp6-four cp-reply-hello
    [ ]
    [ stream>> packet-read ] bi
    binary [
        32 read le> >>salt
        1 read le> >>g
        32 read le> >>N
        32 read le> >>B drop
    ] with-byte-reader ;

M: srp6-four cs-send-M1
    [ M>> ] [ stream>> send-packet ] bi ;

M: srp6-four sp-send-M1
    [ stream>> packet-read ] [ M<< ] bi ;

M: srp6-four sc-send-M2
    [ M>> ] [ stream>> send-packet ] bi ;

M: srp6-four cp-send-M2
    [ stream>> packet-read ] [ M<< ] bi ;

: <srp6-client-state> ( username password stream -- srp6-client-state )
    srp6-client-state new
        swap >>stream
        swap binary encode >>password
        swap binary encode >>username ;

: <srp6-server-state> ( stream -- srp6-server-state )
    srp6-server-state new
        swap >>stream ;

: client-srp6 ( username password stream srp6-type -- return )
    srp6 [
        <srp6-client-state>
        {
            [ cs-hello ]

            [ cp-reply-hello ]
            [ set-A calculate-client-K calculate-client-M drop ]

            [ cs-send-M1 ]

            [ cp-send-M2 ]
            [ verify-server-M drop ]
            [ return>> ]
        } cleave
    ] with-variable ;

: server-srp6 ( stream srp6-type -- return )
    srp6 [
        <srp6-server-state>
        {
            [ sp-hello ]
            [ lookup-username set-g set-N set-B drop ]

            [ sc-reply-hello ]

            [ sp-send-M1 ]
            [ calculate-server-K verify-client-M calculate-server-M drop ]

            [ sc-send-M2 ]
            [ return>> ]
        } cleave
    ] with-variable ;
