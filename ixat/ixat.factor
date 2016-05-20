USING: accessors arrays calendar combinators constructors fry io
json.writer kernel make math math.functions math.vectors
namespaces prettyprint random sequences sets timers vectors ;
IN: ixat

CONSTANT: gps-delta          .001
CONSTANT: car-speed          .000001
CONSTANT: displacement-error .00001

SYMBOL: car-counter

: random-delta ( -- x )
    gps-delta [ neg ] [ ] bi uniform-random-float 2 random 1 = [ neg ] when ;

: random-displacement ( -- array )
    random-delta random-delta 2array ;

TUPLE: car id { current array } { to array } ;
CONSTRUCTOR: <car> car ( id current -- car )
    dup current>> random-displacement v+ >>to ;

: car>coords ( car -- to current )
    [ to>> ] [ current>> ] bi ;

: displacement ( car -- coord )
    car>coords v- ;

: moving? ( car -- ? )
    car>coords displacement-error v~ ;

: displacement-velocity ( car -- velocity )
    displacement [ signum car-speed * ] map ;

: update-position ( car -- car )
    dup displacement-velocity '[ _ v+ ] change-current ;

: update-cars ( seq -- seq' )
    [ update-position ] map ;

: make-n-cars ( n coord -- cars )
    '[ car-counter counter _ <car> ] replicate ;

: default-cars ( n -- cars )
    { 10 10 } make-n-cars ;

: car>json ( car -- json )
    [
        {
            [ id>> "id" ,, ]
            [ current>> first2 [ "lat" ,, ] [ "lng" ,, ] bi* ]
            [ to>> first2 [ "to-lat" ,, ] [ "to-lng" ,, ] bi* ]
        } cleave
    ] H{ } make >json ;

: cars>json. ( cars -- )
    [ car>json ] map . flush ;

: run-cars-timer ( n quot -- timer )
    [ default-cars ] dip '[
        _ update-cars @
    ] 1 seconds every ;
