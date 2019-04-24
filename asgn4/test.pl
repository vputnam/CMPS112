#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/gprolog/bin/gprolog --consult-file

not(X) :- X, !, fail.
not(_).

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

convert_to_radians( degmin( Deg, Min), Rad) :-
   Rad is ((Deg + (Min/60)) * (pi / 180)).

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

arrival_time( Start, Dest, DTime, ATime) :-
   airport( Start, _, degmin(FLatD, FLatM), degmin(FLonD, FLonM)),
   airport( Dest, _, degmin(ALatD, ALatM), degmin(ALonD, ALonM)), 
   convert_to_radians( degmin(FLatD, FLatM), FLat),
   convert_to_radians( degmin(ALatD, ALatM), ALat),
   convert_to_radians( degmin(FLonD, FLonM), FLon),
   convert_to_radians( degmin(ALonD, ALonM), ALon),
   haversine_radians( FLat, FLon, ALat, ALon, Dist ),
   ATime is (Dist/500) + DTime. 

hoursmins_to_hours( time( Hours, Mins ), Hoursonly ) :-
    Hoursonly is Hours + Mins / 60.

findpath( To, To, _,[To], _).
findpath( From, To, BeenTo, [[From, Dtime, Atime] | List], DtimeHM) :-
   flight( From, To, DtimeHM),
   not( member( To, BeenTo)),
   hoursmins_to_hours( DtimeHM, Dtime),
   arrival_time( From, To, Dtime, Atime),
   Atime < 24.0,
   findpath( To, To, [To | BeenTo], List, _).

findpath( From, To, BeenTo, [[From, Dtime, Atime] | List], DtimeHM) :-
   flight( From, X, DtimeHM),
   not( member( X, BeenTo)),
   hoursmins_to_hours( DtimeHM, Dtime),
   arrival_time( From, To, Dtime, Atime),
   Atime < 24.0,
   write( 'Dtime is:' ),
   write( Dtime ), nl,
   write( 'Atime is: ' ),
   write( Atime ), nl,
   flight( X, _, XDtimeHM),
   hoursmins_to_hours( XDtimeHM, XDtime),
   Ytime is XDtime - Atime - 0.5,
   Ytime >= 0,
   write( 'XDtime is:' ),
   write( XDtime ), nl,
   write( 'XDtimeHM is: ' ),
   write( XDtimeHM ), nl,
   findpath( X, To, [X | BeenTo], List, XDtimeHM).   

%print %two %digits %when %printing %time
print_nums( Nums ) :-
    Nums < 10, print( 0 ), print( Nums ).

print_nums( Nums ) :-
    Nums >= 10, print( Nums ).

print_time( Hoursonly ) :-
    Hours is floor( Hoursonly ),
    Mins is ( (Hoursonly - Hours)*60 ),
    Temp is ( floor(Mins/ 60) ),
    Hours_updated is ( Temp + Hours ),
    Mins_updated is ( round(Mins - (Temp*60)) ),
    print_nums( Hours_updated ),
    print( ':' ),
    print_nums( Mins_updated ).

%This %will %do %all %of %the %writing
writepath( [] ) :-
   nl.
writepath( [[From, Dtime1, Atime1], To | []] ) :-
   airport( From, From_name, _, _),
   airport( To, To_name, _, _),
   write( '     ' ), write( 'depart  ' ),
   write( From ), write( '  ' ),
   write( From_name ),
   print_time( Dtime1 ), nl,

   write( '     ' ), write( 'arrive  ' ),
   write( To ), write( '  ' ),
   write( To_name ),
   print_time( Atime1 ), nl,
   !, true.
writepath( [[From, Dtime1, Atime1], [To, Dtime2, Atime2] | Cdr]) :-
   airport( From, From_name, _, _),
   airport( To, To_name, _,_),
   write( '     ' ), write( 'depart  ' ),
   write( From ), write( '  ' ),
   write( From_name ),
   print_time( Dtime1 ), nl,

   write( '     ' ), write( 'arrive  ' ),
   write( To ), write( '  ' ),
   write( To_name ),
   print_time( Atime1 ), nl,
   !, writepath( [[To, Dtime2, Atime2] | Cdr]).
   

%Main %function
fly( From, From ) :-
   write( 'You\'re departing from where you already are! Dummy!.' ),
   nl,
   !, fail.

fly( From, To ) :-
    airport( From, _, _, _ ),
    airport( To, _, _, _ ),

    findpath( From, To, [From], List, _ ),
    !, nl,
    writepath( List ),
    true.

fly( From, To ) :-
    airport( From, _, _, _ ),
    airport( To, _, _, _ ),
    write( 'Your search did not return any results.' ),
    !, fail.
fly( _, _) :-
    write( 'Airports do not match those in our database.' ), nl,
    !, fail. 

