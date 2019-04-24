% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $


%

%% Prolog version of not.

%%



not( X ) :- X, !, fail.
not( _ ).

zero( M ) :- M < 1.

% convert degrees, minutes to radians
dmtr(D, M, R) :- I is (0.0166667 * M),
                 T is I+D,
                 R is (0.0174533 * T).

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


print_ticket(Origin, Destination, time(DHr, DMin), time(AHr, AMin)) :- 
    airport(Origin, OCity, _, _),
    airport(Destination, DCity, _, _),
    write('depart   '),
    write( Origin ),
    write('   '),
    write( OCity ),
    write('   '),
    write(DHr),
    write(':'),
    write(DMin),
    print_extra_zero(DMin),
    nl,
    write('arrive   '),
    write( Destination ),
    write('   '),
    write( DCity ),
    write('   '),
    write(AHr),
    write(':'),
    write(AMin),
    print_extra_zero(AMin),
    nl.

print_extra_zero(M) :-
    zero(M) ->
    write('0')
    ; true.

mathable(time(Hrs,Mins), Ans) :- 
    Ans is Hrs + (Mins/60).

printable(Amt, time(Hrs,Mins)) :-
    H is floor(Amt),
    Mins is round((Amt-H)*60),
    Hrs is H.

% function to get the arrival time of a plane based on its
% flight departure time
get_arr_time(Origin, Destination, Deptime, Arrtime) :-
    airport(Origin, _, Lat, Lon),
    airport(Destination, _, Lat2, Lon2),
    Lat = degmin(D1, M1),
    dmtr(D1, M1, LatRad),
    Lon = degmin(D2, M2),
    dmtr(D2, M2, LonRad),
    Lat2 = degmin(Dd1, Dm1),
    Lon2 = degmin(Dd2, Dm2),
    dmtr(Dd1, Dm1, DLatRad),
    dmtr(Dd2, Dm2, DLonRad),
    haversine_miles(LatRad, LonRad, DLatRad, DLonRad, D),
    %Greatcircle is (D/500),
    %write('Greatcircle: '),
    %write(Greatcircle), nl,
    Arrtime is ((D/500) + Deptime).

%ispath( L, M ) :- ispath2( L, M, [] ).

%ispath2( L, L, _ ).

%ispath2( L, M, Path ) :-
%   flight( L, X, Time),
%   not( member( X, Path )),
%   ispath2( X, M, [L|Path] ).

writepath( [] ) :-
   nl.

writepath( [[Origin, ODeptime, OArrtime], 
[Connection, CDeptime, CArrtime], Destination|Tail] ) :-
  % write( 'Flight1: ' ),
   printable(ODeptime, F1dep),
   printable(OArrtime, F1arr),
   print_ticket(Origin, Connection, F1dep, F1arr), 
   %write( 'Flight2: ' ),
   printable(CDeptime, F2dep),
   printable(CArrtime, F2arr),
   print_ticket(Connection, Destination, F2dep, F2arr),
   writepath( Tail ).

% same origin and destination
listpath( Node, Node, _,[Node], _).

% end of recursion
listpath( Node, Next, Try, [[Node, Deptime, Arrtime] | List], 
DeptimeHM) :-
   flight( Node, Next, DeptimeHM),
   not( member( Next, Try)),
   mathable( DeptimeHM, Deptime),
   get_arr_time( Node, Next, Deptime, Arrtime),
   Arrtime < 24.0,
   %print_ticket(Node, Next, DeptimeHM, Arrtime),
   listpath( Next, Next, [Next | Try], List, _).

% recursive heavy lifting
listpath( Node, Next, Try, [[Node, Deptime, Arrtime] | List],
DeptimeHM) :-
   flight( Node, Some, DeptimeHM),
   not( member( Some, Try)),
   mathable( DeptimeHM, Deptime),
   get_arr_time( Node, Next, Deptime, Arrtime),
   Arrtime < 24.0,
   %write( 'Deptime is:' ),
   %write( Deptime ), nl,
   %write( 'Arrtime is: ' ),
   %write( Arrtime ), nl,
   flight( Some, _, XDeptimeHM),
   mathable( XDeptimeHM, XDeptime),
   Transfer is XDeptime - Arrtime - 0.5,
   Transfer >= 0,
   %write( 'XDeptime is:' ),
   %write( XDeptime ), nl,
   %write( 'XDeptimeHM is: ' ),
   %write( XDeptimeHM ), nl,
   %print_ticket(Node, Next, XDeptimeHM, Arrtime),
   listpath( Some, Next, [Some | Try], List, XDeptimeHM).


% function to find route with single plane ride (best case)

find_route(Origin, Destination) :-
    flight(Origin, Destination, Time),
    mathable(Time, Stime),
    get_arr_time(Origin, Destination, Stime, Etime),
    printable(Etime, UEtime),
    print_ticket(Origin, Destination, Time, UEtime).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  essentially our main function right now  %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% same origin and destination

fly(Abbr1, Abbr1) :-
    write('Usage error: fly(location1, location2).'), 
    nl.

% otherwise
fly(Abbr1, Abbr2) :-
    find_route(Abbr1, Abbr2).

% if we need a better way
fly(Node, Next) :-
    listpath(Node, Next, [Node], List, _),
    write( Node ), write( ' to ' ), write( Next ), write( ' is ' ),
    nl,
    writepath( List ),
    true.    

% if there is no match\
fly(Node, Next) :-
    Node,
    Next,
    write('Sorry, a matching valid flight is not currently supported.'),
    nl,
    fail.

haversine_miles( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is (Dist * 3961).
