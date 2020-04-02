%tower using 
tower(0,[],counts([],[],[],[])).
tower(N,T,C):- 
	
	
	% array size limits
    length(T,N),
    len_col(T, N),
    within_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, X),
    maplist(fd_all_different, X),
	maplist(fd_labeling, T),
	
	reverse_2d(T, RT),
	reverse_2d(X, RX),
	
	
	C = counts(TO,BO,LE,RI),
	length(TO,N),
	length(BO,N),
	length(LE,N),
	length(RI,N),
	
	
	check(TO,X),
	check(BO,RX),
	check(LE,T),
	check(RI,RT).
	
	

check([],[]).
check( COUNTS,MATRIX):-
	maplist(checkeach, COUNTS, MATRIX).
	
%by default checks from the left to the right
checkeach(COUNT, LIST):-
	increasing(LIST,COUNT,0,0).

% third argument:local max    fourth argument: number of increasing
increasing([],N,_,N).
increasing([H|T],N,M,O):-
	H > M,
	OO is O+1,
	increasing(T,N,H,OO).

increasing([H|T],N,M,O):-
	H =< M,
	increasing(T,N,M,O).
	
	
	

reverse_2d(X, RX) :-
    maplist(reverse, X, RX).


len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HD | TL], N) :-
    % http://www.gprolog.org/manual/html_node/gprolog057.html fd_domain(Vars, Lower, Upper)
    fd_domain(HD, 1, N),
    within_domain(TL, N).

% This is SWI-prolog's old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).
		

plain_tower(0,[],counts([],[],[],[])).
plain_tower(N,T,C):- 
	
	length(T,N),
	maplist(all_unique, T),
	matrix_between(T,1,N),
    
    len_col(T, N),
    

	reverse_2d(T, RT),
    
	transpose(T, X),
    
	maplist(all_unique, X),
	reverse_2d(X, RX),
	
	
	
	C = counts(TO,BO,LE,RI),
	length(TO,N),
	length(BO,N),
	length(LE,N),
	length(RI,N),
	
	
	check(TO,X),
	check(BO,RX),
	check(LE,T),
	check(RI,RT).
	
	
all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).


elements_between( Min, Max, List) :-
maplist(between(Min,Max), List).

matrix_between(Matrix, Min, Max) :-
maplist(elements_between(Min, Max), Matrix).
