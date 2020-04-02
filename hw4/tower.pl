%tower using fd
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
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%plain tower

plain_tower(0,[],counts([],[],[],[])).
plain_tower(N,T,C):- 
	wd(N, Domain),!,
	C = counts(TO,BO,LE,RI),
	length(TO,N),
	length(BO,N),
	length(LE,N),
	length(RI,N),
	length(T,N),
	fill_2d(T,Domain),
	transpose(T, X),
	maplist(all_unique, X),
	
	reverse_2d(X, RX),
	
	reverse_2d(T, RT),
	
    
	
	

	
	
	check(TO,X),
	check(BO,RX),
	check(LE,T),
	check(RI,RT).
	
	

wd(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], _).
fill_2d([Head | Tail], Domain) :-
    permutation(Domain, Head),
    fill_2d(Tail, Domain).

all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Speedup
stats_tower(T) :-
  statistics(cpu_time, [Start|_]),
  tower(4,Ta,Ca),
  tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          Row3,
          [RC41,5|Row4Tail],
          Row5],
         counts(Top, [4|BottomTail],
                [Left1,Left2,Left3,Left4,5],
                Right)),
  statistics(cpu_time, [End|_]),
  T is End - Start + 1.

stats_plain_tower(T) :-
  statistics(cpu_time, [Start|_]),
  plain_tower(4,Ta,Ca),
  plain_tower(5,
         [[2,3,4,5,1],
          [5,4,1,3,2],
          Row3,
          [RC41,5|Row4Tail],
          Row5],
         counts(Top, [4|BottomTail],
                [Left1,Left2,Left3,Left4,5],
                Right)),
  statistics(cpu_time, [End|_]),
  T is End - Start.

speedup(Ratio) :-
  stats_tower(T),
  stats_plain_tower(PT),
  Ratio is PT / T.
  
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %ambiguous
 
 ambiguous(N, C, T1, T2) :-
  tower(N, T1, C),
  tower(N, T2, C),
  T1 \= T2.
