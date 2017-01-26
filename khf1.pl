%% listlength(List,Length)
%% Length is the length of List
llength([], 0).
llength([_|Lt], Length):- 
    llength(Lt,N), 
    Length is N+1. 

%% @spec khf1:takeIndex(L::matrixElements(), Index::integer()) -> {SUCCESS::atom(),LL::any()}.
%%   Visszaadja az adott indexu elemet a listabol. Ha nagyobb az index, akkor az utolsot es ezt jelzi a par elso kifejezeseben egy atommal
%% OK => C = 0
%% 1 tol kezd indexelni
takeindex([L1|L2],Index,L1,1) :-
	llength([L1|L2],Llength),
	Llength = 1,
	Index >= 2.
takeindex([X|_],1,X,C) :-
	C is 0.
takeindex([L1|L2],Index,X,C) :- % when length(L)>= Index -> takeIndex(tl(L),Index-1);
	Index > 1,
	Index1 is Index-1,
	takeindex(L2,Index1,X,C).
	
%% rest(A,B,R)
%% R = A mod B
rest(0,_,0).
rest(A,B,A) :-
	A > 0,
	B > 0,
	B > A.
rest(A,B,R)	:-
	A > 0,
	B > 0,
	B =< A,
	A1 is A-B,
	rest(A1,B,R).

% elso x elem torolve a listabol
% Counter mindig 1
removex([],HowMany,Counter,[]).
removex([Lh|Lt],HowMany,Counter,Lt) :-
	Counter = HowMany.
removex([Lh|Lt],HowMany,Counter,Ret) :-
	Counter < HowMany,
	Counter1 is Counter+1,
	removex(Lt,HowMany,Counter1,Ret).

% elso x elem a listabol
% Counter mindig 1
getx([],HowMany,Counter,[]).
getx([Lh|Lt],HowMany,Counter,[]) :-
	Counter > HowMany.
getx([Lh|Lt],HowMany,Counter,[Lh|Ret1]) :-
	Counter =< HowMany,
	Counter1 is Counter + 1,
	getx(Lt,HowMany,Counter1,Ret1).

completesubmatrix(M,Melyikoszlop,Oszlopszam,Sorszam,Jelensor,[]) :-
	Jelensor =< Sorszam,
	Index is Melyikoszlop+((Jelensor-1)*Oszlopszam),
	llength(M,Mlength),
	Index > Mlength.
completesubmatrix(M,Melyikoszlop,Oszlopszam,Sorszam,Jelensor,[]) :-
	Jelensor > Sorszam.
completesubmatrix(M,Melyikoszlop,Oszlopszam,Sorszam,Jelensor,Ret) :-
	Jelensor =< Sorszam,
	Index is Melyikoszlop+((Jelensor-1)*Oszlopszam),
	llength(M,Mlength),
	Index =< Mlength,
	takeindex(M,Index,Elem,C),
	Jelensor1 is Jelensor+1,
	completesubmatrix(M,Melyikoszlop,Oszlopszam,Sorszam,Jelensor1,Ret1),
	append(Elem,Ret1,Ret).
	
completefirst(M,Oszlopszam,Sorszam,Jelenoszlop,[]) :-
	Jelenoszlop > Oszlopszam.
completefirst(M,Oszlopszam,Sorszam,Jelenoszlop,[Ret1|Ret]) :-
	Jelenoszlop =< Oszlopszam,
	completesubmatrix(M,Jelenoszlop,Oszlopszam,Sorszam,1,Ret1),
	Jelenoszlop1 is Jelenoszlop+1,
	completefirst(M,Oszlopszam,Sorszam,Jelenoszlop1,Ret).
	
completematrix([],Oszlopszam,Sorszam,[]).
completematrix(M,Oszlopszam,Sorszam,Ret) :-
	Manage is Oszlopszam*Sorszam,
	getx(M,Manage,1,Mtomanage),
	completefirst(Mtomanage,Oszlopszam,Sorszam,1,Ret1),
	removex(M,Manage,1,M1),
	llength(M1,M1l),
	M1l = 0,
	append(Ret1,[],Ret).
completematrix(M,Oszlopszam,Sorszam,Ret) :-
	Manage is Oszlopszam*Sorszam,
	getx(M,Manage,1,Mtomanage),
	completefirst(Mtomanage,Oszlopszam,Sorszam,1,Ret1),
	removex(M,Manage,1,M1),
	llength(M1,M1l),
	M1l > 0,
	completematrix(M1,Oszlopszam,Sorszam,Ret2),
	append(Ret1,Ret2,Ret).
	
	
	

% sublist1(L:list, M:int, N:int, S:list)
%	L bemeneti lista
%	M kezdeti pozicio
%	N veg pozicio
%	S az eredmeny lista
% Megjegyzes: 1-tol indexelunk. L,2,2,S az L lista masodik elemet adja vissza S-nek, Ha rossz a parameterezes akkor resz listat ad vagy ureset asszem
sublist1(L,M,N,S) :- sublist2(1,L,M,N,S).
sublist2(_,[],_,_,[]).
sublist2(I,[X|Xs],M,N,[X|Ys]) :-
    between1(M,N,I),
    J is I + 1,
    !, sublist2(J,Xs,M,N,Ys).
sublist2(I,[_|Xs],M,N,Ys) :-
    J is I + 1,
    sublist2(J,Xs,M,N,Ys).
between1(Lower, Upper, Point) :-
	integer(Lower),
	integer(Upper),
	(   integer(Point), !,		%  These cuts must be cuts;
	    Lower =< Point, Point =< Upper
	;   var(Point), !,		%  they can't be arrows.
	    Lower =< Upper,
	    between2(Lower, Upper, Point)
	).
between1(Lower, Upper, Point) :-
	Goal = between1(Lower,Upper,Point),
	must_be(Lower, integer, Goal, 1),
	must_be(Upper, integer, Goal, 2),
	must_be(Point, integer, Goal, 3).
	
between2(L, U, N) :- L=:=U, !, N = L.
between2(L, _, L).		% between2(L, U, L) :- L =< U.
between2(L, U, N) :-		% between2(L, U, N) :- L < U,
	M is L+1,		%	M is L+1,
	between2(M, U, N).	%	between2(M, U, N).
	

listalistafeldarabolasa(L,Index,Mod,[]) :-
	llength(L,Llength),
	Index > Llength.
listalistafeldarabolasa(L,Index,Mod,[R|Ret]) :-
	llength(L,Llength),
	Index =< Llength,
	IndexEnd is Index+Mod-1,
	sublist1(L,Index,IndexEnd,R),
	Index1 is Index+Mod,
	listalistafeldarabolasa(L,Index1,Mod,Ret).
	
listafeldarabolasa([],Mod,[]).
listafeldarabolasa([H|T],Mod,Ret) :-
	listalistafeldarabolasa(H,1,Mod,Hdarabolt),
	listafeldarabolasa(T,Mod,Ret1),
	append(Hdarabolt,Ret1,Ret).

kerekfel(Szam,Mod,Ret)	:-
	rest(Szam,Mod,R),
	R = 0,
	Ret is Szam.
kerekfel(Szam,Mod,Ret)	:-
	rest(Szam,Mod,R),
	R > 0,
	Szam1 is Szam-R,
	Ret is Szam1+Mod.
	
createwholeresult(M,X,Y,LL) :-
	listafeldarabolasa(M,X,Return),
	takeindex(M,1,Mfirstline,C),
	llength(Mfirstline,MlengthX),
	llength(M,MlengthY),
	kerekfel(MlengthX,X,Xegesz),
	kerekfel(MlengthY,Y,Yegesz),
	Oszlopszam is Xegesz//X,
	Sorszam is Y,
	completematrix(Return,Oszlopszam,Sorszam,LL).

	
feldarabolasa(M, Y-X, LL) :- createwholeresult(M,X,Y,LL).
	
% [[1,2],[3,4],[5],[1,2],[3,4],[5],[1,2],[3,4],[5]]
% [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]

% [[1,2,3],[4,5],[1,2,3],[4,5],[1,2,3],[4,5]]
% [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]
% [[a],[b],[c],[d],[e],[f],[g],[h]]