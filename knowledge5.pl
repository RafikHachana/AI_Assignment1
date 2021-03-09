

%the adjacent rule tests if the cell (X,Y) is adjacent to (Xcurr,Ycurr)
%the following are all the possible cases of adjacency
%diagonal 

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr+1,
	Y2 is Ycurr+1.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr-1,
	Y2 is Ycurr+1.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr+1,
	Y2 is Ycurr-1.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr-1,
	Y2 is Ycurr-1.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr,
	Y2 is Ycurr+1.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr+1,
	Y2 is Ycurr.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr,
	Y2 is Ycurr-1.

adjacent((Xcurr,Ycurr),(X2,Y2)) :-
	X2 is Xcurr-1,
	Y2 is Ycurr.

withinBounds((X,Y)) :-
	%write("Checking Bounds of "),
	%write((X,Y)),
	X =< 9,
	Y =< 9,
	X >= 1,
	Y >= 1.





%The following is known

home((2,2)).
start((1,1)).

%The following is unknown
doctor((5,5)).
mask((8,8)).
covid((3,2)).
covid((2,5)).
/*covid((2,2)).
covid((2,1)).*/



in((X,Y),[(X,Y)| _ ]).
in((X,Y),[_| Tail]) :-
	in((X,Y),Tail).

covidNeighborhoodV1([(0,0),(0,1),(0,-1),(1,0),(1,1),(1,-1),(-1,0),(-1,1),(-1,-1)]).
covidNeighborhoodV2([(0,0),(0,1),(0,2),(0,-1),(0,-2),
					(1,0),(1,1),(1,2),(1,-1),(1,-2),
					(2,0),(2,1),(2,2),(2,-1),(2,-2),
					(-1,0),(-1,1),(-1,2),(-1,-1),(-1,-2),
					(-2,0),(-2,1),(-2,2),(-2,-1),(-2,-2)]).

checkCovidCells(_,[]).
checkCovidCells((X,Y),[(XS,YS)|Tail]) :-
	NewX is X+XS, NewY is Y+YS,
	\+ covid((NewX,NewY)),
	checkCovidCells((X,Y),Tail).

covidFreeV1((X,Y)) :-
	covidNeighborhoodV1(List),
	checkCovidCells((X,Y),List).

covidFreeV2((X,Y)) :-
	covidNeighborhoodV2(List),
	checkCovidCells((X,Y),List).

safe(Cell,_):- covidFreeV1(Cell), withinBounds(Cell).
safe(_,Safe) :- Safe = 1.

covidPerceivedV2(Cell) :- \+ covidFreeV2(Cell).

isItSafe(Cell,1) :- (doctor(Cell);mask(Cell)).


%1st algorithm: Backtracking with DFS and Binary Search

possibleMove((X1,Y1),(X2,Y2)) :- 
	adjacent((X1,Y1),(X2,Y2)),
	withinBounds((X2,Y2)).


pathDFS(Cell,Cell,_,[Cell],Depth,MaxDepth,_) :- Depth =< MaxDepth.

%DFS before being safe from Covid
pathDFS(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
	%Current \= Destination,
	\+ doctor(Current),
	\+ mask(Current),
	possibleMove(Current,Next),
	\+ member([Next,0],Visited),
	safe(Next,0),
	NewDepth is Depth +1,
	%writeln(Current),
	pathDFS(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,0),
	Result = [Current | Tail].

%DFS when finding a mask or a doctor
pathDFS(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
	%Current \= Destination,
	(doctor(Current);mask(Current)),
	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1, 	%writeln(Current),
	pathDFS(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

%DFS after becoming immune to covid
pathDFS(Current,Destination,Visited,Result,Depth,MaxDepth,1) :-
	Depth =< MaxDepth,
		%Current \= Destination,

	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1,
		%writeln(Current),

	pathDFS(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

searchDFS(Start,Destination,MaxPathLength,MaxPathLength) :- 
	pathDFS(Start,Destination,[],Result,0,MaxPathLength,0),   %Singleton Variable Result
	length(Result,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Result).

searchDFS(Start,Destination,MaxPathLength,MaxPathLength) :- 
	%\+ pathDFS(Start,Destination,[],Result,0,MaxPathLength,0),   %Singleton Variable Result
	writeln("Outcome : Lose.").


searchDFS(Start,Destination,MinPathLength,MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	%write("1st predicate "),writeln(M),
	pathDFS(Start,Destination,[],Result,0,M,0),    %Singleton Variable Result
	searchDFS(Start,Destination,MinPathLength,M).

searchDFS(Start,Destination,MinPathLength, MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	%write("2nd predicate "),writeln(M),
	%\+ pathDFS(Start,Destination,[],Result,0,M,0),    %Singleton Variable Result
	NewMin is M+1,
	searchDFS(Start,Destination,NewMin,MaxPathLength).


searchBacktracking(Start,Destination) :- 
	searchDFS(Start,Destination,1,18).


/*dfs([[Current,Safe,Depth,Parent]|Stack],Visited,Destination,Depth) :-
	findall((X,Y),possibleMove((1,1),(X,Y)),List).*/








%For variant 2, I will prioritize the nodes that are far from the covid first.

pathDFSV2(Cell,Cell,_,[Cell],Depth,MaxDepth,_) :- Depth =< MaxDepth.

%DFS before being safe from Covid, we will try to go to a node far from from covid.
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
	%Current \= Destination,
	\+ doctor(Current),
	\+ mask(Current),
	possibleMove(Current,Next),
	\+ member([Next,0],Visited),
	\+ covidPerceivedV2(Next),
	safe(Next,0),
	NewDepth is Depth +1,
	%writeln(Current),
	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,0),
	Result = [Current | Tail].

%This will be tested if we can't find a node far from covid 
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
	%Current \= Destination,
	\+ doctor(Current),
	\+ mask(Current),
	possibleMove(Current,Next),
	\+ member([Next,0],Visited),
	safe(Next,0),
	NewDepth is Depth +1,
	%writeln(Current),
	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,0),
	Result = [Current | Tail].

%DFS when finding a mask or a doctor
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
	%Current \= Destination,
	(doctor(Current);mask(Current)),
	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1, 	%writeln(Current),
	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

%DFS after becoming immune to covid
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,1) :-
	Depth =< MaxDepth,
		%Current \= Destination,

	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1,
		%writeln(Current),

	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

searchDFSV2(Start,Destination,MaxPathLength,MaxPathLength) :- 
	pathDFSV2(Start,Destination,[],Result,0,MaxPathLength,0),   %Singleton Variable Result
	length(Result,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Result).


searchDFSV2(Start,Destination,MaxPathLength,MaxPathLength) :- 
	%\+ pathDFS(Start,Destination,[],Result,0,MaxPathLength,0),   %Singleton Variable Result
	writeln("Outcome : Lose.").


searchDFSV2(Start,Destination,MinPathLength,MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	%write("1st predicate "),writeln(M),
	pathDFSV2(Start,Destination,[],Result,0,M,0),    %Singleton Variable Result
	searchDFSV2(Start,Destination,MinPathLength,M).

searchDFSV2(Start,Destination,MinPathLength, MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	%write("2nd predicate "),writeln(M),
	%\+ pathDFS(Start,Destination,[],Result,0,M,0),    %Singleton Variable Result
	NewMin is M+1,
	searchDFSV2(Start,Destination,NewMin,MaxPathLength).


searchBacktrackingV2(Start,Destination) :- 
	searchDFS(Start,Destination,1,18).


%THE 2nd algorithm: A*

max(X,Y,X) :- X>=Y.
max(X,Y,Y) :- Y>=X.
 
calculateHeuristic((X1,Y1),(X2,Y2),H) :-
	Xaxis is abs(X2-X1),
	Yaxis is abs(Y2-Y1),
	max(Xaxis,Yaxis,H).

calculateHeuristicV2((X1,Y1),(X2,Y2),Result,1) :-
	calculateHeuristic((X1,Y1),(X2,Y2),Result).

calculateHeuristicV2((X1,Y1),(X2,Y2),Result,0) :-
	calculateHeuristic((X1,Y1),(X2,Y2),H),
	covidPerceivedV2((X1,X2)),
	Result is H+5.

calculateHeuristicV2((X1,Y1),(X2,Y2),Result,0) :-
	calculateHeuristic((X1,Y1),(X2,Y2),Result).

%The following probably doesn't work because it doesn't match the pattern
removeFromList(_,_,[],[]). %Attention: This might be dangerous.
removeFromList(Safe,X,[[Safe,_,_,_,X]|Tail],Tail).

removeFromList(Safe,X,[[S,_,_,_,Y]|Tail],Result) :-
	(X \= Y;Safe\=S),
	removeFromList(Safe,X,Tail,Result).

%Base case
filterNextMoves(_,_,_,_,[],OpenList,ClosedList,OpenList,ClosedList).

filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	(\+ withinBounds(Current);\+ safe(Current,Safe);CurrentParent = Current),
	%write(Current), write(" in filterNextMoves (illegal move)\n"),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewOpen = NewOpenTail,
	NewClosed = NewClosedTail.

%When it is in neither of the lists.
filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	%write(Current), write(" in filterNextMoves (neither of the lists)\n"),
	safe(Current,Safe),
	Current \= CurrentParent,
	calculateHeuristic(Current,Destination,H),
	%write(H),
	\+ member([Safe,_,_,_,Current],OpenList),
	\+ member([Safe,_,_,_,Current],ClosedList),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewDist is ParentDist+1,
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

%In the closed list
filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
	%write(Current), write(" in filterNextMoves (closed list)\n"),
	%calculateHeuristic(Current,Destination,H),
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist =< NewDist,
	%write(Current + " is in the closed list\n"),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewOpen = NewOpenTail.

filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
		%write(Current), write(" in filterNextMoves (closed list)\n"),

	calculateHeuristic(Current,Destination,H),
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist > NewDist,
	removeFromList(Safe,Current,ClosedList,ClosedListWithoutCurrent),
	%write(Current + " is in the closed list\n"),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedListWithoutCurrent,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

%In the open list
filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
		%write(Current), write(" in filterNextMoves (open list)\n"),

	calculateHeuristic(Current,Destination,H),
	member([Safe,_,PrevDist,_,Current],OpenList),
	NewDist is ParentDist+1,
	PrevDist > NewDist,
	removeFromList(Safe,Current,OpenList,OpenListWithoutCurrent),
	%write(Current + " is in the open list\n"),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenListWithoutCurrent,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
		%write(Current), write(" in filterNextMoves (open list)\n"),

	%calculateHeuristic(Current,Destination,H),
	member([Safe,_,PrevDist,_,Current],OpenList),
	%write(Current + " is in the open list\n"),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewDist is ParentDist+1,
	PrevDist =< NewDist,
	NewOpen = NewOpenTail.


pathAStar([[Safe,H,Dist,Parent,Destination]|_],ClosedList,Destination,[[Safe,H,Dist,Parent,Destination]|ClosedList]).
	%write("We are at the Destination!!!\n").
	%Result = [[Safe,H,Dist,Parent,Destination]|ClosedList].


%When a node is not safe
pathAStar([[0,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	(X,Y) \= Destination,
	\+ doctor((X,Y)), \+ mask((X,Y)),
	%write((X,Y)), write(" in pathAStar\n"),
	%write("Open list is : "), write(OpenList), write("\n"),
	%Make a list of potential successors of the current node
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	%write(Neighbors), write(" (Neighbors) in pathAStar\n"),
	%Filter the list: Valid, safe cells, that obey the open and closed list rules
	%write("We're gonna filter dem moves!!\n"),
	filterNextMoves(0,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	%write(FilteredOpenList),
	%write("We're gonna sort\n"),
	sort(FilteredOpenList,NewOpenList),
    %nextCellSafety((X,Y),)
    /*write("Open list for the next iteration : "), write(NewOpenList),write("\n"),
    write("Closed list for the next iteration : "), write([[0,H,Dist,Parent,(X,Y)]|FilteredClosedList]),write("\n"),
    write("Destination for the next iteration : "), write(Destination),write("\n"),
    write("Result for the next iteration : "), write(Result),write("\n"),*/
	pathAStar(NewOpenList,[[0,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).
	%write("Our new result : ") , write(Result), write("\n\n").

%when the node is a doctor or a mask
pathAStar([[0,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	(X,Y) \= Destination,
	(doctor((X,Y));mask((X,Y))),
	%write((X,Y)), write(" in pathAStar (and we became safe now!)\n"),
	%write("Open list is : "), write(OpenList), write("\n"),
	%Make a list of potential successors of the current node
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	%write(Neighbors), write(" (Neighbors) in pathAStar\n"),
	%Filter the list: Valid, safe cells, that obey the open and closed list rules
	filterNextMoves(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	%write(FilteredOpenList),
	sort(FilteredOpenList,NewOpenList),
	%write("Open list for the next iteration : "), write(NewOpenList),write("\n"),
    %nextCellSafety((X,Y),)
	pathAStar(NewOpenList,[[0,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).

%When it is already safe
pathAStar([[1,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	(X,Y) \= Destination,
	%write((X,Y)), write(" in pathAStar (we are immune)\n"),
	%write("Open list is : "), write(OpenList), write("\n"),
	%Make a list of potential successors of the current node
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	%write(Neighbors), write(" (Neighbors) in pathAStar\n"),
	%Filter the list: Valid, safe cells, that obey the open and closed list rules
	filterNextMoves(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	
	sort(FilteredOpenList,NewOpenList),
    %nextCellSafety((X,Y),)
    %write("Open list for the next iteration : "), write(NewOpenList),write("\n"),
	pathAStar(NewOpenList,[[1,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).

safeBit(Cell,[[Safe,_,_,_,Cell]|_],Safe).

safeBit(Cell,[[_,_,_,_,OtherCell]|Tail],Safe) :- Cell \= OtherCell, safeBit(Cell,Tail,Safe).

findParentAStar(_,_,nil,[]).
findParentAStar(Safe,Cell,Parent,[[Safe,_,_,Parent,Cell]|_]).
findParentAStar(Safe,Cell,Parent,[[OtherSafe,_,_,_,OtherCell]|Tail]) :- 
	(Cell \= OtherCell; Safe \= OtherSafe),
	%writeln(Tail),
	findParentAStar(Safe,Cell,Parent,Tail).

constructPathAStar(0,nil,_,_,[]).

constructPathAStar(0,PathStart,PathStart,_,[PathStart]). %works if the start doesn't have a doctor or mask
%If the safe bit changes (always from 1 to 0)
constructPathAStar(1,Current,PathStart,ParentList,Result) :-
	Current\=PathStart,
	Current \= nil,
	%write("Constructing path at "), write(Current), write("(2nd pred) with Safety"), write(1),write("\n"),
	FirstParent = nil,
	findParentAStar(1,Current,nil,ParentList),
	%write("This is a doctor or a mask!\n"),
	findParentAStar(0,Current,Parent,ParentList),
	constructPathAStar(0,Parent,PathStart,ParentList,NewResult),
	Result = [Current | NewResult].
%finding a parent of the same safe bit
constructPathAStar(Safe,Current,PathStart,ParentList,Result) :-
	Current\=PathStart,
	Current \= nil,
	%write("Constructing path at "), write(Current), write(" with Safety"), write(Safe),write("\n"),
	findParentAStar(Safe,Current,Parent,ParentList),
	Parent \= nil,
	constructPathAStar(Safe,Parent,PathStart,ParentList,NewResult),
	Result = [Current | NewResult].




searchAStar(Start,Destination) :-
	calculateHeuristic(Start,Destination,H),
	pathAStar([[0,H,0,nil,Start]],[],Destination,List),
	%write("Done with A*\n \n \n"),
	%writeln(List),
	safeBit(Destination,List,DestinationSafeBit),
	%writeln(DestinationSafeBit),
	constructPathAStar(DestinationSafeBit,Destination,Start,List,ReversedPath),
	reverse(ReversedPath,Path),
	length(Path,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Path).

searchAStar(Start,Destination) :-
	writeln("Outcome : Lose.").






%A* with the 2nd variant is here

filterNextMovesV2(_,_,_,_,[],OpenList,ClosedList,OpenList,ClosedList).

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	(\+ withinBounds(Current);\+ safe(Current,Safe);CurrentParent = Current),
	%write(Current), write(" in filterNextMoves (illegal move)\n"),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewOpen = NewOpenTail,
	NewClosed = NewClosedTail.

%When it is in neither of the lists.
filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	%write(Current), write(" in filterNextMoves (neither of the lists)\n"),
	safe(Current,Safe),
	Current \= CurrentParent,
	calculateHeuristicV2(Current,Destination,H,Safe),
	%write(H),
	\+ member([Safe,_,_,_,Current],OpenList),
	\+ member([Safe,_,_,_,Current],ClosedList),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewDist is ParentDist+1,
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

%In the closed list
filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist =< NewDist,
	%write(Current + " is in the closed list\n"),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewOpen = NewOpenTail.

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
		%write(Current), write(" in filterNextMoves (closed list)\n"),

	calculateHeuristicV2(Current,Destination,H,Safe),
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist > NewDist,
	removeFromList(Safe,Current,ClosedList,ClosedListWithoutCurrent),
	%write(Current + " is in the closed list\n"),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedListWithoutCurrent,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

%In the open list
filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
		%write(Current), write(" in filterNextMoves (open list)\n"),

	calculateHeuristicV2(Current,Destination,H,Safe),
	member([Safe,_,PrevDist,_,Current],OpenList),
	NewDist is ParentDist+1,
	PrevDist > NewDist,
	removeFromList(Safe,Current,OpenList,OpenListWithoutCurrent),
	%write(Current + " is in the open list\n"),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenListWithoutCurrent,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),
	safe(Current,Safe),
	Current \= CurrentParent,
		%write(Current), write(" in filterNextMoves (open list)\n"),

	%calculateHeuristic(Current,Destination,H),
	member([Safe,_,PrevDist,_,Current],OpenList),
	%write(Current + " is in the open list\n"),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewDist is ParentDist+1,
	PrevDist =< NewDist,
	NewOpen = NewOpenTail.


pathAStarV2([[Safe,H,Dist,Parent,Destination]|_],ClosedList,Destination,[[Safe,H,Dist,Parent,Destination]|ClosedList]).
	%write("We are at the Destination!!!\n").
	%Result = [[Safe,H,Dist,Parent,Destination]|ClosedList].


%When a node is not safe
pathAStarV2([[0,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	(X,Y) \= Destination,
	\+ doctor((X,Y)), \+ mask((X,Y)),
	%write((X,Y)), write(" in pathAStar\n"),
	%write("Open list is : "), write(OpenList), write("\n"),
	%Make a list of potential successors of the current node
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	%write(Neighbors), write(" (Neighbors) in pathAStar\n"),
	%Filter the list: Valid, safe cells, that obey the open and closed list rules
	%write("We're gonna filter dem moves!!\n"),
	filterNextMovesV2(0,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	%write(FilteredOpenList),
	%write("We're gonna sort\n"),
	sort(FilteredOpenList,NewOpenList),
    %nextCellSafety((X,Y),)
    /*write("Open list for the next iteration : "), write(NewOpenList),write("\n"),
    write("Closed list for the next iteration : "), write([[0,H,Dist,Parent,(X,Y)]|FilteredClosedList]),write("\n"),
    write("Destination for the next iteration : "), write(Destination),write("\n"),
    write("Result for the next iteration : "), write(Result),write("\n"),*/
	pathAStarV2(NewOpenList,[[0,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).
	%write("Our new result : ") , write(Result), write("\n\n").

%when the node is a doctor or a mask
pathAStarV2([[0,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	(X,Y) \= Destination,
	(doctor((X,Y));mask((X,Y))),
	%write((X,Y)), write(" in pathAStar (and we became safe now!)\n"),
	%write("Open list is : "), write(OpenList), write("\n"),
	%Make a list of potential successors of the current node
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	%write(Neighbors), write(" (Neighbors) in pathAStar\n"),
	%Filter the list: Valid, safe cells, that obey the open and closed list rules
	filterNextMovesV2(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	%write(FilteredOpenList),
	sort(FilteredOpenList,NewOpenList),
	%write("Open list for the next iteration : "), write(NewOpenList),write("\n"),
    %nextCellSafety((X,Y),)
	pathAStarV2(NewOpenList,[[0,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).

%When it is already safe
pathAStarV2([[1,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	(X,Y) \= Destination,
	%write((X,Y)), write(" in pathAStar (we are immune)\n"),
	%write("Open list is : "), write(OpenList), write("\n"),
	%Make a list of potential successors of the current node
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	%write(Neighbors), write(" (Neighbors) in pathAStar\n"),
	%Filter the list: Valid, safe cells, that obey the open and closed list rules
	filterNextMovesV2(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	
	sort(FilteredOpenList,NewOpenList),
    %nextCellSafety((X,Y),)
    %write("Open list for the next iteration : "), write(NewOpenList),write("\n"),
	pathAStarV2(NewOpenList,[[1,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).


searchAStarV2(Start,Destination) :-
	calculateHeuristicV2(Start,Destination,H,0),
	pathAStarV2([[0,H,0,nil,Start]],[],Destination,List),
	safeBit(Destination,List,DestinationSafeBit),
	%writeln(DestinationSafeBit),
	constructPathAStar(DestinationSafeBit,Destination,Start,List,ReversedPath),
	reverse(ReversedPath,Path),
	length(Path,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Path).

searchAStarV2(Start,Destination) :-
	writeln("Outcome : Lose.").


execute:-
	start(Actor),
	home(Home),
	writeln("\n---Backtracking with variant 1 : ---\n"),
	time(searchBacktracking(Actor,Home)),
	writeln("\n---Backtracking with variant 2 : ---\n"),
	time(searchBacktrackingV2(Actor,Home)),
	writeln("\n--- A* with variant 1 : ---\n"),
	time(searchAStar(Actor,Home)),
	writeln("\n--- A* with variant 2 : --- \n"),
	time(searchAStarV2(Actor,Home)).