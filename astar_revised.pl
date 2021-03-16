%the adjacent rule tests if the cell (X,Y) is adjacent to (Xcurr,Ycurr)
%the following are all the possible cases of adjacency
%diagonal 

%The following are all the possible adjacent pairs of cells (to be used in DFS)

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

%check if the cell is within bounds of the map

withinBounds((X,Y)) :-
	maxX(MAXX), maxY(MAXY),
	X =< MAXX,
	Y =< MAXY,
	X >= 1,
	Y >= 1.


%The following are the variables of the map

%home((2,2)).
start((1,1)).
maxX(9).
maxY(9).


:- dynamic home/1.
:- dynamic mask/1.
:- dynamic covid/1.
:- dynamic doctor/1.





%These describe the covid infected cells
covidNeighborhoodV1([(0,0),(0,1),(0,-1),(1,0),(1,1),(1,-1),(-1,0),(-1,1),(-1,-1)]).

%The infected cells and the adjacent ring around them
covidNeighborhoodV2([(0,0),(0,1),(0,2),(0,-1),(0,-2),
					(1,0),(1,1),(1,2),(1,-1),(1,-2),
					(2,0),(2,1),(2,2),(2,-1),(2,-2),
					(-1,0),(-1,1),(-1,2),(-1,-1),(-1,-2),
					(-2,0),(-2,1),(-2,2),(-2,-1),(-2,-2)]).

%checks the moore neighborhood of a cell to see if it is infected
checkCovidCells(_,[]).
checkCovidCells((X,Y),[(XS,YS)|Tail]) :-
	NewX is X+XS, NewY is Y+YS,
	\+ covid((NewX,NewY)),
	checkCovidCells((X,Y),Tail).

%checks whether the cell is infected or not
covidFreeV1((X,Y)) :-
	covidNeighborhoodV1(List),
	checkCovidCells((X,Y),List).

%checks if the cell is covid infected or adjacent to a covid zone
covidFreeV2((X,Y)) :-
	covidNeighborhoodV2(List),
	checkCovidCells((X,Y),List).

%the opposite of the above
covidPerceivedV2(Cell) :- \+ covidFreeV2(Cell).

%checks if a cell is safe (we are either immune or the cell is safe)
safe(Cell,_):- covidFreeV1(Cell), withinBounds(Cell).
safe(_,Safe) :- Safe = 1.


%isItSafe(Cell,1) :- (doctor(Cell);mask(Cell)).


%1st algorithm: Backtracking with DFS and Binary Search
%checks for a move to be legal 
possibleMove((X1,Y1),(X2,Y2)) :- 
	adjacent((X1,Y1),(X2,Y2)),
	withinBounds((X2,Y2)).

%Base case: we found the home 
pathDFS(Cell,Cell,_,[Cell],Depth,MaxDepth,_) :- Depth =< MaxDepth.

%DFS before being safe from Covid
pathDFS(Current,Destination,Visited,Result,Depth,MaxDepth,0) :- %Result will have the final path 
	Depth =< MaxDepth,
	\+ doctor(Current),
	\+ mask(Current),
	possibleMove(Current,Next),
	\+ member([Next,0],Visited),
	safe(Next,0),
	NewDepth is Depth +1,
	pathDFS(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,0),
	Result = [Current | Tail].

%DFS when finding a mask or a doctor
pathDFS(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
	(doctor(Current);mask(Current)),
	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1, 	%writeln(Current),
	pathDFS(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

%DFS after becoming immune to covid
pathDFS(Current,Destination,Visited,Result,Depth,MaxDepth,1) :-
	Depth =< MaxDepth,
	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1,
	pathDFS(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

%This is the binary search function on the path length 

searchDFS(Start,Destination,MaxPathLength,MaxPathLength) :- 
	pathDFS(Start,Destination,[],Result,0,MaxPathLength,0),
	length(Result,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Result).

searchDFS(_,_,MaxPathLength,MaxPathLength) :- 
	writeln("Outcome : Lose.").


searchDFS(Start,Destination,MinPathLength,MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	pathDFS(Start,Destination,[],_,0,M,0),    %Singleton Variable Result
	searchDFS(Start,Destination,MinPathLength,M).

searchDFS(Start,Destination,MinPathLength, MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	NewMin is M+1,
	searchDFS(Start,Destination,NewMin,MaxPathLength).

%the main function to call 
searchBacktracking(Start,Destination) :- 
	searchDFS(Start,Destination,1,25).


%For variant 2, I will prioritize the nodes that are far from the covid first.

pathDFSV2(Cell,Cell,_,[Cell],Depth,MaxDepth,_) :- Depth =< MaxDepth.

%DFS before being safe from Covid, we will try to go to a node far from from covid.
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,

	\+ doctor(Current),
	\+ mask(Current),
	possibleMove(Current,Next),
	\+ member([Next,0],Visited),
	\+ covidPerceivedV2(Next),      %this is the only difference from variant 1
	safe(Next,0),
	NewDepth is Depth +1,
	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,0),
	Result = [Current | Tail].

%This will be tested if we can't find a node far from covid 
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,0) :-
	Depth =< MaxDepth,
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
	(doctor(Current);mask(Current)),
	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1, 	%writeln(Current),
	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

%DFS after becoming immune to covid
pathDFSV2(Current,Destination,Visited,Result,Depth,MaxDepth,1) :-
	Depth =< MaxDepth,
	possibleMove(Current,Next),
	\+ member([Next,1],Visited),
	NewDepth is Depth +1,
	pathDFSV2(Next,Destination,[[Current,0]|Visited],Tail,NewDepth,MaxDepth,1),
	Result = [Current | Tail].

searchDFSV2(Start,Destination,MaxPathLength,MaxPathLength) :- 
	pathDFSV2(Start,Destination,[],Result,0,MaxPathLength,0), 
	length(Result,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Result).


searchDFSV2(_,_,MaxPathLength,MaxPathLength) :- 
	writeln("Outcome : Lose.").


searchDFSV2(Start,Destination,MinPathLength,MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	pathDFSV2(Start,Destination,[],_,0,M,0),
	searchDFSV2(Start,Destination,MinPathLength,M).

searchDFSV2(Start,Destination,MinPathLength, MaxPathLength) :-
	MinPathLength \= MaxPathLength,
	M is (MinPathLength+MaxPathLength)//2,
	NewMin is M+1,
	searchDFSV2(Start,Destination,NewMin,MaxPathLength).


searchBacktrackingV2(Start,Destination) :- 
	searchDFS(Start,Destination,1,25).


%THE 2nd algorithm: A*

%utility function for the max of 2 integers
max(X,Y,X) :- X>=Y.
max(X,Y,Y) :- Y>=X.
 
%calculates the heuristic : the diagonal distance between 2 cells 
calculateHeuristic((X1,Y1),(X2,Y2),H) :-
	Xaxis is abs(X2-X1),
	Yaxis is abs(Y2-Y1),
	max(Xaxis,Yaxis,H).



%utility function to remove an item from the queue of cells 
removeFromList(_,_,[],[]).
removeFromList(Safe,X,[[Safe,_,_,_,X]|Tail],Tail).

removeFromList(Safe,X,[[S,_,_,_,Y]|Tail],Result) :-
	(X \= Y;Safe\=S),
	removeFromList(Safe,X,Tail,Result).

%The following function takes the possible next moves and adds them to the queue if they are valid

%Base case
filterNextMoves(_,_,_,_,[],OpenList,ClosedList,OpenList,ClosedList).

filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	((\+ withinBounds(Current);\+ safe(Current,Safe));CurrentParent = Current),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewOpen = NewOpenTail,
	NewClosed = NewClosedTail.

%The cell is already in the open list and we need to update its info
filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,PrevDist,_,Current],OpenList),
	NewDist is ParentDist+1,
	PrevDist > NewDist,
	removeFromList(Safe,Current,OpenList,OpenListWithoutCurrent),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenListWithoutCurrent,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	calculateHeuristic(Current,Destination,H),
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

%the cell is already in the open list with a shorter distance, no need to update it 
filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,PrevDist,_,Current],OpenList),
	NewDist is ParentDist+1,
	PrevDist =< NewDist,
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewOpen = NewOpenTail.


%The cell is in the closed list, we either update it or not depending on the distance 

filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist > NewDist,
	removeFromList(Safe,Current,ClosedList,ClosedListWithoutCurrent),
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedListWithoutCurrent,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	calculateHeuristic(Current,Destination,H),
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist =< NewDist,
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewOpen = NewOpenTail.


%The cell is in neither of the lists, so we add it to the open list 
filterNextMoves(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	filterNextMoves(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewDist is ParentDist+1,
	calculateHeuristic(Current,Destination,H),
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].


%the algorithm itself

%Base cases 
pathAStar([[Safe,H,Dist,Parent,Destination]|_],ClosedList,Destination,[[Safe,H,Dist,Parent,Destination]|ClosedList]).
pathAStar([],_,_,nil).


%When a node is not safe
pathAStar([[2,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	withinBounds((X,Y)),safe((X,Y),0),(X,Y)\=Destination,
	\+ doctor((X,Y)), \+ mask((X,Y)),
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	filterNextMoves(2,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	
	sort(FilteredOpenList,NewOpenList),
    
	pathAStar(NewOpenList,[[2,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).
	

%when the node is a doctor or a mask
pathAStar([[2,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	withinBounds((X,Y)),safe((X,Y),0),(X,Y)\=Destination,
	(doctor((X,Y));mask((X,Y))),
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],
	filterNextMoves(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	sort(FilteredOpenList,NewOpenList),
	pathAStar(NewOpenList,[[2,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).

%When it is already safe (we are immune)
pathAStar([[1,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	withinBounds((X,Y)),safe((X,Y),0),(X,Y)\=Destination,
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	filterNextMoves(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	
	sort(FilteredOpenList,NewOpenList),
	pathAStar(NewOpenList,[[1,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).


%this one checks the safety bit of a cell in the closed list (used to see if we were immune when we reached destination)
safeBit(Cell,[[Safe,_,_,_,Cell]|_],Safe).

safeBit(Cell,[[_,_,_,_,OtherCell]|Tail],Safe) :- Cell \= OtherCell, safeBit(Cell,Tail,Safe).


%finds the parent of a cell in the closed list 
findParentAStar(_,_,nil,[]).
findParentAStar(Safe,Cell,Parent,[[Safe,_,_,Parent,Cell]|_]).
findParentAStar(Safe,Cell,Parent,[[OtherSafe,_,_,_,OtherCell]|Tail]) :- 
	(Cell \= OtherCell; Safe \= OtherSafe),
	%writeln(Tail),
	findParentAStar(Safe,Cell,Parent,Tail).


%constructs a path from the parents of cells to obtain the the final path 
constructPathAStar(2,nil,_,_,[]).

constructPathAStar(2,PathStart,PathStart,_,[PathStart]). %works if the start doesn't have a doctor or mask
%If the safe bit changes (always from 1 to 0)
constructPathAStar(1,Current,PathStart,ParentList,Result) :-
	Current\=PathStart,
	Current \= nil,
	findParentAStar(1,Current,nil,ParentList),
	findParentAStar(2,Current,Parent,ParentList),
	constructPathAStar(0,Parent,PathStart,ParentList,NewResult),
	Result = [Current | NewResult].
%finding a parent of the same safe bit
constructPathAStar(Safe,Current,PathStart,ParentList,Result) :-
	Current\=PathStart,
	Current \= nil,
	findParentAStar(Safe,Current,Parent,ParentList),
	Parent \= nil,
	constructPathAStar(Safe,Parent,PathStart,ParentList,NewResult),
	Result = [Current | NewResult].


%the main function to call 

searchAStar(Start,Destination) :-
	calculateHeuristic(Start,Destination,H),
	pathAStar([[2,H,0,nil,Start]],[],Destination,List),
	List \= nil,
	safeBit(Destination,List,DestinationSafeBit),
	constructPathAStar(DestinationSafeBit,Destination,Start,List,ReversedPath),
	reverse(ReversedPath,Path),
	length(Path,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Path).

searchAStar(_,_) :-
	writeln("Outcome : Lose.").




%A* with the 2nd variant is here


calculateHeuristicV2((X1,Y1),(X2,Y2),Result,1) :-
	calculateHeuristic((X1,Y1),(X2,Y2),Result).

calculateHeuristicV2((X1,Y1),(X2,Y2),Result,2) :-
	covidPerceivedV2((X1,X2)),
	calculateHeuristic((X1,Y1),(X2,Y2),H),
	Result is H+10.

calculateHeuristicV2((X1,Y1),(X2,Y2),Result,2) :-
	calculateHeuristic((X1,Y1),(X2,Y2),Result).

%Base case
filterNextMovesV2(_,_,_,_,[],OpenList,ClosedList,OpenList,ClosedList).

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	((\+ withinBounds(Current);\+ safe(Current,Safe));CurrentParent = Current),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewOpen = NewOpenTail,
	NewClosed = NewClosedTail.

%In the open list
filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,PrevDist,_,Current],OpenList),
	NewDist is ParentDist+1,
	PrevDist > NewDist,
	removeFromList(Safe,Current,OpenList,OpenListWithoutCurrent),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenListWithoutCurrent,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	calculateHeuristicV2(Current,Destination,H,Safe),
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,_,_,Current],OpenList),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewOpen = NewOpenTail.
%In the closed list

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,ClosedDist,_,Current],ClosedList),
	NewDist is ParentDist+1,
	ClosedDist > NewDist,
	removeFromList(Safe,Current,ClosedList,ClosedListWithoutCurrent),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedListWithoutCurrent,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	calculateHeuristic(Current,Destination,H,Safe),
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	member([Safe,_,_,_,Current],ClosedList),
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewOpen = NewOpenTail.
%When it is in neither of the lists.
filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,[Current|Tail],OpenList,ClosedList,NewOpen,NewClosed) :-
	withinBounds(Current),safe(Current,Safe),CurrentParent \= Current,
	filterNextMovesV2(Safe,ParentDist,CurrentParent,Destination,Tail,OpenList,ClosedList,NewOpenTail,NewClosedTail),
	NewClosed = NewClosedTail,
	NewDist is ParentDist+1,
	calculateHeuristicV2(Current,Destination,H,Safe),
	F is NewDist+H,
	NewOpen = [[Safe,F,NewDist,CurrentParent,Current]|NewOpenTail].

pathAStarV2([[Safe,H,Dist,Parent,Destination]|_],ClosedList,Destination,[[Safe,H,Dist,Parent,Destination]|ClosedList]).


%When a node is not safe
pathAStarV2([[2,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	withinBounds((X,Y)),safe((X,Y),2),(X,Y)\=Destination,
	\+ doctor((X,Y)), \+ mask((X,Y)),
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	filterNextMovesV2(2,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	sort(FilteredOpenList,NewOpenList),
	pathAStarV2(NewOpenList,[[2,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).

%when the node is a doctor or a mask
pathAStarV2([[2,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	withinBounds((X,Y)),safe((X,Y),2),(X,Y)\=Destination,
	(doctor((X,Y));mask((X,Y))),
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	filterNextMovesV2(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	sort(FilteredOpenList,NewOpenList),
	pathAStarV2(NewOpenList,[[2,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).

%When it is already safe
pathAStarV2([[1,H,Dist,Parent,(X,Y)]|OpenList],ClosedList,Destination,Result) :-
	withinBounds((X,Y)),(X,Y)\=Destination,
	XplusOne is X+1,
	XminusOne is X-1,
	YplusOne is Y+1,
	YminusOne is Y-1,
	Neighbors = [(XplusOne,Y),(XminusOne,Y),(XplusOne,YplusOne),(XplusOne,YminusOne),
					(X,YplusOne),(X,YminusOne),(XminusOne,YplusOne),(XminusOne,YminusOne)],

	filterNextMovesV2(1,Dist,(X,Y),Destination,Neighbors,OpenList,ClosedList,FilteredOpenList,FilteredClosedList),
	
	sort(FilteredOpenList,NewOpenList),
	pathAStarV2(NewOpenList,[[1,H,Dist,Parent,(X,Y)]|FilteredClosedList],Destination,Result).


searchAStarV2(Start,Destination) :-
	calculateHeuristicV2(Start,Destination,H,0),
	pathAStarV2([[2,H,0,nil,Start]],[],Destination,List),
	safeBit(Destination,List,DestinationSafeBit),
	constructPathAStar(DestinationSafeBit,Destination,Start,List,ReversedPath),
	reverse(ReversedPath,Path),
	length(Path,L),
	Steps is L-1,
	writeln("Outcome : Win."),
	write("Number of steps : "), writeln(Steps),
	write("Path : "),writeln(Path).

searchAStarV2(_,_) :-
	writeln("Outcome : Lose.").



%The following is the IO part 

coordinate((X,Y)) :- integer(X),integer(Y),withinBounds((X,Y)).

%reading N covid coordinates
inputCovid(0).
inputCovid(N) :-
	writeln("Enter the coordinates of the next COVID cell in the format : (X,Y)."),
	read(C), coordinate(C),
	assert(covid(C)),
	NewN is N-1,
	inputCovid(NewN).
	

%reading input 
input:-
	writeln("IMPORTANT: END EACH INPUT LINE WITH . "),
	writeln("How many COVID agents do you want on the map?"),
	read(N),integer(N),
	retractall(covid),
	inputCovid(N),
	writeln("Enter the coordinates of the Home cell in the format : (X,Y)."),
	read(H), coordinate(H), covidFreeV1(H),
	retractall(home),
	assert(home(H)),
	writeln("Enter the coordinates of the Doctor cell in the format : (X,Y)."),
	read(D), coordinate(D), covidFreeV1(D),
	retractall(doctor),
	assert(doctor(D)),
	writeln("Enter the coordinates of the Mask cell in the format : (X,Y)."),
	read(M), coordinate(M), covidFreeV1(M),
	retractall(mask),
	assert(mask(M))
	.

input:- 
	writeln("There was some error when you entered the input,\n please recheck the input formatting and try again. :)"),
	input.


%the random generator

blocked(Cell) :- (home(Cell);\+covidFreeV1(Cell);mask(Cell);start(Cell);doctor(Cell)).
randomCoordinate((X,Y)) :-
	maxX(UpperX),
	maxY(UpperY),
	random(0,UpperX,RandX),
	random(0,UpperY,RandY),
	((X is RandX+1, Y is RandY+1, \+ blocked((X,Y)))  ;  randomCoordinate((X,Y))).

generateCovid((X,Y)):-
	randomCoordinate((X,Y)),
	(X>2,Y>2);generateCovid((X,Y)).

%generate 2 covid cells
generate(2):-
	retractall(covid), retractall(home),retractall(mask),retractall(doctor),
	generateCovid(C1), assert(covid(C1)),
	generateCovid(C2), assert(covid(C2)),
	randomCoordinate(H), assert(home(H)),
	randomCoordinate(M), assert(mask(M)),
	randomCoordinate(D), assert(doctor(D)),
	write("Covid 1 : "), writeln(C1),
	write("Covid 2 : "), writeln(C2),
	write("Home    : "), writeln(H),
	write("Mask    : "), writeln(M),
	write("Doctor  : "), writeln(D).

%generate 1 covid 
generate(1):-
	retractall(covid), retractall(home),retractall(mask),retractall(doctor),
	randomCoordinate(C1), assert(covid(C1)),
	randomCoordinate(H), assert(home(H)),
	randomCoordinate(M), assert(mask(M)),
	randomCoordinate(D), assert(doctor(D)),
	write("Covid 1 : "), writeln(C1),
	write("Home    : "), writeln(H),
	write("Mask    : "), writeln(M),
	write("Doctor  : "), writeln(D).

%execute the code with user input 
executeInput:-
	input,
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

%execute the code with a random map with 1 covid 
executeRandom_1C:-
	generate(1),
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

%execute the code with a random map with 2 covid cells 
executeRandom_2C:-
	generate(2),
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
	



