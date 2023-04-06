:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(State) :- State = [initial,
[['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
				  ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''],
				  ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', '']]].

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards([_, B], Boards) :- B = Boards.

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard([_, B], UPos, Board) :- positions(X), nth0(I, X, UPos, _), nth0(I, B, Board, _).
                                       


% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getUBoard([_, B], UB) :- maplist(getBoardResult, B, UB).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos([_, B], UPos, Pos, Cell) :- 
    positions(X), nth0(I, X, UPos, _), nth0(I, B, Board, _), nth0(J, X, Pos, _), nth0(J, Board, Cell, _).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(X), nth0(I, X, Pos, _), nth0(I, Board, Cell, _).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0).


countMoves([], _, 0).
countMoves([H|T], H, N) :- countMoves(T, H, N1), N is N1 + 1, !.
countMoves([_|T], H, N) :- countMoves(T, H, N), !.

countAllMoves([], _, 0).
countAllMoves([H|T], X, N) :- countMoves(H, X, HN), countAllMoves(T, X, TN), N is HN + TN.


getNextPlayer([_, B], NextPlayer) :- countAllMoves(B, x, N), countAllMoves(B, 0, M), (N > M -> NextPlayer = 0; NextPlayer = x).


% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
% getNextAvailableBoards obține tablele individuale (ca poziții în tabla de UTTT) disponibile pentru următoarea mutare.
% în starea inițială, este întreaga listă de poziții (jucătorul poate muta în orice tablă), deci pentru initialState(S0), getNextAvailableBoards(S0, Boards), Boards trebuie legat la lista completă de poziții: [nw, n, ne, w, c, e, sw, s, se];
% de obicei, este o listă conținând o singură poziție, aceeași cu poziția dintr-o tablă individuală unde a mutat jucătorul precedent. În exemplul de mai sus, cum jucătorul 0 tocmai a mutat în poziția w a tablei n, jucătorul x trebuie sa mute obligatoriu în tabla w, deci getNextAvailableBoards(State, Boards) trebuie să lege Boards la lista [w];
% atunci când jucătorul precedent a mutat într-o poziție care corespunde unei table individuale în care jocul s a terminat, sunt disponibile pentru următoarea poziție toate tablele care nu sunt încă finalizate (nu au fost câștigate sau remizate). 
% Dacă în exemplul de mai sus x mută în centrul tablei w, pentru următoarea mutare, a lui 0 vor fi disponibile tablele n, ne, w, e, sw, s

findallRes([PrevMove, B], Res) :- 
    positions(X), X \= PrevMove,
    findall(B1, (member(B1, X), nth0(I, X, B1, _), nth0(I, B, Board, _), getBoardResult(Board, Result), Result = ''), Res).


getNextAvailableBoards([initial, B], NextBoardPoss) :- initialState([_, Board]), B = Board, NextBoardPoss = [nw, n, ne, w, c, e, sw, s, se].
getNextAvailableBoards([PrevMove, B], NextBoardPoss) :- 
    positions(X), nth0(I, X, PrevMove, _), nth0(I, B, Board, _), 
    getBoardResult(Board, Result), (Result = '' -> NextBoardPoss = [PrevMove]; 
                                    findallRes([PrevMove, B], Res), NextBoardPoss = Res).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Result) :- ( \+ player_wins(_, Board), member('', Board) -> Result = '';
                                 (\+ member('', Board), \+ player_wins(_, Board) -> Result = r;
                                 player_wins(W, Board), Result = W)).


% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s a realizat ultima mutare.
buildState(B, PrevMove, State) :- State = [PrevMove, B].

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.

first((X, _), X).
second((_, Y), Y).

validMove([PrevMove, B], Move) :-
    getUBoard([PrevMove, B], UBoard), getBoardResult(UBoard, Result), Result = '',
    getNextAvailableBoards([PrevMove, B], NextBoardPoss), length(NextBoardPoss, 1), 
        getPos([PrevMove, B], PrevMove, Move, R), R = '';
    getUBoard([PrevMove, B], UBoard), getBoardResult(UBoard, Result), Result = '',
    getNextAvailableBoards([PrevMove, B], NextBoardPoss), length(NextBoardPoss, L), L > 1, first(Move, F), 
        getBoard([PrevMove, B], F, Board), getBoardResult(Board, Result), Result = '',
        second(Move, S),
        getPos([PrevMove, B], F, S, R), R = ''.
        


% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.

replace([_ | T], 0, X, [X | T]).
replace([H | T], I, X, [H | R]) :- I > 0, NI is I - 1, replace(T, NI, X, R).

replace([], _, _, []).
replace([H | T], I, X, [R | R2]) :- replace(H, I, X, R), replace(T, I, X, R2).


makeMove([PrevMove, B], Move, NewState) :-
    validMove([PrevMove, B], Move),
    getNextAvailableBoards([PrevMove, B], NextBoardPoss), length(NextBoardPoss, 1), 
        getBoard([PrevMove, B], PrevMove, Board),
        getNextPlayer([PrevMove, B], NextPlayer),
        positions(X), nth0(I, X, Move, _), replace(Board, I, NextPlayer, NewBoard),
        nth0(I2, X, PrevMove, _),
        replace(B, I2, NewBoard, NewUTT),
        buildState(NewUTT, Move, NewState);
    validMove([PrevMove, B], Move),
    getNextAvailableBoards([PrevMove, B], NextBoardPoss), length(NextBoardPoss, L), L > 1, first(Move, F), second(Move, S),
        getBoard([PrevMove, B], F, Board),
        getNextPlayer([PrevMove, B], NextPlayer),
        positions(X), nth0(I, X, S, _), replace(Board, I, NextPlayer, NewBoard),
        nth0(I2, X, F, _),
        replace(B, I2, NewBoard, NewUTT),
        buildState(NewUTT, S, NewState).
        

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).

filter_elem([], [], a).
filter_elem([HE | RE], [HP | RP], Result) :- (HE = '' -> Result = HP; filter_elem(RE, RP, Result)).

dummy_first([PrevMove, B], NextMove) :-
    getUBoard([PrevMove, B], UBoard),
    filter_elem(UBoard, [nw, n, ne, w, c, e, sw, s, se], Upos),
    getBoard([PrevMove, B], Upos, Board),
    filter_elem(Board, [nw, n, ne, w, c, e, sw, s, se], Pos),
    (Upos = PrevMove -> NextMove = Pos; NextMove = (Upos, Pos)).
        

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).


dummy_last([PrevMove, B], NextMove) :- 
    getUBoard([PrevMove, B], UBoard),
    reverse(UBoard, RUBoard),
    filter_elem(RUBoard, [se, s, sw, e, c, w, ne, n, nw], Upos),
    getBoard([PrevMove, B], Upos, Board),
    reverse(Board, RBoard),
    filter_elem(RBoard, [se, s, sw, e, c, w, ne, n, nw], Pos),
    (Upos = PrevMove -> NextMove = Pos; NextMove = (Upos, Pos)).

% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.

makeBoardMove(Board, Player, Move, NewBoard) :-
    positions(X), nth0(I, X, Move, _), replace(Board, I, Player, NewBoard).

getNextPlayerInBoard(x, 0).
getNextPlayerInBoard(0, x).

freePositions(Board, Positions) :-
    positions(X),
    findall(Pos, (nth0(I, X, Pos, _), nth0(I, Board, '')), Positions).

playerCanWin(Board, Player) :-
    freePositions(Board, Positions),
    member(Pos, Positions),
    makeBoardMove(Board, Player, Pos, NewBoard),
    getBoardResult(NewBoard, Result),
    Player = Result.
    

movePriority(Player, Board, Move, Priority) :-
    (makeBoardMove(Board, Player, Move, NewBoard), getBoardResult(NewBoard, Result), Result = Player -> Priority = 0;
    (getNextPlayerInBoard(Player, NextPlayer), makeBoardMove(Board, NextPlayer, Move, NewBoard), getBoardResult(NewBoard, Result), Result = NextPlayer -> Priority = 1;
    (Board = ['', '', '', '', '', '', '', '', ''], member(Move, [nw,ne,sw,se]) -> Priority = 2;
    (\+ member(Player, Board), getNextPlayerInBoard(Player, NextPlayer), getPos(Board, c, Cell), Cell = NextPlayer, member(Move, [nw,ne,sw,se]) -> Priority = 3;
    (\+ member(Player, Board), getNextPlayerInBoard(Player, NextPlayer), getPos(Board, c, Cell), Cell \= NextPlayer, Move = c -> Priority = 3;
    (makeBoardMove(Board, Player, Move, NewBoard), playerCanWin(NewBoard, Result), Result = Player -> Priority = 4;
    (member(Move, [nw,ne,sw,se]) -> Priority = 5;
    Priority = 6))))))).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.
bestIndividualMoves(Player, Board, Moves) :-
    positions(X), findall((Priority, Move), (member(Move, X), getPos(Board, Move, Cell), Cell = '', movePriority(Player, Board, Move, Priority)), List),
    sortMoves(List, Moves).

head([H | _], H).

% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.

narrowGreedy([PrevMove, B], Move) :-
    getUBoard([PrevMove, B], UBoard),
    freePositions(UBoard, Positions),
    length(Positions, L),
    (L = 1 -> head(Positions, UPos), getBoard([PrevMove, B], UPos, Board),
              getNextPlayer([PrevMove, B], NextPlayer),
              bestIndividualMoves(NextPlayer, Board, Moves),
              head(Moves, Move);
    getNextPlayer([PrevMove, B], NextPlayer),
    bestIndividualMoves(NextPlayer, UBoard, UMoves),
    head(UMoves, UPos),
    getBoard([PrevMove, B], UPos, Board),
    bestIndividualMoves(NextPlayer, Board, Moves),
    head(Moves, BPos),
    (UPos = PrevMove -> Move = BPos; Move = (UPos, BPos))).

% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.
bestMoves(_, _) :- false.

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(_, _) :- false.
