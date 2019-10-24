% caso 2. juegos de ajedrez
% ======== Algortimo basico del profesor
test_puzzle(Name,puzzle(Structure,Clues,Queries,Solution)):-
   structure(Name,Structure),
   clues(Name,Structure,Clues),
   queries(Name,Structure,Queries,Solution).

solve_puzzle(puzzle(Structure, Clues,Queries,Solution),Solution):-
  solve(Clues),solve(Queries).

solve([Clue|Clues]):-Clue,solve(Clues).
solve([]).

mostrar([]).
mostrar([C|Cs]) :- writeln(C),mostrar(Cs).

resolver(Acertijo, Structure, Solucion) :-
         test_puzzle(Acertijo,Puzzle),
         solve_puzzle(Puzzle,Solucion),
         Puzzle=puzzle(Structure,_,_,_),
         mostrar(Structure),
         mostrar(Solucion).

%========= Relaciones espec�ficas para un acertijo dado =========
structure(juegos_ajedrez, []
         ).

clues(
       juegos_ajedrez,
       Juegos_Ajedrez,
       [
        pistas([],Juegos_Ajedrez)
       ]
     ).

queries(
       juegos_ajedrez,
       Juegos_Ajedrez,
       [
        %aqui van las preguntas
       ],
       [
        %aqui van las respuestas
       ]
       ).
% Aqui van las pistas
pistas([],_).
pistas([P|Ps],E) :- pista(P,E), pistas(Ps,E).
%==============================================
 % 1. ...

 % estructuras
 % Ejemplo = dia(D,viaje(D,_,_)).
