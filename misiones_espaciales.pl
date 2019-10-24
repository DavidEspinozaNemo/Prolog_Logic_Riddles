% ======== Algortimo basico del profesor
test_puzzle(Name,puzzle(Structure,Clues,Queries,Solution)):-
   structure(Name,Structure),
   clues(Name,Structure,Clues),
   queries(Name,Structure,Queries,Solution).

solve_puzzle(puzzle(Structure,Clues,Queries,Solution),Solution):-
   solve(Clues),solve(Queries).

solve([Clue|Clues]):-Clue,solve(Clues).
solve([]).

mostrar([]).
mostrar([C|Cs]) :- writeln(C),mostrar(Cs).

resolver(Acertijo, Structure, Solucion) :-
         test_puzzle(Acertijo,Puzzle),    % .
         solve_puzzle(Puzzle,Solucion),   % .
         Puzzle=puzzle(Structure,_,_,_),  % .
         mostrar(Structure),              % .
         mostrar(Solucion).

 %modificacion para el problema de la misiones espaciales
structure(misiones,[mision(mes(1,enero),_,_),
                    mision(mes(2,febrero),_,_),
                    mision(mes(3,marzo),_,_),
                    mision(mes(4,abril),_,_)]
          ).

clues(
      misiones,
      Misiones,
      [
        pistas([1,2,3,4,5],Misiones)
      ]
      ).

%Aqui es donde buscamos la respuesta
queries(
        misiones,
        Misiones,
        [
          member(mision(mes(NumeroMes,NombreDelMes),geraldine,MisionEnLaQueFue))
        ],
        [
          ['Geraldine fue en la mision', MisionEnLaQueFue, ' en ', NombreDelMes ]
        ]
        ).

pistas([],_).
pistas([P|Ps],E) :- pista(P,E), pistas(Ps,E).

%pistas que tengo que hacer


%Son las estructuras de las Misiones espaciales. (asertijo No1)
mesValido(mes(No,Mes),mision(mes(No,Mes),_,_)).
astronauta(As,mision(_,As,_)).
misionEspacial(ME,mision(_,_,ME)).
