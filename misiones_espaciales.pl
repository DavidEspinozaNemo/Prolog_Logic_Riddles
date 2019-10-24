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
 % mes(1,enero) => 1, mes(2,febrero) => 2
 % mes(3,marzo) => 3, mes(4,abril) => 4
structure(misiones,[mision(1,_,_),
                    mision(2,_,_),
                    mision(3,_,_),
                    mision(4,_,_)]
          ).

clues(
      misiones,
      Misiones,
      [ %este es el orden que creo mejor para el backtraking
        pistas([5,4,3,2,1,6],Misiones)
      ]
      ).

%Aqui es donde buscamos la respuesta
queries(
        misiones,
        Misiones,
        [
          member(mision(NumeroMes,geraldine,MisionEnLaQueFue),Misiones)
        ],
        [
          ['Geraldine fue en la mision', MisionEnLaQueFue, ' en el mes ', NumeroMes ]
        ]
        ).

pistas([],_).
pistas([P|Ps],E) :- pista(P,E), pistas(Ps,E).

%pistas que tengo que hacer
% 1. La persona asignada a la mision PR-97 sa lanzara en algun momento antes
%    que la persona asignada a la mision CR-260.
pista(1,E) :- mesValido(NoMes1,M1),misionEspacial('PR-97',M1),select(M1,E,E2),
              mesValido(NoMes2,M2),misionEspacial('CR-260',M2),member(M2,E2),
              NoMes1 < NoMes2.

% 2. Del astronauta asignado a la misión TV-412 y la persona asignada a la
%    misión CR-260, uno es Isaac y el otro se lanzará en abril.
%   Isacc es el astronauta de 'TV-412' o de 'CR-260' y
%   abril es el mes de despege de 'TV-412' o de 'CR-260'.

% Isacc es el astronauta de 'TV-412'
pista(2,E) :- astronauta(isaac,M1),misionEspacial('TV-412',M1),select(M1,E,E2),
              mesValido(4,M2),misionEspacial('CR-260',M2),member(M2,E2).
% Isacc es el astronauta de 'CR-260'
pista(2,E) :- astronauta(isaac,M1),misionEspacial('CR-260',M1),select(M1,E,E2),
              mesValido(4,M2),misionEspacial('TV-412',M2),member(M2,E2).

% 3. La persona asignada a la misión TV-412 se lanzará 1 mes después de Francis.
pista(3, E) :- mesValido(NoMes1,M1),misionEspacial('TV-412',M1),select(M1,E,E2),
               mesValido(NoMes2,M2),astronauta(francis,M2),member(M2,E2),
               NoMes1 is NoMes2 + 1.


% 4. Patti se lanzará 2 meses después del graduado asignado a la misión PR-97.
pista(4, E) :- mesValido(NoMes1,M1),astronauta(patti,M1),select(M1,E,E2),
               mesValido(NoMes2,M2),misionEspacial('PR-97',M2),member(M2,E2),
               NoMes1 is NoMes2 + 2.

% 5. La persona que se lanza en enero es Isaac o el astronauta asignado a la misión AV-435.
%    Isacc se lanza en enero.
pista(5, E) :- astronauta(isaac,M),mesValido(1,M),member(M,E).
%    La mision misión AV-435 se lanza en enero.
pista(5, E) :- misionEspacial('AV-435',M),mesValido(1,M),member(M,E).
%
pista(6, E) :- misionEspacial('TV-412',M1),select(M1,E,E2),
               misionEspacial('AV-435',M2),select(M2,E2,E3),
               misionEspacial('CR-260',M3),select(M3,E3,E4),
               misionEspacial('PR-97',M4),select(M4,E4,_).
%Son las estructuras de las Misiones espaciales. (asertijo No1)
mesValido(No,mision(No,_,_)).
astronauta(As,mision(_,As,_)).
misionEspacial(ME,mision(_,_,ME)).
