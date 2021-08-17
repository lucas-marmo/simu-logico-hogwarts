%%%%%%%%%%%%%%%%%%%%%%%%%% PARTE 1 %%%%%%%%%%%%%%%%%%%%%%%%%%
%mago(Nombre,Sangre,Caracteristicas).
mago(harry,mestiza,[coraje,amistad,orgullo,inteligencia]).
mago(draco,pura,[inteligencia,orgullo]).
mago(hermione,impura,[inteligencia,orgullo,responsabilidad]).
mago(roberto,pura,[inteligencia,orgullo,responsabilidad,amistad]).

%odiaria(Mago,Casa).
odiaria(harry,slytherin).
odiaria(draco,hufflepuff).

%importantes(Casa,Caracteristicas). --> Con listas
importantes(gryffindor,[coraje]).
importantes(slytherin,[orgullo,inteligencia]).
importantes(ravenclaw,[inteligencia,responsabilidad]).
importantes(hufflepuff,[amistad]).
%importante(Casa,Caracteristica). --> Sin listas
importante(gryffindor,coraje).
importante(slytherin,orgullo).
importante(slytherin,inteligencia).
importante(ravenclaw,inteligencia).
importante(ravenclaw,responsabilidad).
importante(hufflepuff,amistad).

casa(Casa):-
    importante(Casa,_).

%%% PUNTO 1
permiteEntrar(Casa,Mago):-
    mago(Mago,_,_),
    casa(Casa),
    Casa\=slytherin.
permiteEntrar(slytherin,Mago):-
    mago(Mago,_,_),
    not(mago(Mago,impura,_)).

%%% PUNTO 2
caracterApropiado(Mago,Casa):-
    mago(Mago,_,Caracteristicas),
    importantes(Casa,Importantes),
    forall(member(Caracteristica,Importantes),member(Caracteristica,Caracteristicas)).

%%% PUNTO 3
puedeQuedar(Mago,Casa):-
    permiteEntrar(Casa,Mago),
    caracterApropiado(Mago,Casa),
    not(odiaria(Mago,Casa)).
puedeQuedar(hermione,gryffindor).

%%% PUNTO 4
cadenaDeAmistades(Magos):-
    forall(member(Mago,Magos),esAmistoso(Mago)),
    podrianTodosEstarEnLaMismaCasaQueElSiguiente(Magos).

esAmistoso(Mago):-
    mago(Mago,_,Caracteristicas),
    member(amistad,Caracteristicas).

primerElemento(Mago1,[Mago1|_]).

podrianTodosEstarEnLaMismaCasaQueElSiguiente([Mago1|Magos]):-
    primerElemento(Mago2,Magos),
    puedeQuedar(Mago1,Casa),
    puedeQuedar(Mago2,Casa),
    Mago1\=Mago2,
    recursividad(Magos).

recursividad([Mago2|Magos]):-
    podrianTodosEstarEnLaMismaCasaQueElSiguiente([Mago2|Magos]).

recursividad([_]).

%%%%%%%%%%%%%%%%%%%%%%%%%% PARTE 2 %%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous accionRealizada/2.
:- discontiguous puntaje/2.

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

%lugarProhibido(Lugar,Puntos).
lugarProhibido(bosque,-50).
lugarProhibido(biblioteca,-10).
lugarProhibido(tercerPiso,-75).

%accion(Accion,Puntaje)
puntaje(fueraDeCama,-50).
puntaje(irALugar(Lugar),Puntaje):-
    lugarProhibido(Lugar,Puntaje).
puntaje(buenaAccion(Puntaje,_),Puntaje).

malaAccion(Accion):-
    puntaje(Accion,Puntaje),
    Puntaje<0.

accionRealizada(harry,fueraDeCama).
accionRealizada(harry,irALugar(bosque)).
accionRealizada(harry,irALugar(tercerPiso)).
accionRealizada(hermione,irALugar(tercerPiso)).
accionRealizada(hermione,irALugar(biblioteca)).
accionRealizada(draco,irALugar(mazmorras)).
accionRealizada(ron,buenaAccion(50,ganarPartidaAjedrez)).
accionRealizada(hermione,buenaAccion(50,salvarAmigos)).
accionRealizada(harry,buenaAccion(60,ganarleAVoldermort)).

%%% PUNTO 1
% 1a
buenAlumno(Mago):-
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).

hizoAlgunaAccion(Mago):-
    accionRealizada(Mago,_).

hizoAlgoMalo(Mago):-
    accionRealizada(Mago,Accion),
    malaAccion(Accion).

% 1b
accionRecurrente(Accion):-
    accionRealizada(UnMago,Accion),
    accionRealizada(OtroMago,Accion),
    UnMago\=OtroMago.

%%% PUNTO 2
puntajeCasa(Casa,PuntajeTotal):-
    esDe(_,Casa),
    findall(Puntaje,puntajeDeMiembro(_,Puntaje,Casa),Puntajes),
    sum_list(Puntajes, PuntajeTotal).

puntajeDeMiembro(Mago,PuntajeMiembro,Casa):-
    esDe(Mago,Casa),
    findall(Puntaje,puntajeDeAccion(_,Mago,Puntaje),Puntajes),
    sum_list(Puntajes,PuntajeMiembro).

puntajeDeAccion(Accion,Mago,Puntaje):-
    accionRealizada(Mago,Accion),
    puntaje(Accion,Puntaje).

%%% PUNTO 3
casaGanadora(Casa):-
    puntajeCasa(Casa,PuntajeMaximo),
    forall(puntajeCasa(_,OtroPuntaje),PuntajeMaximo>=OtroPuntaje).

%%% PUNTO 4

accionRealizada(hermione,respondioPregunta(dondeSeEncuentraBezoar,20,snape)).
accionRealizada(hermione,respondioPregunta(comoHacerLevitarPluma,25,flitwick)).
puntaje(respondioPregunta(_,Dificultad,Profesor),Dificultad):-
    Profesor\=snape.
puntaje(respondioPregunta(_,Dificultad,snape),Puntaje):-
    is(Puntaje,Dificultad/2).
