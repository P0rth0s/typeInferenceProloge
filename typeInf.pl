:- dynamic gvar/2.

/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

typeStatement(ifStmnt(B, T, CodeT, CodeF), unit) :-
    typeExp(B, bool),
    typeCode(CodeT, T),
    typeCode(CodeF, T),
    bType(T).

typeStatement(forStmnt(VarName, B, Iter, T, Code), unit) :-
    gvar(VarName, T1),
    typeExp(Iter, T1),
    bType(T1),
    typeExp(B, bool),
    typeCode(Code, T),
    bType(T).

%this allows us to pass statements into functions
typeStatement(funcDef(Name, Params, NewT, Code), unit):-
    atom(Name),
    is_list(Params),
    typeCode(Code, T),
    append(Params, T, NewT),
    asserta(gvar(Name, NewT)),
    !. %Stop so dont match Code with expList below

typeStatement(funcDef(Name, Params, NewT, Code), unit):-
    atom(Name),
    is_list(Params),
    typeExpList(Code, T),
    append(Params, T, NewT),
    asserta(gvar(Name, NewT)).

/* 
pairs_keys_values([X, Y], [a, v], [Z, Z2]).
X = a-Z,
Y = v-Z2.
*/
typeStatement(lvLet(Name, T, Code, In, Local), unit):-
    atom(Name),
    typeExp(Code, T),
    bType(T),
    pairs_keys_values([X], [Name], [T]), %get local variable pair
    append(X, Local, Local1),
    typeCode(In, T1, Local1).

%lvar attempt
typeStatement(lvLet(Name, T, Code, In), unit):-
    atom(Name),
    typeExp(Code, T),
    bType(T),
    asserta(lvar(Name, T)),
    typeCode(In, T1),
    deleteLVars().

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([S], T):-typeExp(S, T).
typeCode([S, S2|Code], T):-
    typeExp(S,_T),
    typeCode([S2|Code], T).


typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* Code can either be ([S], T) or ([[S]], T) to represent we can have multiple blocks of code  - maybe get rid of*/
typeCode([S], T):- typeCode(S, T).
typeCode([S | S2], T) :-
    typeCode(S, _T),
    typeCode(S2, T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    deleteFunctions(),
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(bool).
bType(int).
bType(float).
bType(string).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteLVars():-retractall(lvar), asserta(lvar(_X,_Y):-false()).

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

deleteFunctions():-retractall(func), asserta(func(_X,_Y, _Z):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(iplus, [int,int,int]).
fType(imult, [int, int, int]).
fType(fmult, [float, float, float]).
fType(fplus, [float, float, float]).
fType(fdiv, [float, float, float]).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).

fType(fLessThen, [float, float, bool]).
fType(iLessThen, [int, int, bool]).
fType(fGreaterThen, [float, float, bool]).
fType(iGreaterThen, [int, int, bool]).
fType(fEqualTo, [float, float, bool]).
fType(iEqualTo, [int, int, bool]).

fType(compareStrings, [string, string, bool]).

fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
% gvar(_, _) :- false(). replaced with making dynamic
