:- begin_tests(typeInf).
:- include(typeInf).
:- dynamic gvar/2.

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct




%My Custom tests - Infer
% 1. if statement
test(infer_if_t, [nondet]):-
    infer([ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] )], unit), %typeStatement( ifStmnt( iLessThen(int, int), T, X, imult(int, int) ), unit).
    assertion(T == int).

% 2. for loop test
test(infer_for, [nondet]):-
    infer([forStmnt(int, iLessThen(int, int), Iter, T, [fplus(float, float)])], unit), %typeStatement( forStmnt(int, iLessThen(int, int), Iter, T, fplus(float, float)), unit ).
    assertion(T == float).

% 3. Function
test(infer_function, [nondet]) :-
    infer([funcDef(f, [X, Y], T, [iToFloat(iplus(X, Y))])], unit), %typeStatement(funcDef(f3, [X, Y], T, iToFloat(iplus(X, Y))), unit).
    assertion(T == float), assertion(X == int), assertion(Y == int),
    func(f, [int, int], [int, int|float]).

%4. Test a block of code with multiple lines in it in if
test(infer_if_block, [nondet]) :-
    typeStatement( ifStmnt( iLessThen(int, int), T, [imult(int, int)], [fmult(float, float), imult(int, int)] ), unit), % typeStatement( ifStmnt( iLessThen(int, int), T, [iplus(int, int)], [fmult(float, float), imult(int, int)] ), unit).
    assertion(T == int).



% 4. local variables
test(infer_lvar, [nondet]) :-
    infer([lvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    lvar(v,int).

% 5. global variables with initialization

% 6. global function with return statement

% 7.

% 8. expression computation

% 9. code blocks

% 10.

% 11.

% 12.

% 13.

% 14.

% 15.

% 16.

% 17.

% 18.

% 19.

% 20.

:-end_tests(typeInf).
