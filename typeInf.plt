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
    assertion(T == int), assertion(X == int).

% 2. Fail since both branches of if do not have same type
test(infer_if_t, [fail]):-
    infer([ifStmnt( iLessThen(int, int), T, [fmult(float, float)], [imult(int, int)] )], unit). %typeStatement( ifStmnt( iLessThen(int, int), T, fmult(float, float), imult(int, int) ), unit).

% 3. for loop test
test(infer_for, [nondet]):-
    infer([forStmnt(int, iLessThen(int, int), Iter, T, [fplus(float, float)])], unit), %typeStatement( forStmnt(int, iLessThen(int, int), Iter, T, fplus(float, float)), unit ).
    assertion(T == float).

% 4. fail because boolean type not being passed as second arg
test(infer_for, [fail]):-
    infer([forStmnt(int, fplus(float, float), Iter, T, [fplus(float, float)])], unit).

% 5. Function
test(infer_function, [nondet]) :-
    infer([funcDef(f, [X, Y], T, [iToFloat(iplus(X, Y))])], unit), %typeStatement(funcDef(f3, [X, Y], T, iToFloat(iplus(X, Y))), unit).
    assertion(T == float), assertion(X == int), assertion(Y == int),
    func(f, [int, int], [int, int|float]).

%6. Test a block of code with multiple lines in it in if statement's CodeF, confirms typeCode works
test(infer_if_block, [nondet]) :-
    typeStatement( ifStmnt( iLessThen(int, int), T, [imult(int, int)], [fmult(float, float), imult(int, int)] ), unit), % typeStatement( ifStmnt( iLessThen(int, int), T, [iplus(int, int)], [fmult(float, float), imult(int, int)] ), unit).
    assertion(T == int).

%7. Test Code as a multiline block
test(infer_code, [nondet]):-
    infer([ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] ), forStmnt(int, iLessThen(int, int), Iter, T1, [fplus(float, float)])], unit), %typeStatement( forStmnt(int, iLessThen(int, int), Iter, T, fplus(float, float)), unit ).
    assertion(T == int), assertion(X == int), assertion(T1 == float).

%8. Code can either be [S] or it can be [[S]], represents blocks of code seperated by ;; per Dr. Dobria's insturctions in readme
test(infer_blocks_of_code):-
    infer([[ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] )], [forStmnt(int, iLessThen(int, int), Iter, T1, [fplus(float, float)])] ], unit),
    assertion(X == int), assertion(T == int), assertion(T1 = float).

% 9. local variables
/* */
test(infer_lvar, [nondet]) :-
    infer([lvLet(v, T, int, iplus( typeExp(lvar(v), T), int ))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int).



:-end_tests(typeInf).
