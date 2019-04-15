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
    infer([ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] )], unit),
    assertion(T == int), assertion(X == int).

% 2. Fail since both branches of if do not have same type
test(infer_if_t, [fail]):-
    infer([ifStmnt( iLessThen(int, int), T, [fmult(float, float)], [imult(int, int)] )], unit).

% 3. for loop test
test(infer_for, [nondet]):-
    typeStatement(gvLet(a, T_init, iplus(int, int)), unit), %init var
    typeStatement( forStmnt(a, iLessThen(int, int), iplus(int, int), T, [fplus(float, float)]), unit ),
    assertion(T == float), assertion(T_init == int).

% 4. fail because boolean type not being passed as second arg
test(infer_for, [fail]):-
    typeStatement(gvLet(a, T_init, iplus(int, int)), unit), %init var
    typeStatement( forStmnt(a, fplus(float, float), iplus(int, int), T, [fplus(float, float)]), unit ).

% 5. fail because var passed as first arg and iter passed as 3rd arg are different types
test(for_iter_init_diff, [fail]):-
    typeStatement(gvLet(a, T_init, fplus(float, float)), unit), %init var
    typeStatement( forStmnt(a, fplus(float, float), iplus(int, int), T, [fplus(float, float)]), unit ).

% 6. Function
test(infer_function, [nondet]) :-
    infer([funcDef(f, [X, Y], T, [iToFloat(iplus(X, Y))])], unit),
    assertion(T == float), assertion(X == int), assertion(Y == int),
    func(f, [int, int], [int, int|float]).

%7. Test a block of code with multiple lines in it in if statement's CodeF, confirms typeCode works
test(infer_if_block, [nondet]) :-
    typeStatement( ifStmnt( iLessThen(int, int), T, [imult(int, int)], [fmult(float, float), imult(int, int)] ), unit),
    assertion(T == int).

%8. Test Code as a multiline block
test(infer_code, [nondet]):-
    infer([ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] ), gvLet(a, T_init, iplus(int, int)), forStmnt(a, iLessThen(int, int), iplus(int, int), T1, [fplus(float, float)])], unit),
    assertion(T == int), assertion(X == int), assertion(T1 == float).

% 9. Two statements can be nested
test(nest_statements, [nondet]) :-
    typeStatement(gvLet(a, int, iplus(int, int)), unit),
    typeStatement( forStmnt(a, iLessThen(int, int), iplus(int, int), T, [ ifStmnt( iLessThen(int, int), T_if, [int], [imult(int, int)] ) ]), unit ),
    assertion(T == unit), assertion(T_if == int).

% 10. local variables in expression
/* let x = int in blah blah */
test(infer_lvar, [nondet]) :-
    infer([lvLet(v, T, int, [iplus(int, int)] )], unit),
    assertion(T==int).

%TODO 11. local variables in expression should not exist outside expression
/* let x = int in blah blah then check afer - TODO Delete not working */ 
test(infer_lvar, [fail]) :-
    infer([lvLet(v, T, int, [iplus(int, int)] )], unit),
    lvar(v, int).

% 12. Code can either be [S] or it can be [[S]], represents blocks of code seperated by ;; per Dr. Dobria's insturctions in readme
test(infer_blocks_of_code, [nondet]):-
    infer([[ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] )], [gvLet(a, T_init, iplus(int, int)), forStmnt(a, iLessThen(int, int), iplus(int, int), T1, [fplus(float, float)])] ], unit),
    assertion(X == int), assertion(T == int), assertion(T1 = float).

%TODO 13. Test to make sure user defined function can be called from tpeCode block
test(infer_function, [nondet]) :-
    infer([funcDef(f, [X, Y], T, [iToFloat(iplus(X, Y))]), func(f, [int, int], [int, int|float]) ], unit), %typeStatement(funcDef(f3, [X, Y], T, iToFloat(iplus(X, Y))), unit).
    assertion(T == float), assertion(X == int), assertion(Y == int).

% 14. 
% 15.
% 16.
% 17.
% 18.
% 19.
% 20.

:-end_tests(typeInf).
