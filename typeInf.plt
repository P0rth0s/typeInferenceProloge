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
    assertion(T == [int, int | float]), assertion(X == int), assertion(Y == int),
    gvar(f, [int, int, float]).

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

%TODO 13. Test to make sure user defined function can be called from same infer
test(infer_function, [nondet]) :-
    infer([funcDef(f6, [X, Y], T, [ iToFloat(iplus(X, Y)) ]), f6(X, Y)], unit),
    assertion(T == float), assertion(X == int), assertion(Y == int).

% 14. Test nested function definitions
test(nested_function_definition, [nondet]) :-
    typeStatement(funcDef(f, [int, int], [int, int, float], [ iToFloat(iplus(int, int)) ]), unit),
    typeStatement(funcDef(f1, [int, int], T1, [ f(X, Y) ]), unit),
    assertion(X == int), assertion(Y == int), assertion(T1 == [int, int | float]),
    gvar(f, N).

% 15. function with statement
test(function_with_statement, [nondet]) :-
    infer([funcDef(f, [string, string], T, [ ifStmnt( compareStrings(string, string), T1, [string], [string]) ])], unit),
    assertion(T == [string, string| unit]), assertion(T1 == string).

% 16. Cant use an undefined function
test(nested_function_definition_undef, [fail]) :-
    typeStatement(funcDef(a1, [int, int], T1, [ a(X, Y) ]), unit),
    typeStatement(funcDef(a, [int, int], [int, int, float], [ iToFloat(iplus(int, int)) ]), unit),
    assertion(X == int), assertion(Y == int), assertion(T1 == [int, int, float]),
    gvar(f, N).

% 17. Goes with test 12 Make sure when we pass to infer as [[S]] S can be S | ST
test(infer_blocks_of_code, [nondet]):-
    infer([[ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)] ),  ifStmnt( iLessThen(int, int), T, [X], [imult(int, int)])     ], [gvLet(a, T_init, iplus(int, int)), forStmnt(a, iLessThen(int, int), iplus(int, int), T1, [fplus(float, float)])] ], unit),
    assertion(X == int), assertion(T == int), assertion(T1 = float).

% 18. The readme specifically requested explicit initialization so that is demonstrated here
test(explicit_var_init, [nondet]) :-
    infer([gvLet(v, T, int)], unit),
    assertion(T==int),
    gvar(v,int).

% 19. Function called within statement
test(call_function_in_statement, [nondet]) :-
    typeStatement(funcDef(f, [int, int], [int, int, float], [ iToFloat(iplus(int, int)) ]), unit),
    typeStatement(ifStmnt( iLessThen(int, int), T, [X], [imult(int, fToInt(f(int, int)))] ), unit),
    assertion(X == int), assertion(T == int).

% 20. Function def should be able to point towards list
test( function_def_list, [nondet]) :-
    typeStatement(funcDef(f, [int, int], X, [ iToFloat(iplus(int, int)), imult(int, int) ]), unit),
    assertion(X == [int, int | int]).
:-end_tests(typeInf).
