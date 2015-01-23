Aron List, 3896536
Jelle Hol, 3760685

Task 3
From the list of parameter declarations we constructed an envirionment mapping variable names to offsets from the mark pointer. We set the mark pointer to the begin of the method with link. We added an ExprCall to the grammar and an apropriate parser. We modified the fold functions in CSharpCode.hs to cope with an environment.

Task 4
We added a simple patter match on the method name in fExprCall and changed the generated code so it would eval all the expressions and call TRAP 0 afterwards.

Task 5
As the return functions allready put the value of the expression after the return statement on the stack we only needed to store it in a register and have the caller load it from that register. The POP throwing away the result of statements when we're done with them will clen it up nicely

Task 6
Changed the return type of statements and expressions to a tuple of code and list of declared variables. in fMembMeth we construct a local env from the param env and the env with the local vars.

Task 8
added a post-processing function that transforms expressions with a compuund assignment to the fully unfolded ones. i.e. x += 5 becomes x = x + 5 in the expression tree.

Task 12
changed the environment to a tuple of local and global variable environments. in fClass we construct the global env, create a pointer in R4 to the variable space and reserve this space.