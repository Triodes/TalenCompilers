Task 1

- Added appropriate token types (ConstBool, ConstChar)
- Added matching lexer functions based on lexConstInt
- Added matching SSM instructions to generator function fExprCon

Task 2

- Extended the pExpr function with a foldr over a list of lists of operators sorted by priority (inspired by the lecture notes)
- The fold function opLayer uses chainl to create an operator parser tree with the right precedence

Task 7

- Added comment lexer lexComments
- Combined lexComments and lexWhiteSpace into lexIgnore
- Replaced lexWhiteSpace with lexIgnore in lexicalScanner