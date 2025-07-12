# scope-graphs

Use [Main](src/Main.flix) to `compile` and `compileFile` to run all phases.

See [Examples](examples/) for a view of the language.

### Asts
- [AST](src/Ast/ParsedAst.flix) contains the Parsed AST.

### Phases
- [File](src/Phase/File.flix) reads files (`String`).
- [Lexer](src/Phase/Lexer.flix) tokenizes strings (`String -> Vector[Token]`).
- [Parser](src/Phase/Parser.flix) parses tokens (`Vector[Token] -> ParsedAst.Scope`).
- [Sanitizer](src/Phase/Sanitizer.flix) sanitizes the parsed output (`ParsedAst.Scope -> Unit`).
