# scope-graphs

<!-- Requires the jar of https://github.com/flix/flix/actions/runs/16250297056 -->

Use [Main](src/Main.flix) to `compile` and `compileFile` to run all phases.

See [Examples](examples/) for a view of the language.

### Asts
- [AST](src/Ast/ParsedAst.flix) contains the Parsed AST.

### Phases
- [File](src/Phase/File.flix) reads files (`String`).
- [Lexer](src/Phase/Lexer.flix) tokenizes strings (`String -> Vector[Token]`).
- [Parser](src/Phase/Parser.flix) parses tokens (`Vector[Token] -> ParsedAst.Scope`).
- [Sanitizer](src/Phase/Sanitizer.flix) sanitizes the parsed output (`ParsedAst.Scope -> Unit`).

## Using the vsix extension
```
flix> :build
flix> :fatjar
```
install the `.vsix` extension

## Build Extension
```
cd extension/flixsg
npm install
vsce package
```
and then copy the .vsix file down
