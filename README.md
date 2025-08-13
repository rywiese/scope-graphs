# scope-graphs

A scope graph is an approach for name resolution in a compiler front-end.
With this approach, the parsed AST is extended with
1. nodes representing scopes
2. edges representing relationships between scopes (lexical parent/child relationships, imports, declarations within scopes)

This reduces name resolution to finding paths from identifiers to AST nodes, and provides a mechanism for formalizing scoping/shadowing rules. There are some papers on this [here](https://pl.ewi.tudelft.nl/research/projects/scope-graphs/).

This project is a small compiler frontend that parses and builds a scope graph for FlixSG, a subset of the [Flix](https://flix.dev/) programming language.

<!-- Requires the jar of https://github.com/flix/flix/actions/runs/16250297056 -->

Use [Main](src/Main.flix) to `compile` and `compileFile` to run all phases.

See [Examples](examples/) for a view of the language. With the vsix extension installed and the language server running, you can ctrl/cmd + click on identifiers to see how they are resolved.

### Asts
- [ParsedAst](src/Ast/ParsedAst.flix) contains the Parsed AST.
- [ScopedAst](src/Ast/ParsedAst.flix) contains the Scoped AST, as well as the scope nodes and edges.

### Phases
- [File](src/Phase/File.flix) reads files (`String`).
- [Lexer](src/Phase/Lexer.flix) tokenizes strings (`String -> Vector[Token]`).
- [Parser](src/Phase/Parser.flix) parses tokens (`Vector[Token] -> ParsedAst.Scope`).
- [Sanitizer](src/Phase/Sanitizer.flix) sanitizes the parsed output (`ParsedAst.Scope -> Unit`).
- [Scoper](src/Phase/Scoper.flix) builds a scope graph from the parsed AST (`ParsedAst -> ScopeGraph`).

### Graph
- [ScopeGraph](src/Ast/ScopeGraph.flix) contains functions for resolving names by searching for paths in the graph.

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
