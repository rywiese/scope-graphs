# scope-graphs

- [AST](src/Ast.flix) contains the Parsed AST
- [Parser](src/Parsing/Parser.flix) allows parsing of strings
- [File](src/File.flix) allows "checking" files, i.e. the full pipeline
- [Sanitizer](src/Sanitizer.flix) can sanitize parsed output, doing extra checks.

## Known parsing issues
- Trailing `pub` are ignored in scopes.
