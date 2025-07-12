mod ScopeGraph.File {
    use ScopeGraph.Ast
    use ScopeGraph.Phases.Parser
    use ScopeGraph.Phases.Sanitizer
    use ScopeGraph.Phases.Lexer
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    /// Errors crash unsafely.
    pub def checkFile(path: String): Unit \ Parser.Error + Sanitizer.Error + Lexer.Error + FileRead + IO = region rc {
        let input = FileRead.read(path);
        let lexed = input |> StringCursor.mk(rc) |> Lexer.lex;
        println("Lexed successfully");
        let program = lexed |> VectorCursor.mk(rc) |> Parser.parseProgram;
        println("parsed successfully");
        Sanitizer.sanitize(program);
        println("Sanitized successfully")
    }

    pub def checkFileUnsafe(path: String): Unit \ IO =
        run checkFile(path)
        with handler Lexer.Error {
            def error(msg, _) =
                bug!("Lexer error: ${msg}")
        }
        with handler Parser.Error {
            def error(msg, _) =
                bug!("Parser error: ${msg}")
        } with handler Sanitizer.Error {
            def error(msg, _) =
                bug!("Sanitizer error: ${msg}")
        } with runFileRead

    def runFileRead(f: Unit -> t \ ef): t \ ef - FileRead + IO =
        match run f() with FileRead.runWithIO {
            case Err(msg) => bug!(ToString.toString(msg))
            case Ok(v) => v
        }
}
