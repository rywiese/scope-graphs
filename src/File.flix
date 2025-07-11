mod ScopeGraph.File {
    use ScopeGraph.Sanitizer
    use ScopeGraph.Ast
    use ScopeGraph.Parser
    use ScopeGraph.Parsing.StringCursor

    /// Errors crash unsafely
    pub def checkFile(path: String): Unit \ Parser.Error + Sanitizer.Error + FileRead + IO = region rc {
        let input = FileRead.read(path);
        let sc = StringCursor.mk(rc, input);
        let program = Parser.parseProgram(sc);
        println("parsed successfully");
        Sanitizer.sanitize(program);
        println("Sanitized successfully")
    }

    pub def checkFileUnsafe(path: String): Unit \ IO =
        run checkFile(path)
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
