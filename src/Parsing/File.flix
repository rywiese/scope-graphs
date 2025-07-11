mod ScopeGraph.Parsing.File {
    use ScopeGraph.Ast
    use ScopeGraph.Parser
    use ScopeGraph.Parsing.StringCursor

    /// Errors crash unsafely
    pub def parseFile(path: String): Ast.Scope \ Parser.Error + FileRead = region rc {
        let input = FileRead.read(path);
        let sc = StringCursor.mk(rc, input);
        Parser.parseScope(sc)
    }

    pub def parseFileUnsafe(path: String): Ast.Scope \ IO =
        run parseFile(path)
        with handler Parser.Error {
                def error(msg, _) =
                    bug!("parse error: ${msg}")
            }
        with runFileRead

    def runFileRead(f: Unit -> t \ ef): t \ ef - FileRead + IO =
        match run f() with FileRead.runWithIO {
            case Err(msg) => bug!(ToString.toString(msg))
            case Ok(v) => v
        }
}
