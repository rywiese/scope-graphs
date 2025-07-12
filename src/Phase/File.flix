mod ScopeGraph.Phase.File {
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Error

    pub def read(path: String): String \ IO + Crash = {
        run FileRead.read(path) with runFileRead
    }

    def runFileRead(f: Unit -> t \ ef): t \ ef - FileRead + Crash + IO =
        match run f() with FileRead.runWithIO {
            case Err(msg) =>
                Crash.crash(Error.mkUnknownError(category = "Reader", ToString.toString(msg)))
            case Ok(v) => v
        }

}
