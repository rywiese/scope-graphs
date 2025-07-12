mod ScopeGraph.Phase.File {

    pub eff Error {
        def error(msg: String): Void
    }

    mod Error {
        pub def runUnsafe(prefix: String, f: Unit -> t \ ef): t \ ef - Error =
            run f() with handler Error {
                def error(msg, _) = bug!(prefix + " " + msg)
            }
    }

    pub def read(path: String): String \ IO + Error = {
        run FileRead.read(path) with runFileRead
    }

    def runFileRead(f: Unit -> t \ ef): t \ ef - FileRead + Error + IO =
        match run f() with FileRead.runWithIO {
            case Err(msg) => Error.error(ToString.toString(msg))
            case Ok(v) => v
        }

}
