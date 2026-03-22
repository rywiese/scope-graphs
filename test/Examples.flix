mod SG.Test.Examples {
    use Assert.assertTrue

    use SG.Ast.Shared.Location.mkLoc
    use SG.Ast.Shared.Location.Source
    use SG.Ast.Shared.Location.Located
    use SG.Ast.ParsedAst
    use SG.Ast.ScopeGraph
    use SG.Manager
    use SG.Ast.Util.Finder

    @Test
    pub def misc(): Unit \ Assert + IO = assertTrue(checkOneFile("./examples/misc.flixsg"))

    @Test
    pub def paper1(): Unit \ Assert + IO = assertTrue(checkOneFile("./examples/paper1.flixsg"))

    @Test
    pub def list(): Unit \ Assert + IO = assertTrue(checkOneFile("./examples/list.flixsg"))

    @Test
    pub def usage(): Unit \ Assert + IO = assertTrue(checkOneFile("./examples/usage.flixsg"))

    def checkOneFile(path: String): Bool \ IO = run region rc {
        let man = Manager.empty(rc);
        match man |> Manager.addFileIfNotPresent(path) {
            case Result.Err(err) =>
                println(err);
                false
            case Result.Ok(()) =>
                match man |> Manager.check {
                    case Validation.Success(_) => true
                    case Validation.Failure(errs) =>
                        foreach (err <- errs) println(err);
                        false
                }
        }
    } with runFileRead

    def runFileRead(f: Unit -> Bool \ ef): Bool \ ef - Fs.FileRead + IO = {
        run f() with Fs.FileRead.runWithIO
    }

}
