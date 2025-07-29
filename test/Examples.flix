mod SG.Test.Examples {
    use SG.Ast.Shared.Location.mkLoc
    use SG.Ast.Shared.Location.Source
    use SG.Ast.Shared.Location.Located
    use SG.Ast.ParsedAst
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Node
    use SG.Manager
    use SG.Ast.Util.Finder

    @Test
    pub def misc(): Bool \ IO = checkOneFile("./examples/misc.flixsg")

    @Test
    pub def paper1(): Bool \ IO = checkOneFile("./examples/paper1.flixsg")

    @Test
    pub def list(): Bool \ IO = checkOneFile("./examples/list.flixsg")

    def checkOneFile(path: String): Bool \ IO = run region rc {
        let man = Manager.empty(rc);
        man |> Manager.addFileIfNotPresent(path);
        match man |> Manager.check {
            case Validation.Success(_) =>
                // let {parsedAst = _parsedAst, scopeGraph = scopeGraph} = coerce(cr);
                // println(scopeGraph |> ScopeGraph.refs);
                true
            case Validation.Failure(errs) =>
                foreach (err <- errs) println(err);
                false
        }
    } with runFileRead

    def runFileRead(f: Unit -> Bool \ ef): Bool \ ef - FileRead + IO = {
        match (run f() with FileRead.runWithIO) {
            case Result.Ok(b) => b
            case Result.Err(err) =>
                println(err);
                false
        }
    }

}
