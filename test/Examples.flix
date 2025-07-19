mod ScopeGraph.Test.Examples {
    use ScopeGraph.Manager

    @Test
    pub def misc(): Bool \ IO = run region rc {
        let man = Manager.empty(rc);
        man |> Manager.addFileIfNotPresent("./examples/misc.flixsg");
        match man |> Manager.check {
            case Validation.Success(_) => true
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
