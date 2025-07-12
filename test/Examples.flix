mod ScopeGraph.Test.Examples {

    @Test
    pub def misc(): Bool \ IO = {
        ScopeGraph.compileFileUnsafe("./examples/misc.flixSG");
        true
    }

}
