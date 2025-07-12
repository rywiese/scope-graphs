mod ScopeGraph.Test.Full {
    @Test
    pub def examplesMisc(): Bool \ IO = {
        ScopeGraph.File.checkFileUnsafe("./examples/misc.flixSG");
        true
    }
}
