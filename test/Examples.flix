mod ScopeGraph.Test.Examples {
    use ScopeGraph.Phase.Scoper

    @Test
    pub def misc(): Bool \ IO = {
        let ast = ScopeGraph.compileFileUnsafe("./examples/misc.flixSG");
        region rc {
            let _sg = Scoper.buildScopeGraph(rc, ast);
            // println(sg)
            checked_ecast(true)
        }
    }

    @Test
    pub def paper1(): Bool \ IO = {
        let ast = ScopeGraph.compileFileUnsafe("./examples/paper1.flixSG");
        region rc {
            let _sg = Scoper.buildScopeGraph(rc, ast);
            // println(sg)
            checked_ecast(true)
        }
    }

}
