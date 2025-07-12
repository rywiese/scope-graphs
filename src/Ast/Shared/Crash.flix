mod ScopeGraph.Ast.Shared {

    pub eff Crash {
        def crash(error: Error): Void
    }

    mod Crash {
        use ScopeGraph.Ast.Shared.Error

        pub def runUnsafe(f: Unit -> t \ ef): t \ ef - Crash =
            run f() with handler Crash {
                def crash(error, _) = bug!(ToString.toString(error))
            }

        pub def runWith(handle: Error -> t \ ef1, f: Unit -> t \ ef2): t \ ef2 - Crash + ef1 =
            run f() with handler Crash {
                def crash(error, _) = handle(error)
            }

    }

}
