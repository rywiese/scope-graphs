mod ScopeGraph.Test.Parser {
    use Functor.map
    use ScopeGraph.Parsing.StringCursor.mk
    use ScopeGraph.Parser
    use ScopeGraph.Ast

    @Test
    pub def module01(): Bool \ IO = run region rc {
        let input = "  mod Test.Me { }";
        mk(rc, input) |> Parser.parseProgram;
        true
    } with handleError

    @Test
    pub def module02(): Bool \ IO = run region rc {
        let input = "  mod Test.Me { mod Q {}mod   Qqq2.A    {} }  ";
        mk(rc, input) |> Parser.parseProgram;
        true
    } with handleError

    @Test
    pub def use01(): Bool \ IO = run region rc {
        let input = "  use A   use A.B   use A.b  use A.B as B   use A.f as g";
        mk(rc, input) |> Parser.parseProgram;
        true
    } with handleError

    @Test
    pub def examplesMisc(): Bool \ IO = {
        discard ScopeGraph.Parsing.File.parseFileUnsafe("./examples/misc.txt");
        true
    }

    def handleError(f: Unit -> Bool \ ef): Bool \ ef - Parser.Error + IO = {
        run f() with handler Parser.Error {
            def error(msg, _) = {
                println(msg);
                false
            }
        }
    }
}
