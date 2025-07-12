mod ScopeGraph.Test.Parser {
    use Functor.map
    use ScopeGraph.Ast
    use ScopeGraph.Phases.Parser
    use ScopeGraph.Phases.Lexer
    use ScopeGraph.Util.StringCursor
    use ScopeGraph.Util.VectorCursor

    @Test
    pub def module01(): Bool \ IO = run region rc {
        let input = "  mod Test.Me { }";
        input |>
            StringCursor.mk(rc) |>
            Lexer.lex |>
            VectorCursor.mk(rc) |>
            Parser.parseProgram;
        true
    } with handleError

    @Test
    pub def module02(): Bool \ IO = run region rc {
        let input = "  mod Test.Me { mod Q {}mod   Qqq2.A    {} }  ";
        input |>
            StringCursor.mk(rc) |>
            Lexer.lex |>
            VectorCursor.mk(rc) |>
            Parser.parseProgram;
        true
    } with handleError

    @Test
    pub def use01(): Bool \ IO = run region rc {
        let input = "  use A   use A.B   use A.b  use A.B as B   use A.f as g";
        input |>
            StringCursor.mk(rc) |>
            Lexer.lex |>
            VectorCursor.mk(rc) |>
            Parser.parseProgram;
        true
    } with handleError

    def handleError(f: Unit -> Bool \ ef): Bool \ (ef - Parser.Error) - Lexer.Error + IO = {
        run f()
        with handler Parser.Error {
            def error(msg, _) = {
                println(msg);
                false
            }
        } with handler Lexer.Error {
            def error(msg, _) = {
                println(msg);
                false
            }
        }
    }
}
