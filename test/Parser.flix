mod ScopeGraph.Test.Parser {
    use Functor.map
    use ScopeGraph.StringCursor.mk
    use ScopeGraph.Parser
    use ScopeGraph.Ast

    @Test
    pub def qName01(): Bool \ IO = run region rc {
        let input = "  John.Travolta.height";
        let name = mk(rc, input) |> Parser.parseName;
        Ast.Name.getSimple(name) == (List#{"John", "Travolta"}, "height")
    } with handleError

    @Test
    pub def module01(): Bool \ IO = run region rc {
        let input = "  mod Test.Me { }";
        let scope = mk(rc, input) |> Parser.parseProgram;
        match scope {
            case module :: Nil =>
                coerce(module)#name |> Ast.Name.getSimple == (List#{"Test"}, "Me")
            case other =>
                println("found modules ${other |> map(Ast.Module.getName)}");
                false
        }
    } with handleError

    def handleError(f: Unit -> Bool \ ef): Bool \ ef - Parser.Error + IO = {
        run f() with handler Parser.Error {
            def error(msg, _) = {
                println(msg);
                false
            }
        }
    }
}
