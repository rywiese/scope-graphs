mod ScopeGraph.Test.Parser {
    use ScopeGraph.StringCursor.mk
    use ScopeGraph.Parser
    use ScopeGraph.Ast

    @Test
    pub def qName01(): Bool \ IO = run region rc {
        let input = "  John.Travolta.height";
        let name = mk(rc, input) |> Parser.parseName;
        Ast.Name.getSimple(name) == (List#{"John", "Travolta"}, "height")
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
