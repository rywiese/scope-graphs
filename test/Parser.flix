mod ScopeGraph.Test.Parser {
    use Functor.map
    use ScopeGraph.Parsing.StringCursor.mk
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
        match coerce(scope) {
            case {modules = module :: Nil, uses = Nil} =>
                coerce(module)#name |> Ast.Name.getSimple == (List#{"Test"}, "Me")
            case {modules | _} =>
                println("found modules ${modules |> map(Ast.Module.getName)}");
                false
        }
    } with handleError

    @Test
    pub def module02(): Bool \ IO = run region rc {
        let input = "  mod Test.Me { mod Q {}mod   Qqq2.A    {} }  ";
        let scope = mk(rc, input) |> Parser.parseProgram;
        match coerce(scope) {
            case {modules = module :: Nil, uses = Nil} =>
                coerce(module)#name `eqName` List#{"Test", "Me"}
            case {modules | _} =>
                println("found modules ${modules |> map(Ast.Module.getName)}");
                false
        }
    } with handleError

    @Test
    pub def use01(): Bool \ IO = run region rc {
        let input = "  use A   use A.B   use A.b  use A.B as B   use A.f as g";
        let scope = mk(rc, input) |> Parser.parseProgram;
        match coerce(scope) {
            case {modules = Nil, uses = one :: two :: three :: four :: five :: Nil} =>
                Ast.Use.getSimple(one) == (Nil, "A", None) and
                Ast.Use.getSimple(two) == (List#{"A"}, "B", None) and
                Ast.Use.getSimple(three) == (List#{"A"}, "b", None) and
                Ast.Use.getSimple(four) == (List#{"A"}, "B", Some("B")) and
                Ast.Use.getSimple(five) == (List#{"A"}, "f", Some("g"))
            case {uses | _} =>
                println("found uses ${uses |> map(u -> coerce(u)#name)}");
                false
        }
    } with handleError


    def eqName(n: Ast.Name, l: List[String]): Bool = {
        let (qualifiers, name) = n |> Ast.Name.getSimple;
        (qualifiers `List.append` List#{name}) == l
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
