mod ScopeGraph.Ast {
    pub enum CheckResult({
        ast = ParsedAst
    })

    instance Coerce[CheckResult] {
        type Out = {ast = ParsedAst}

        pub def coerce(x: CheckResult): {ast = ParsedAst} = {
            let CheckResult.CheckResult(r) = x;
            r
        }
    }
}

mod ScopeGraph.Ast.CheckResult {
    use Functor.map

    use ScopeGraph.Ast.Util.Finder
    use ScopeGraph.Ast.Shared.Location.LocRange
    use ScopeGraph.Ast.Shared.Location.Located
    use ScopeGraph.Ast.Shared.Location.Loc
    use ScopeGraph.Ast.ParsedAst.AstNode
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.Shared.Location

    pub def mk(ast: ParsedAst): CheckResult =
        CheckResult({ast = ast})

    pub def enclosingRange(loc: Loc, cr: CheckResult): Option[LocRange] = {
        Finder.find(loc, _ -> (), coerce(cr)#ast) |> map(Located.getLoc)
    }

    pub def definition(loc: Loc, _cr: CheckResult): Option[LocRange] = {
        let first = Location.mkLoc(line = 1, column = 1, 0, coerce(loc)#src);
        Some(Location.Loc.point(first))
    }

}
