mod ScopeGraph.Ast {
    pub enum CheckResult({
        ast = ParsedAst.Scope
    })

    instance Coerce[CheckResult] {
        type Out = {ast = ParsedAst.Scope}

        pub def coerce(x: CheckResult): {ast = ParsedAst.Scope} = {
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

    pub def mk(ast: ParsedAst.Scope): CheckResult =
        CheckResult({ast = ast})

    pub def enclosingRange(loc: Loc, cr: CheckResult): Option[LocRange] = {
        Finder.find(loc, coerce(cr)#ast) |> map(Located.getLoc)
    }

    pub def definition(_loc: Loc, _cr: CheckResult): Option[LocRange] = {
        let first = Location.mkLoc(line = 1, column = 1, 0);
        Some(Location.Loc.point(first))
    }

}
