mod SG.Ast {
    use SG.Ast.ScopeGraph.ScopedAst

    pub enum CheckResult({
        parsedAst = ParsedAst,
        ast = ScopedAst
    })

    instance Coerce[CheckResult] {
        type Out = {parsedAst = ParsedAst, ast = ScopedAst}

        pub def coerce(x: CheckResult): {parsedAst = ParsedAst, ast = ScopedAst} = {
            let CheckResult.CheckResult(r) = x;
            r
        }
    }
}

mod SG.Ast.CheckResult {
    use SG.Ast.ScopeGraph.ScopedAst
    use Functor.map

    use SG.Ast.Util.Finder
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.Loc
    use SG.Ast.ParsedAst.AstNode
    use SG.Ast.ParsedAst
    use SG.Ast.Shared.Location

    pub def mk(parsedAst: ParsedAst, ast: ScopedAst): CheckResult =
        CheckResult({parsedAst = parsedAst, ast = ast})

    pub def enclosingRange(loc: Loc, cr: CheckResult): Option[LocRange] = {
        Finder.find(loc, _ -> (), coerce(cr)#parsedAst) |> map(Located.getLoc)
    }

    pub def definition(loc: Loc, _cr: CheckResult): Option[LocRange] = {
        let first = Location.mkLoc(line = 1, column = 1, 0, coerce(loc)#src);
        Some(Location.Loc.point(first))
    }

}
