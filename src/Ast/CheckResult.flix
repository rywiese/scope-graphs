mod SG.Ast {
    use SG.Ast.ScopeGraph.ScopedAst

    pub enum CheckResult[r: Region]({
        parsedAst = ParsedAst,
        ast = ScopedAst[r]
    })

    instance Coerce[CheckResult[r]] {
        type Out = {parsedAst = ParsedAst, ast = ScopedAst[r]}

        pub def coerce(x: CheckResult[r]): {parsedAst = ParsedAst, ast = ScopedAst[r]} = {
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

    pub def mk(parsedAst: ParsedAst, ast: ScopedAst[r]): CheckResult[r] =
        CheckResult({parsedAst = parsedAst, ast = ast})

    pub def enclosingRange(loc: Loc, cr: CheckResult[r]): Option[LocRange] = {
        Finder.find(loc, _ -> (), coerce(cr)#parsedAst) |> map(Located.getLoc)
    }

    pub def definition(loc: Loc, _cr: CheckResult[r]): Option[LocRange] = {
        let first = Location.mkLoc(line = 1, column = 1, 0, coerce(loc)#src);
        Some(Location.Loc.point(first))
    }

}
