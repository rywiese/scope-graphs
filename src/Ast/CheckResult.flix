mod SG.Ast {
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

mod SG.Ast.CheckResult {
    use Functor.map

    use SG.Ast.Util.Finder
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.Loc
    use SG.Ast.ParsedAst.AstNode
    use SG.Ast.ParsedAst
    use SG.Ast.Shared.Location

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
