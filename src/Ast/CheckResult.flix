mod SG.Ast {
    use SG.Ast.ScopeGraph

    pub enum CheckResult({
        parsedAst = ParsedAst,
        scopeGraph = ScopeGraph
    })

    instance Coerce[CheckResult] {
        type Out = {parsedAst = ParsedAst, scopeGraph = ScopeGraph}

        pub def coerce(x: CheckResult): {parsedAst = ParsedAst, scopeGraph = ScopeGraph} = {
            let CheckResult.CheckResult(r) = x;
            r
        }
    }
}

mod SG.Ast.CheckResult {
    use Functor.map
    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.AstNode
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Node
    use SG.Ast.Shared.Location
    use SG.Ast.Shared.Location.Loc
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Util.Finder

    pub def mk(parsedAst: ParsedAst, scopeGraph: ScopeGraph): CheckResult =
        CheckResult({parsedAst = parsedAst, scopeGraph = scopeGraph})

    pub def enclosingRange(loc: Loc, cr: CheckResult): Option[LocRange] = {
        Finder.find(loc, _ -> (), coerce(cr)#parsedAst) |> map(Located.getLoc)
    }

    pub def definition(loc: Loc, cr: CheckResult): Option[LocRange] =
        let {parsedAst = parsedAst, scopeGraph = scopeGraph} = coerce(cr);
        Finder.find(loc, _ -> (), parsedAst)
            |> Option.flatMap(node -> match node {
                case ParsedAst.AstNode.Name(name) => Some(coerce(name)#name)
                case ParsedAst.AstNode.Tpe(tpe) => match tpe {
                    case ParsedAst.Tpe.Base(name, _) => Some(coerce(name)#name)
                    case ParsedAst.Tpe.App(_, _, _) => None
                }
                case ParsedAst.AstNode.Use(_uze) => None // renaming?
                case _ => None
            })
            |> Option.flatMap(ref -> ScopeGraph.findDecl(ref, scopeGraph))
            |> Option.map(Node.Decl.handle >> Located.getLoc)

}
