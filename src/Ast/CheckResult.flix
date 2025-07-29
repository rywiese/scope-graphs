mod SG.Ast {
    use SG.Ast.ScopeGraph

    pub enum CheckResult({
        scopeGraph = ScopeGraph
    })

    instance Coerce[CheckResult] {
        type Out = { scopeGraph = ScopeGraph }

        pub def coerce(x: CheckResult): { scopeGraph = ScopeGraph } = {
            let CheckResult.CheckResult(r) = x;
            r
        }
    }
}

mod SG.Ast.CheckResult {
    use Functor.map
    use SG.Ast.ParsedAst
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.AstNode
    use SG.Ast.ScopeGraph.Decl
    use SG.Ast.Shared.Location
    use SG.Ast.Shared.Location.Loc
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Util.Finder

    pub def mk(scopeGraph: ScopeGraph): CheckResult =
        CheckResult({ scopeGraph = scopeGraph })

    pub def enclosingRange(loc: Loc, cr: CheckResult): Option[LocRange] = {
        Finder.find(loc, _ -> (), coerce(cr)#scopeGraph) |> map(Located.getLoc)
    }

    pub def definition(loc: Loc, cr: CheckResult): Option[LocRange] =
        let { scopeGraph = scopeGraph } = coerce(cr);
        Finder.find(loc, _ -> (), scopeGraph)
            |> Option.flatMap(node -> match node {
                case AstNode.Refr(refr) => ScopeGraph.findDecl(refr, scopeGraph)
                // case for findRefrs(handle) ?
                case _ => None
            })
            |> Option.map(Decl.handle >> Located.getLoc)

}
