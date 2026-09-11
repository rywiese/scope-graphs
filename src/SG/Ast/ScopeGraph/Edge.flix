pub mod SG.Ast.ScopeGraph.Edge {
    use SG.Ast.ScopeGraph.Decl
    use SG.Ast.ScopeGraph.EdgeType
    use SG.Ast.ScopeGraph.Scope

    pub def edgeType(edge: Edge): EdgeType =
        match edge {
            case Lex(_) => EdgeType.Lex
            case Use(_) => EdgeType.Use
            case Decl(_) => EdgeType.Dec
        }
}
