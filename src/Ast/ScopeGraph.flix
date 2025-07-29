mod SG.Ast.ScopeGraph {
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.ScopeGraph.Handle.matches

    type alias Path = List[Edge]

    pub enum EdgeType with Eq, Order {
        case Decl
        case Use
        case Lex
    }

    pub def edges(graph: ScopeGraph): Map[Namespace, List[Edge]] =
        let ScopeGraph({ nodes = _, edges }) = graph;
        edges

    def outgoingEdges(scope: Namespace, graph: ScopeGraph): List[Edge] =
        graph
            |> edges
            |> Map.get(scope)
            |> Option.getWithDefault(List.empty())

    pub def findHandle(src: Refr, graph: ScopeGraph): Option[Handle] =
        let refr = coerce(src);
        let namespace = refr#qualifier
            |> Option.flatMap(qualifier -> findHandle(qualifier, graph))
            |> Option.map(parent -> {
                let Handle.Handle(_, _, namespace) = parent;
                namespace
            })
            |> Option.getWithDefault(refr#referencedIn);
        graph
            |> findDecl(namespace, src)
            |> Option.map(Decl.handle)

    def findDecl(src: Namespace, refr: Refr, graph: ScopeGraph): Option[Decl] =
        graph
            |> outgoingEdges(src)
            |> List.sortBy(Edge.edgeType)
            |> List.findMap(edge -> match edge {
                case Edge.Decl(decl) => if (refr `matches` decl |> Decl.handle) {
                    Some(decl)
                } else {
                    None
                }
                case Edge.Lex(parent) => graph |> findDecl(parent, refr)
                case Edge.Use(used) => graph
                    |> findHandle(used)
                    |> Option.flatMap(handle -> {
                        let Handle.Handle(_, _, namespace) = handle;
                        graph |> findDecl(namespace, refr)
                    })
            })

}
