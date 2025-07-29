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

    // def matchingDecl(scope: Namespace, refr: Refr, graph: ScopeGraph): List[Decl] =
    //     graph
    //         |> outgoingEdges(scope)
    //         |> List.filterMap(edge -> match edge {
    //             case Edge.Decl(decl) => if (refr `matches` decl |> Decl.handle) {
    //                 Some(decl)
    //             } else {
    //                 None
    //             }
    //             case _ => None
    //         })

    pub def findDecl(src: Refr, graph: ScopeGraph): Option[Decl] =
        let namespace = coerce(src)#referencedIn;
        graph |> findDeclFromNamespace(namespace, src)

    // pub def findDeclOld(src: Refr, graph: ScopeGraph): Option[Decl] =
    //     let namespace = coerce(src)#referencedIn;
    //     graph
    //         // |> findPaths(namespace, src)
    //         // |> Option.map(scope -> findPaths(scope, src, graph))
    //         // |> Option.getWithDefault(List.empty())
    //         // |> List.sortWith(???) // TODO: ordering of paths
    //         |> List.filterMap(path -> match List.last(path) {
    //             case Some(Edge.Decl(decl)) => Some(decl)
    //             case _ => None
    //         })
    //         |> List.head // this maybe should be a fold that stops when we've found exactly one and fails if we find too many

    def findDeclFromNamespace(src: Namespace, refr: Refr, graph: ScopeGraph): Option[Decl] =
        graph
            |> outgoingEdges(src)
            |> List.findMap(edge -> match edge {
                case Edge.Decl(decl) => if (refr `matches` decl |> Decl.handle) {
                    Some(decl)
                } else {
                    None
                }
                case Edge.Lex(parent) => graph |> findDeclFromNamespace(parent, refr)
                case _ => None
                // case Edge.Use(used) => graph
                //     |> findDecl(used)
                //     |> Option.flatMap(decl -> {
                //         let Handle.Handle(_, _, namespace) = decl |> Decl.handle;
                //         graph |> findDeclFromNamespace(namespace, refr)
                //     })
            })

}
