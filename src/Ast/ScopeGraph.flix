mod SG.Ast.ScopeGraph {
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.ScopeGraph.Handle.matches

    type alias Path = List[Edge]

    pub enum EdgeType with Eq {
        case Decl
        case Use
        case Lex
    }

    instance Order[EdgeType] {
        pub def compare(x: EdgeType, y: EdgeType): Comparison =
            def rank(e: EdgeType) = {
                match e {
                    case EdgeType.Decl => 1
                    case EdgeType.Lex => 2
                    case EdgeType.Use => 3
                }
            };
            Order.compare(rank(x), rank(y))
    }

    pub def edges(graph: ScopeGraph): Map[Namespace, List[Edge]] =
        let ScopeGraph({ nodes = _, edges }) = graph;
        edges

    def outgoingEdges(node: Namespace, graph: ScopeGraph): List[Edge] =
        graph
            |> edges
            |> Map.get(node)
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
        region rc {
            let queue = MutList.empty(rc) !> MutList.push(src);
            let explored = MutSet.empty(rc) !> MutSet.add(src);
            def loop() = {
                queue
                    |> MutList.pop
                    |> Option.map(node -> {
                        let adj = graph
                            |> outgoingEdges(node)
                            |> List.sortBy(Edge.edgeType);
                        adj
                            |> List.findMap(edge -> match edge {
                                case Edge.Decl(decl) => if (refr `matches` decl |> Decl.handle) {
                                    Some(decl)
                                } else {
                                    None
                                }
                                case _ => None
                            })
                            |> Option.toOk(adj)
                    })
                    |> Option.flatMap(result -> match result {
                        case Ok(found) => Some(found)
                        case Err(adj) => {
                            let successors = adj |> List.findMap(edge -> match edge {
                                case Edge.Decl(_) => None
                                case Edge.Lex(parent) => Some(parent)
                                case Edge.Use(usage) => {
                                    
                                    graph
                                        |> findHandle(usage)
                                        |> Option.map(handle -> {
                                            let Handle.Handle(_, _, namespace) = handle;
                                            namespace
                                        })
                                }
                            });
                            foreach(node <- successors) {
                                if (not (node `MutSet.memberOf` explored)) {
                                    queue |> MutList.push(node);
                                    explored |> MutSet.add(node)
                                } else () // Detect cycle???
                            };
                            loop()
                        }
                    })
            };
            loop()
        }

}
