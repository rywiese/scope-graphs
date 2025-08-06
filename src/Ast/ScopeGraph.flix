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
            rank(x) <=> rank(y)
    }

    pub def edges(graph: ScopeGraph): Map[Scope, List[Edge]] =
        let ScopeGraph({ nodes = _, edges }) = graph;
        edges

    def outgoingEdges(node: Scope, graph: ScopeGraph): List[Edge] =
        graph
            |> edges
            |> Map.get(node)
            |> Option.getWithDefault(List.empty())

    pub def findHandle(src: Refr, graph: ScopeGraph): Option[Handle] =
        graph
            |> findDecls(src)
            |> List.head
            |> Option.map(Decl.handle)

    pub def findDecls(src: Refr, graph: ScopeGraph): List[Decl] =
        match src {
            case Refr.Qualified(_, qualifier, _) => graph
                |> findDecls(qualifier)
                |> List.flatMap(Decl.associatedScope
                    >> Option.map(node -> graph |> bfs(node, src))
                    >> Option.getWithDefault(List.empty())
                )
                // TODO: filter out long paths (multiple nodes will have different bfs results)
                // TODO: the below is maybe too aggressive a filtering
                |> List.head
                |> Option.toList
            case Refr.Unqualified(_, _, referencedIn) => graph |> bfs(referencedIn, src)
        }

    // All paths of length n where n is the shortest path
    def bfs(src: Scope, refr: Refr, graph: ScopeGraph): List[Decl] =
        region rc {
            let queue = MutList.empty(rc) !> MutList.push(src);
            let explored = MutSet.empty(rc) !> MutSet.add(src);
            def loop() = {
                let result = queue
                    |> MutList.pop
                    |> Option.map(node -> {
                        let outgoing = graph
                            |> outgoingEdges(node)
                            |> List.sortBy(Edge.edgeType);
                        let matches = outgoing |> List.flatMap(edge -> match edge {
                            case Edge.Decl(decl) => if (refr `matches` decl |> Decl.handle) {
                                decl :: Nil
                            } else {
                                Nil
                            }
                            case Edge.Lex(_) => Nil
                            case Edge.Use(_usage) => Nil
                                // this is bad. we can't start a new bfs inside an existing one or we'll never terminate
                                // unless... maybe we pass in a new regex that doesn't allow crossing imports again...
                                // graph |> findDecls(usage)
                        });
                        if (not (matches |> List.isEmpty)) {
                            Ok(matches)
                        } else {
                            Err(outgoing)
                        }
                    })
                    |> Option.getWithDefault(Ok(List.empty()));
                match result {
                    case Ok(found) => found
                    case Err(outgoing) => {
                        let successors = outgoing |> List.findMap(edge -> match edge {
                            case Edge.Decl(_) => None
                            case Edge.Lex(parent) => Some(parent)
                            case Edge.Use(_) => None
                        });
                        foreach(node <- successors) {
                            if (not (node `MutSet.memberOf` explored)) {
                                queue |> MutList.push(node);
                                explored |> MutSet.add(node)
                            } else ()
                        };
                        loop()
                    }
                }
            };
            loop()
        }

}
