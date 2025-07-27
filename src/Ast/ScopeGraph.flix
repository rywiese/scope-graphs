mod SG.Ast {
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Node
    use SG.Ast.Shared.Location.LocString

    pub enum ScopeGraph({
        refs = Map[LocString, Node.Scope],
        edges = Map[Node.Scope, List[Edge]]
    })

    mod ScopeGraph {
        use SG.Ast.Shared.Location.LocString
        use SG.Ast.ScopeGraph.Node

        mod Node {
            use SG.Ast.Shared.Location.Located
            use SG.Ast.ParsedAst
            use SG.Ast.Shared.Location.LocRange
            use SG.Ast.Shared.Location.LocString

            pub enum Scope with Eq, Order, ToString {
                case Top
                case Module(String, Scope)
                case Alias(String, Scope)
                case Let(String, Scope)
                case Def(String, Scope)
                case Enum(String, Scope)
            }

            pub enum Decl {
                case Module(ParsedAst.Module)
                case TypeAlias(ParsedAst.TypeAlias)
                case Enum(ParsedAst.Enum)
                case EnumCase(ParsedAst.EnumCase)
                case Def(ParsedAst.Def)
                case Tpe(LocString)
                case Id(LocString)
            }

            mod Scope {
                use SG.Ast.ParsedAst
                use SG.Ast.ScopeGraph.Node.Scope

                pub def parent(scope: Scope): Scope =
                    match scope {
                        case Top => Top
                        case Module(_, parent) => parent
                        case Alias(_, parent) => parent
                        case Let(_, parent) => parent
                        case Def(_, parent) => parent
                        case Enum(_, parent) => parent
                    }

                pub def fromFullyQualified(moduleName: ParsedAst.Name): Scope =
                    Scope.Top |> module(moduleName)

                pub def module(moduleName: ParsedAst.Name, parent: Scope): Scope =
                    let { qualifiers, name, loc = _ } = coerce(moduleName);
                    let withQualifiers = qualifiers
                        |> List.map(qualifier -> coerce(qualifier)#s)
                        |> List.foldLeft(s -> n -> Scope.Module(n, s), parent);
                    let modName = coerce(name)#s;
                    Scope.Module(modName, withQualifiers)

            }

            mod Decl {
                use SG.Ast.ParsedAst
                use SG.Ast.Shared.Location.LocString
                use SG.Ast.ScopeGraph.Node

                pub def handle(decl: Decl): LocString =
                    match decl {
                        case Module(module) => coerce(coerce(module)#name)#name
                        case TypeAlias(aliass) => coerce(aliass)#name
                        case Enum(eenum) => coerce(eenum)#name
                        case EnumCase(enumCase) => coerce(enumCase)#name
                        case Def(deff) => coerce(deff)#name
                        case Tpe(tpe) => tpe
                        case Id(id) => id
                    }
            }

        }

        pub enum Edge {
            case Lex(Node.Scope)
            case Use(Node.Scope)
            case Decl(Node.Decl)
        }

        type alias Path = List[Edge]

        def refs(graph: ScopeGraph): Map[LocString, Node.Scope] =
            let ScopeGraph({ refs, edges = _ }) = graph;
            refs

        def edges(graph: ScopeGraph): Map[Node.Scope, List[Edge]] =
            let ScopeGraph({ refs = _, edges }) = graph;
            edges

        def scopeOf(str: LocString, graph: ScopeGraph): Option[Node.Scope] =
            graph
                |> refs
                |> Map.get(str)

        def outgoingEdges(scope: Node.Scope, graph: ScopeGraph): List[Edge] =
            graph
                |> edges
                |> Map.get(scope)
                |> Option.getWithDefault(List.empty())

        pub def findDecl(src: LocString, graph: ScopeGraph): Option[Node.Decl] =
            graph
                |> scopeOf(src)
                |> Option.map(scope -> findPaths(scope, src, graph))
                |> Option.getWithDefault(List.empty())
                // !> List.forEach(path -> {
                //     let _ = dbg("Here's a path:\n");
                //     let _ = foreach(edge <- path) match edge {
                //         case Edge.Lex(scope) => {
                //             let _ = dbg("${scope},");
                //             ()
                //         }
                //         case Edge.Use(scope) => {
                //             let _ = dbg("${scope},");
                //             ()
                //         }
                //         case Edge.Decl(decl) => {
                //             let handle = Node.Decl.handle(decl);
                //             let _ = dbg("${handle}");
                //             ()
                //         }
                //     };
                //     ()
                // })
                // |> List.sortWith(???) // TODO: ordering of paths
                |> List.filterMap(path -> match List.last(path) {
                    case Some(Edge.Decl(decl)) => Some(decl)
                    case _ => None
                })
                |> List.head // this maybe should be a fold that stops when we've found exactly one and fails if we find too many

        def findPaths(src: Node.Scope, tgt: LocString, graph: ScopeGraph): List[Path] =
            graph
                |> outgoingEdges(src)
                |> List.flatMap(edge -> {
                    match edge {
                        case Edge.Decl(decl) => if (Node.Decl.handle(decl) == tgt) {
                            (edge :: Nil) :: Nil
                        } else {
                            Nil
                        }
                        case Edge.Lex(scope) => findPaths(scope, tgt, graph) |> List.map(path -> edge :: path)
                        case Edge.Use(scope) => findPaths(scope, tgt, graph) |> List.map(path -> edge :: path)
                    }
                })

    }

}
