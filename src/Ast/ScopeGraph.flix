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

            pub enum Scope with Eq, Order {
                case Top
                case Module(Scope, String)
                case Let(Scope, String)
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

                pub def enter(moduleName: ParsedAst.Name, parent: Scope): Scope =
                    let { qualifiers, name, loc = _ } = coerce(moduleName);
                    let withQualifiers = qualifiers
                        |> List.map(qualifier -> coerce(qualifier)#s)
                        |> List.foldLeft(Scope.Module, parent);
                    let modName = coerce(name)#s;
                    Scope.Module(withQualifiers, modName)

                pub def fromFullyQualified(moduleName: ParsedAst.Name): Scope =
                    Scope.Top |> enter(moduleName)

                pub def letBind(id: String, parent: Scope): Scope =
                    Scope.Let(parent, id)
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
