mod SG.Ast {
    use SG.Ast.ScopeGraph.Node

    pub enum ScopeGraph({
        refs = Map[Node.Ref, Node.Scope],
        edges = Map[Node.Scope, List[ScopeGraph.Edge]]
    })

    mod ScopeGraph {
        use SG.Ast.ScopeGraph.Node

        mod Node {
            use SG.Ast.Shared.Location.Located
            use SG.Ast.ParsedAst
            use SG.Ast.Shared.Location.LocRange
            use SG.Ast.Shared.Location.LocString

            pub enum Ref with Eq, Order {
                case Name(ParsedAst.Name)
                case LocString(LocString)
            }

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

            mod Ref {
                use SG.Ast.Shared.Location.Located.getLoc
                use SG.Ast.ParsedAst
                use SG.Ast.ScopeGraph.Node
                use SG.Ast.ScopeGraph.Node.Ref

                pub def fromAstNode(astNode: ParsedAst.AstNode): Option[Ref] =
                    match astNode {
                        case ParsedAst.AstNode.Name(name) => Some(Ref.Name(name))
                        case ParsedAst.AstNode.Use(_uze) => None // renaming?
                        case _ => None
                    }

                pub def matches(other: Ref, this: Ref): Bool =
                    id(this) == id(other)

                pub def id(ref: Ref): String =
                    match ref {
                        case Ref.Name(name) => coerce(coerce(name)#name)#s
                        case Ref.LocString(str) => coerce(str)#s
                    }

            }

            instance Located[Ref] {
                pub def getLoc(x: Ref): LocRange =
                    match x {
                        case Ref.Name(name) => coerce(name)#loc
                        case Ref.LocString(str) => coerce(str)#loc
                    }
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
                use SG.Ast.ScopeGraph.Node

                pub def handle(decl: Decl): Node.Ref =
                    match decl {
                        case Module(module) => Node.Ref.Name(coerce(module)#name)
                        case TypeAlias(aliass) => Node.Ref.LocString(coerce(aliass)#name)
                        case Enum(eenum) => Node.Ref.LocString(coerce(eenum)#name)
                        case EnumCase(enumCase) => Node.Ref.LocString(coerce(enumCase)#name)
                        case Def(deff) => Node.Ref.LocString(coerce(deff)#name)
                        case Tpe(tpe) => Node.Ref.LocString(tpe)
                        case Id(id) => Node.Ref.LocString(id)
                    }
            }

        }

        pub enum Edge {
            case Lex(Node.Scope)
            case Use(Node.Scope)
            case Decl(Node.Decl)
        }

        def refs(graph: ScopeGraph): Map[Node.Ref, Node.Scope] =
            let ScopeGraph({ refs, edges = _ }) = graph;
            refs

        def edges(graph: ScopeGraph): Map[Node.Scope, List[Edge]] =
            let ScopeGraph({ refs = _, edges }) = graph;
            edges

        def scopeForRef(ref: Node.Ref, graph: ScopeGraph): Option[Node.Scope] =
            graph
                |> refs
                |> Map.get(ref)

        def edgesForScope(scope: Node.Scope, graph: ScopeGraph): List[Edge] =
            graph
                |> edges
                |> Map.get(scope)
                |> Option.getWithDefault(List.empty())

        pub def findDecl(ref: Node.Ref, graph: ScopeGraph): Option[Node.Decl] =
            findPathsFromRef(ref, graph) |> List.head

        pub def findPathsFromRef(src: Node.Ref, graph: ScopeGraph): List[Node.Decl] =
            graph
                |> scopeForRef(src)
                |> Option.map(scope -> findPathsFromScope(scope, src, graph))
                |> Option.getWithDefault(List.empty())

        pub def findPathsFromScope(src: Node.Scope, matching: Node.Ref, graph: ScopeGraph): List[Node.Decl] =
            graph
                |> edgesForScope(src)
                |> List.flatMap(edge -> match edge {
                    case Edge.Decl(snk) => snk :: Nil
                    case Edge.Lex(scope) => findPathsFromScope(scope, matching, graph)
                    case Edge.Use(scope) => findPathsFromScope(scope, matching, graph)
                })
                |> List.filter(Node.Decl.handle >> Node.Ref.matches(matching))

    }

}
