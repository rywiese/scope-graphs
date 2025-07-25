mod SG.Ast {
    use SG.Ast.ScopeGraph.Node

    pub enum ScopeGraph({
        refs = Map[Node.Ref, Node.Scope],
        edges = Map[Node.Scope, List[ScopeGraph.Edge]]
    })

    mod ScopeGraph {

        mod Node {
            use SG.Ast.ParsedAst
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
                use SG.Ast.ScopeGraph.Node

                pub def handle(decl: Decl): Node.Ref = ??? // or Located? something that we can point to specifically
            }

        }

        pub enum Edge {
            case Lex(Node.Scope)
            case Use(Node.Scope)
            case Decl(Node.Decl)
        }

    }

}
