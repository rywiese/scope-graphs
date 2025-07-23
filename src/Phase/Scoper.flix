mod SG.Phase.Scoper {
    use SG.Ast.ParsedAst.Module.Module
    use ToString.toString

    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.AstNode
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Def
    use SG.Ast.ScopeGraph.Edge
    use SG.Ast.ScopeGraph.Enum
    use SG.Ast.ScopeGraph.Module
    use SG.Ast.ScopeGraph.Node
    use SG.Ast.ScopeGraph.Scope
    use SG.Ast.ScopeGraph.TypeAlias
    use SG.Ast.ScopeGraph.Use
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.LocString

    pub def resolve(ast: ParsedAst): Validation[Error, ScopeGraph.ScopedAst] = {
        Validation.Success(visitAst(ast))
    }

    def visitAst(parsedAst: ParsedAst): ScopeGraph.ScopedAst =
        let top = parsedAst#m
            |> Map.valuesOf
            |> List.map(visitScope(Node.Top))
            |> List.fold;
        let edges = MultiMap.empty();
            // |> addEdgesFromScope(top);
        { top = top, edges = edges }

    // Phase 1: Top scope AST

    def visitScope(node: Node, scope: ParsedAst.Scope): Scope =
        let coerced = coerce(scope);
        Scope.Scope({
            uses = coerced#uses |> List.map(visitUse(node)),
            modules = coerced#modules |> List.map(visitModule(node)),
            aliases = coerced#aliases |> List.map(visitTypeAlias(node)),
            enums = coerced#enums |> List.map(visitEnum(node)),
            defs = coerced#defs |> List.map(visitDef(node)),
            node = node
        })

    def visitUse(parent: Node, uze: ParsedAst.Use): ScopeGraph.Use =
        let coerced = coerce(uze);
        Use.Use({ +parent = parent | coerced })

    def visitModule(parent: Node, module: ParsedAst.Module): Module =
        let coerced = coerce(module);
        let name = coerce(coerced#name);
        let node = Node.Named({
            qualifiers = name#qualifiers |> List.map(ls -> coerce(ls)#s),
            name = coerce(name#name)#s
        });
        Module.Module({
            ac = coerced#ac,
            loc = coerced#loc,
            scope = visitScope(node, coerced#scope),
            parent = parent
        })

    def visitTypeAlias(parent: Node, typeAlias: ParsedAst.TypeAlias): TypeAlias =
        let coerced = coerce(typeAlias);
        TypeAlias.TypeAlias({ +parent = parent | coerced })

    def visitEnum(parent: Node, eenum: ParsedAst.Enum): Enum =
        let coerced = coerce(eenum);
        Enum.Enum({ +parent = parent | coerced })

    def visitDef(parent: Node, deff: ParsedAst.Def): Def =
        let coerced = coerce(deff);
        Def.Def({ +parent = parent | coerced })

    // Phase 2: Edges

    def addEdgesFromScope(scope: Scope, edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        let coerced = coerce(scope);
        edges
            |> addEdgesFromUses(coerced#uses)
            |> addEdgesFromModules(coerced#modules)
            |> addEdgesFromAliases(coerced#aliases)
            |> addEdgesFromEnums(coerced#enums)
            |> addEdgesFromDefs(coerced#defs)

    def addEdgesFromUses(uses: List[Use], edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        uses |> List.foldLeft(acc -> uze -> addEdgesFromUse(uze, acc), edges)

    def addEdgesFromUse(uze: Use, edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        let coerced = coerce(uze);
        let src = coerced#parent;
        let qualifiedName = coerce(coerced#name);
        let qualifiers = List.map(ls -> coerce(ls)#s, qualifiedName#qualifiers);
        let name = coerce(qualifiedName#name)#s;
        let edge = if (LocString.isUpperCase(qualifiedName#name)) {
            Edge.Use({
                id = None,
                node = Node.Named({
                    qualifiers = qualifiers,
                    name = name
                })
            })
        } else match List.last(qualifiers) {
            case Some(last) => Edge.Use({
                id = Some(name),
                node = Node.Named({
                    qualifiers = List.take(List.size(qualifiers) - 1, qualifiers),
                    name = last
                })
            })
            case None => Edge.Use({
                id = Some(name),
                node = Node.Top
            })
        };
        edges |> MultiMap.insert(src, edge)

    def addEdgesFromModules(modules: List[Module], edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        modules |> List.foldLeft(acc -> module -> addEdgesFromModule(module, acc), edges)

    def addEdgesFromModule(module: Module, edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        let coerced = coerce(module);
        let parent = coerced#parent;
        let node = coerce(coerced#scope)#node;
        edges
            |> MultiMap.insert(parent, Edge.Module({ module = module }))
            |> MultiMap.insert(node, Edge.Parent({ parent = parent }))
            |> addEdgesFromScope(coerced#scope)

    def addEdgesFromAliases(aliases: List[TypeAlias], edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        aliases |> List.foldLeft(acc -> aliass -> addEdgesFromAlias(aliass, acc), edges)

    def addEdgesFromAlias(typeAlias: TypeAlias, edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        let coerced = coerce(typeAlias);
        let parent = coerced#parent;
        edges |> MultiMap.insert(parent, Edge.TypeAlias({ typeAlias = typeAlias }))

    def addEdgesFromEnums(enums: List[Enum], edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        enums |> List.foldLeft(acc -> eenum -> addEdgesFromEnum(eenum, acc), edges)

    def addEdgesFromEnum(eenum: Enum, edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        let coerced = coerce(eenum);
        let parent = coerced#parent;
        edges |> MultiMap.insert(parent, Edge.Enum({ eenum = eenum }))

    def addEdgesFromDefs(defs: List[Def], edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        defs |> List.foldLeft(acc -> deff -> addEdgesFromDef(deff, acc), edges)

    def addEdgesFromDef(deff: Def, edges: MultiMap[Node, Edge]): MultiMap[Node, Edge] =
        let coerced = coerce(deff);
        let parent = coerced#parent;
        edges |> MultiMap.insert(parent, Edge.Def({ deff = deff }))

}
