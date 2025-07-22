mod SG.Phase.Scoper {
    use ToString.toString

    use SG.Ast.ParsedAst
    use SG.Ast.ParsedAst.AstNode
    use SG.Ast.ScopeGraph
    use SG.Ast.ScopeGraph.Module
    use SG.Ast.ScopeGraph.Scope
    use SG.Ast.ScopeGraph.TypeAlias
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.LocString

    pub def resolve(rc: Region[r], ast: ParsedAst): Validation[Error, ScopeGraph.ScopedAst[r]] \ r = {
        Validation.Success({scope = buildScopeGraph(rc, ast)})
    }

    pub def buildScopeGraph(
        rc: Region[r],
        parsedAst: ParsedAst
    ): ScopeGraph.Scope[r] \ r =
        let root = ScopeGraph.Scope.root(rc);
        let dag = buildScopeDag(rc, root, parsedAst);
        foreach(ast <- Map.valuesOf(parsedAst#m)) {
            visitScope(dag, ast, root)
        };
        root //!> visitScope(rc, dag, parsedAst)

    // TODO: Name this and figure out where to put it
    def extractName(name: ParsedAst.Name): ScopeGraph.Name =
        let coerced = coerce(name);
        ScopeGraph.Name.Name({
            name = LocString.getString(coerced#name),
            qualifiers = List.map(LocString.getString, coerced#qualifiers)
        })

    // TODO: Name this and figure out where to put it
    def lookupNode(
        name: ScopeGraph.Name,
        dag: Map[ScopeGraph.Name, ScopeGraph.Scope[r]]
    ): ScopeGraph.Scope[r] =
        match Map.get(name, dag) {
            case Some(node) => node
            case None => bug!("Missing module name from dag: ${name}")
        }

    // Phase 1: Building the DAG

    def buildScopeDag(
        rc: Region[r],
        root: ScopeGraph.Scope[r],
        scope: ParsedAst
    ): Map[ScopeGraph.Name, ScopeGraph.Scope[r]] \ r =
        let dag = MutMap.empty(rc);
        foreach(ast <- Map.valuesOf(scope#m)) {
            addScopeToDag(rc, root, ast, dag)
        };
            // !> addScopeToDag(rc, root, scope)
        dag |> MutMap.toMap

    def addScopeToDag(
        rc: Region[rs],
        parent: ScopeGraph.Scope[rs],
        scope: ParsedAst.Scope,
        dag: MutMap[ScopeGraph.Name, ScopeGraph.Scope[rs], rd]
    ): Unit \ rs + rd =
        foreach(module <- coerce(scope)#modules) {
            addModuleToDag(rc, parent, module, dag)
        }

    def addModuleToDag(
        rc: Region[rs],
        parent: ScopeGraph.Scope[rs],
        module: ParsedAst.Module,
        dag: MutMap[ScopeGraph.Name, ScopeGraph.Scope[rs], rd]
    ): Unit \ rs + rd =
        let coerced = coerce(module);
        let name = extractName(coerced#name);
        let scope = MutMap.getOrElsePut(name, ScopeGraph.Scope.child(rc, parent), dag); // ideally getOrElsePut takes a function that produces the value only if there is not a value for the key
        addScopeToDag(rc, scope, coerced#scope, dag)

    // Phase 2: Adding edges

    def visitScope(
        dag: Map[ScopeGraph.Name, ScopeGraph.Scope[r]],
        scope: ParsedAst.Scope,
        node: ScopeGraph.Scope[r]
    ): Unit \ r =
        let coerced = coerce(scope);
        node
            !> Scope.addUses(List.map(visitUse(dag), coerced#uses))
            !> Scope.addModules(List.map(visitModule, coerced#modules))
            !> Scope.addAliases(List.map(visitTypeAlias, coerced#aliases))
            !> Scope.addEnums(List.map(visitEnum, coerced#enums))
            !> Scope.addDefs(List.map(visitDef, coerced#defs));
        ()

    def visitUse(
        dag: Map[ScopeGraph.Name, ScopeGraph.Scope[r]],
        uze: ParsedAst.Use
    ): ScopeGraph.Scope[r] =
        let name = extractName(coerce(uze)#name);
        lookupNode(name, dag)

    def visitModule(
        module: ParsedAst.Module
    ): ScopeGraph.Module =
        let coerced = coerce(module);
        ScopeGraph.Module.Module({
            ac = coerced#ac,
            name = extractName(coerced#name)
        })

    def visitTypeAlias(typeAlias: ParsedAst.TypeAlias): ScopeGraph.TypeAlias =
        let coerced = coerce(typeAlias);
        ScopeGraph.TypeAlias.TypeAlias({
            ac = coerced#ac
        })

    def visitEnum(eenum: ParsedAst.Enum): ScopeGraph.Enum =
        let coerced = coerce(eenum);
        ScopeGraph.Enum.Enum({
            ac = coerced#ac
        })

    def visitDef(deff: ParsedAst.Def): ScopeGraph.Def =
        let coerced = coerce(deff);
        ScopeGraph.Def.Def({
            ac = coerced#ac
        })

}
