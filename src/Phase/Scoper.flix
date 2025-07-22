mod ScopeGraph.Phase.Scoper {
    use ScopeGraph.Ast.Shared.Location.LocString
    use ToString.toString
    use ScopeGraph.Ast.ParsedAst.AstNode
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.ScopeGraph
    use ScopeGraph.Ast.ScopeGraph.Module
    use ScopeGraph.Ast.ScopeGraph.Scope
    use ScopeGraph.Ast.ScopeGraph.TypeAlias

    pub def buildScopeGraph(
        rc: Region[r],
        parsedAst: ParsedAst.Scope
    ): ScopeGraph.Scope[r] \ r =
        let root = ScopeGraph.Scope.root(rc);
        let dag = buildScopeDag(rc, root, parsedAst);
        root !> visitScope(rc, dag, parsedAst)

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
        scope: ParsedAst.Scope
    ): Map[ScopeGraph.Name, ScopeGraph.Scope[r]] \ r =
        MutMap.empty(rc)
            !> addScopeToDag(rc, root, scope)
            |> MutMap.toMap
    
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
        rc: Region[r],
        dag: Map[ScopeGraph.Name, ScopeGraph.Scope[r]],
        scope: ParsedAst.Scope,
        node: ScopeGraph.Scope[r]
    ): Unit \ r =
        let coerced = coerce(scope);
        node
            !> Scope.addUses(List.map(visitUse(dag), coerced#uses))
            !> Scope.addModules(List.map(visitModule(rc, dag), coerced#modules))
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
        rc: Region[r],
        dag: Map[ScopeGraph.Name, ScopeGraph.Scope[r]],
        module: ParsedAst.Module
    ): ScopeGraph.Module[r] \ r =
        let coerced = coerce(module);
        let name = extractName(coerced#name);
        ScopeGraph.Module.Module({
            ac = coerced#ac,
            scope = lookupNode(name, dag) !> visitScope(rc, dag, coerced#scope)
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
