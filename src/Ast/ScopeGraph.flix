mod SG.Ast.ScopeGraph {
    use Order.compare
    use ToString.toString

    use SG.Ast.Shared.Access

    pub enum Scope[r: Eff]({
        parent = Option[Scope[r]],
        uses = MutList[Scope[r], r],
        modules = MutList[Module[r], r],
        aliases = MutList[TypeAlias, r],
        enums = MutList[Enum, r],
        defs = MutList[Def, r]
    })

    pub enum Module[r: Eff]({
        ac = Access,
        scope = Scope[r]
    })

    pub enum TypeAlias({
        ac = Access
    })

    pub enum Enum({
        ac = Access
    })

    pub enum Def({
        ac = Access
    })

    pub enum Name({
        qualifiers = List[String],
        name = String
    })

    mod Scope {
        use SG.Ast.ScopeGraph.Def
        use SG.Ast.ScopeGraph.Enum
        use SG.Ast.ScopeGraph.Module
        use SG.Ast.ScopeGraph.TypeAlias
        use SG.Ast.ScopeGraph.Scope

        def empty(rc: Region[r], parent: Option[Scope[r]]): Scope[r] \ r =
            Scope.Scope({
                parent = parent,
                uses = MutList.empty(rc),
                modules = MutList.empty(rc),
                aliases = MutList.empty(rc),
                enums = MutList.empty(rc),
                defs = MutList.empty(rc)
            })

        pub def root(rc: Region[r]): Scope[r] \ r =
            empty(rc, None)

        pub def child(rc: Region[r], parent: Scope[r]): Scope[r] \ r =
            empty(rc, Some(parent))

        pub def getParent(scope: Scope[r]): Option[Scope[r]] = coerce(scope)#parent

        pub def addUses(uses: List[Scope[r]], scope: Scope[r]): Unit \ r =
            coerce(scope)#uses |> MutList.pushAll(uses)

        pub def addModules(modules: List[Module[r]], scope: Scope[r]): Unit \ r =
            coerce(scope)#modules |> MutList.pushAll(modules)

        pub def addAliases(aliases: List[TypeAlias], scope: Scope[r]): Unit \ r =
            coerce(scope)#aliases |> MutList.pushAll(aliases)

        pub def addEnums(enums: List[Enum], scope: Scope[r]): Unit \ r =
            coerce(scope)#enums |> MutList.pushAll(enums)

        pub def addDefs(defs: List[Def], scope: Scope[r]): Unit \ r =
            coerce(scope)#defs |> MutList.pushAll(defs)

    }

    instance Coerce[Name] {
        type Out = { qualifiers = List[String], name = String }

        pub def coerce(x: Name): { qualifiers = List[String], name = String } =
            let Name.Name(r) = x;
            r
    }

    instance Eq[Name] {
        pub def eq(x: Name, y: Name): Bool = toString(x) == toString(y)
    }

    instance Order[Name] {
        pub def compare(x: Name, y: Name): Comparison = compare(toString(x), toString(y))
    }

    instance ToString[Name] {
        pub def toString(x: Name): String =
            let coerced = coerce(x);
            List.append(coerced#qualifiers, coerced#name :: Nil)
                |> String.intercalateChar('.')
    }

    instance Coerce[Scope[r]] {
    
        type Out = {
            parent = Option[Scope[r]],
            uses = MutList[Scope[r], r],
            modules = MutList[Module[r], r],
            aliases = MutList[TypeAlias, r],
            enums = MutList[Enum, r],
            defs = MutList[Def, r]
        }
    
        pub def coerce(x: Scope[r]): {
            parent = Option[Scope[r]],
            uses = MutList[Scope[r], r],
            modules = MutList[Module[r], r],
            aliases = MutList[TypeAlias, r],
            enums = MutList[Enum, r],
            defs = MutList[Def, r]
        } =
            let Scope.Scope(r) = x;
            r
    
    }

}
