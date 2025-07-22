mod ScopeGraph.Ast.ScopeGraph {
    use Order.compare
    use ToString.toString

    pub struct Scope[r] {
        mut parent: Option[Scope[r]],
        uses: MutList[Scope[r], r],
        modules: MutList[Module[r], r],
        aliases: MutList[TypeAlias, r],
        enums: MutList[Enum, r],
        defs: MutList[Def, r]
    }

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
        use ScopeGraph.Ast.ScopeGraph.Def
        use ScopeGraph.Ast.ScopeGraph.Enum
        use ScopeGraph.Ast.ScopeGraph.Module
        use ScopeGraph.Ast.ScopeGraph.TypeAlias
        use ScopeGraph.Ast.ScopeGraph.Scope.setParent

        pub def root(rc: Region[r]): Scope[r] \ r =
            new Scope @ rc {
                parent = None,
                uses = MutList.empty(rc),
                modules = MutList.empty(rc),
                aliases = MutList.empty(rc),
                enums = MutList.empty(rc),
                defs = MutList.empty(rc)
            }

        pub def child(rc: Region[r], parent: Scope[r]): Scope[r] \ r =
            root(rc) !> setParent(parent)

        def setParent(parent: Scope[r], scope: Scope[r]): Unit \ r =
            match scope->parent {
                case None => scope->parent = Some(parent)
                case Some(_) => bug!("Should not modify parent once assigned")
            }

        pub def getParent(scope: Scope[r]): Option[Scope[r]] \ r = scope->parent

        pub def addUses(uses: List[Scope[r]], scope: Scope[r]): Unit \ r =
            scope->uses |> MutList.pushAll(uses)
        
        pub def addModules(modules: List[Module[r]], scope: Scope[r]): Unit \ r =
            scope->modules |> MutList.pushAll(modules)
        
        pub def addAliases(aliases: List[TypeAlias], scope: Scope[r]): Unit \ r =
            scope->aliases |> MutList.pushAll(aliases)
        
        pub def addEnums(enums: List[Enum], scope: Scope[r]): Unit \ r =
            scope->enums |> MutList.pushAll(enums)
        
        pub def addDefs(defs: List[Def], scope: Scope[r]): Unit \ r =
            scope->defs |> MutList.pushAll(defs)

        // pub def prettyPrint(scope: Scope[r]): Unit \ IO + r =
        //     println("parent: ${scope->parent}")

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

}
