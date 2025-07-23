mod SG.Ast.ScopeGraph {
    use SG.Ast.ParsedAst.AstNode.TypeAlias
    use SG.Ast.ParsedAst.Exp // Make this shared
    use SG.Ast.ScopeGraph.Enum
    use SG.Ast.ParsedAst.EnumCase // Make this shared
    use SG.Ast.ParsedAst.Tpe // Make this shared
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.ParsedAst.Name // Make this shared
    use Order.compare
    use ToString.toString

    use SG.Ast.Shared.Access
    use SG.Ast.Shared.Location.LocRange

    pub type alias ScopedAst = {
        top = Scope, // maybe this should be a list as well
        edges = MultiMap[Node, Edge]
    }

    pub enum Scope({
        uses = List[Use],
        modules = List[Module],
        aliases = List[TypeAlias],
        enums = List[Enum],
        defs = List[Def],
        node = Node
    })

    pub enum Use({
        ac = Access,
        name = Name,
        renaming = Option[LocString],
        loc = LocRange,
        parent = Node
    })

    pub enum Module({
        ac = Access,
        loc = LocRange,
        scope = Scope,
        parent = Node
    })

    pub enum TypeAlias({
        ac = Access,
        name = LocString,
        args = List[LocString],
        tpe = Tpe,
        loc = LocRange,
        parent = Node
    })

    pub enum Enum({
        ac = Access,
        name = LocString,
        args = List[LocString],
        cases = List[EnumCase],
        loc = LocRange,
        parent = Node
    })

    pub enum Def({
        ac = Access,
        name = LocString,
        args = List[(LocString, Tpe)],
        tpe = Tpe,
        exp = Exp,
        loc = LocRange,
        parent = Node
    })

    pub enum Node {
        case Top
        case Named({ qualifiers = List[String], name = String })
    }

    mod Node {
        pub def fullyQualifiedName(node: Node): String =
            match node {
                case Top => ""
                case Named({ qualifiers, name }) =>
                    (qualifiers ++ (name :: Nil)) |> String.intercalateChar('.')
            }
    }

    pub enum Edge {
        case Parent({ parent = Node })
        case Use({ id = Option[String], node = Node })
        case Module({ module = Module })
        case TypeAlias({ typeAlias = TypeAlias })
        case Enum({ eenum = Enum })
        case Def({ deff = Def })
    }

    instance SemiGroup[Scope] {
        // TODO: maybe rethink this?
        pub def combine(x: Scope, y: Scope): Scope =
            let scope1 = coerce(x);
            let scope2 = coerce(y);
            Scope.Scope({
                uses = scope1#uses ++ scope2#uses,
                modules = scope1#modules ++ scope2#modules,
                aliases = scope1#aliases ++ scope2#aliases,
                enums = scope1#enums ++ scope2#enums,
                defs = scope1#defs ++ scope2#defs,
                node = if (scope1#node == scope2#node)
                    scope1#node
                else
                    bug!("Cannot combine nodes of different scopes")
            })
    }

    instance Monoid[Scope] {    
        pub def empty(_unit: Unit): Scope = Scope.Scope({
            uses = Nil,
            modules = Nil,
            aliases = Nil,
            enums = Nil,
            defs = Nil,
            node = Node.Top
        })
    }

    instance Coerce[Scope] {

        type Out = {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            node = Node
        }

        pub def coerce(x: Scope): {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            node = Node
        } =
            let Scope.Scope(r) = x;
            r

    }

    instance Coerce[Use] {

        type Out = {
            ac = Access,
            name = Name,
            renaming = Option[LocString],
            loc = LocRange,
            parent = Node
        }

        pub def coerce(x: Use): {
            ac = Access,
            name = Name,
            renaming = Option[LocString],
            loc = LocRange,
            parent = Node
        } =
            let Use.Use(r) = x;
            r

    }

    instance Coerce[Module] {

        type Out = {
            ac = Access,
            loc = LocRange,
            scope = Scope,
            parent = Node
        }

        pub def coerce(x: Module): {
            ac = Access,
            loc = LocRange,
            scope = Scope,
            parent = Node
        } =
            let Module.Module(r) = x;
            r

    }

    instance Coerce[TypeAlias] {
    
        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange,
            parent = Node
        }
    
        pub def coerce(x: TypeAlias): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            tpe = Tpe,
            loc = LocRange,
            parent = Node
        } =
            let TypeAlias.TypeAlias(r) = x;
            r
    
    }

    instance Coerce[Enum] {
    
        type Out = {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange,
            parent = Node
        }
    
        pub def coerce(x: Enum): {
            ac = Access,
            name = LocString,
            args = List[LocString],
            cases = List[EnumCase],
            loc = LocRange,
            parent = Node
        } =
            let Enum.Enum(r) = x;
            r
    
    }

    instance Coerce[Def] {

        type Out = {
            ac = Access,
            name = LocString,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange,
            parent = Node
        }

        pub def coerce(x: Def): {
            ac = Access,
            name = LocString,
            args = List[(LocString, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange,
            parent = Node
        } =
            let Def.Def(r) = x;
            r

    }

    instance Eq[Node] {
        pub def eq(x: Node, y: Node): Bool =
            Node.fullyQualifiedName(x) == Node.fullyQualifiedName(y)
    }

    instance Order[Node] {
        pub def compare(x: Node, y: Node): Comparison =
            compare(Node.fullyQualifiedName(x), Node.fullyQualifiedName(y))
    }

    instance Eq[Edge] {
        pub def eq(x: Edge, y: Edge): Bool = ???
    }

    instance Order[Edge] {
        pub def compare(x: Edge, y: Edge): Comparison = ???
    }

}
