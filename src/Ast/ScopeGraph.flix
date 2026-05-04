mod SG.Ast.ScopeGraph {
    use Comparison.EqualTo
    use Order.compare
    use SG.Ast.Shared.Access
    use SG.Ast.Shared.Location
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.Source
    use SG.Ast.ScopeGraph.Handle.matches

    pub enum ScopeGraph({
        nodes = Map[Source, Decls],
        edges = Map[Scope, List[Edge]]
    })

    type alias Path = List[Edge]

    pub enum EdgeType with Eq {
        case Dec
        case Use
        case Lex
    }

    instance Order[EdgeType] {
        pub def compare(x: EdgeType, y: EdgeType): Comparison =
            def rank(e: EdgeType) = {
                match e {
                    case EdgeType.Dec => 1
                    case EdgeType.Lex => 2
                    case EdgeType.Use => 3
                }
            };
            rank(x) <=> rank(y)
    }

    pub def edges(graph: ScopeGraph): Map[Scope, List[Edge]] =
        let ScopeGraph({ nodes = _, edges }) = graph;
        edges

    def outgoingEdges(node: Scope, graph: ScopeGraph): List[Edge] =
        graph
            |> edges
            |> Map.get(node)
            |> Option.getWithDefault(List.empty())

    pub def findHandle(src: Refr, graph: ScopeGraph): Option[Handle] =
        graph
            |> findDecls(src, true)
            |> List.head
            |> Option.map(Decl.handle)

    // `uses` true whether to include `Use` edges while BFSing.
    // this is a temporary hack until regex support is in place.
    pub def findDecls(src: Refr, uses: Bool, graph: ScopeGraph): List[Decl] =
        match src {
            case Refr.Qualified(_, qualifier, _) => graph
                |> findDecls(qualifier, uses)
                |> List.flatMap(decl -> decl
                    |> Decl.associatedScope
                    |> Option.map(node -> graph |> bfs(node, src, uses))
                    |> Option.getWithDefault(Nil)
                )
                // TODO: filter out long paths (multiple nodes will have different bfs results)
                // TODO: the below is maybe too aggressive a filtering
                |> List.head
                |> Option.toList
            case Refr.Unqualified(_, _, referencedIn) => graph |> bfs(referencedIn, src, uses)
        }

    // All paths of length n where n is the shortest path
    // `uses` true whether to include `Use` edges while BFSing.
    // this is a temporary hack until regex support is in place.
    def bfs(src: Scope, refr: Refr, uses: Bool, graph: ScopeGraph): List[Decl] =
        region rc {
            let queue = MutList.empty(rc) !> MutList.push(src);
            let explored = MutSet.empty(rc) !> MutSet.add(src);
            def loop() = {
                let result = queue
                    |> MutList.pop
                    |> Option.map(node -> {
                        let outgoing = graph
                            |> outgoingEdges(node)
                            |> List.sortBy(Edge.edgeType);
                        let (candidates, successors) = outgoing
                            |> List.map(edge -> match edge {
                                case Edge.Decl(decl) => (decl :: Nil, Nil)
                                case Edge.Lex(parent) => (Nil, parent :: Nil)
                                case Edge.Use(usage) => if (uses) {
                                    let candidates = graph |> findDecls(usage, false);
                                    let successors = candidates |> List.filterMap(Decl.associatedScope);
                                    (candidates, successors)
                                } else {
                                    (Nil, Nil)
                                }
                            })
                            |> List.foldLeft(pair -> acc -> {
                                let (candidates, successors) = pair;
                                let (accCandidates, accSuccessors) = acc;
                                (candidates ++ accCandidates, successors ++ accSuccessors)
                            }, (Nil, Nil));
                        let matches = candidates |> List.filter(decl -> refr `matches` decl |> Decl.handle);
                        if (not (matches |> List.isEmpty)) {
                            Ok(matches)
                        } else {
                            Err(successors)
                        }
                    })
                    |> Option.getWithDefault(Ok(List.empty()));
                match result {
                    case Ok(found) => found
                    case Err(successors) => {
                        foreach(node <- successors) {
                            if (not (node `MutSet.memberOf` explored)) {
                                queue |> MutList.push(node);
                                explored |> MutSet.add(node)
                            } else ()
                        };
                        loop()
                    }
                }
            };
            loop()
        }


    pub enum Decls({
        uses = List[Use],
        modules = List[Module],
        aliases = List[TypeAlias],
        enums = List[Enum],
        defs = List[Def],
        loc = LocRange
    })

    pub enum Use({
        ac = Access,
        usage = Refr,
        renaming = Option[Handle], // finder
        loc = LocRange
    })

    pub enum Module({
        ac = Access,
        handle = Handle,
        introduces = Scope,
        decls = Decls,
        loc = LocRange
    })

    pub enum TypeAlias({
        ac = Access,
        handle = Handle,
        args = List[Handle],
        tpe = Tpe,
        loc = LocRange
    })

    pub enum Enum({
        ac = Access,
        handle = Handle,
        introduces = Scope,
        args = List[Handle],
        cases = List[EnumCase],
        loc = LocRange
    })

    pub enum EnumCase({
        ac = Access,
        handle = Handle,
        terms = List[Tpe],
        loc = LocRange
    })

    pub enum Def({
        ac = Access,
        handle = Handle,
        args = List[(Handle, Tpe)],
        tpe = Tpe,
        exp = Exp,
        loc = LocRange
    })

    pub enum Exp {
        case Let(Handle, Exp, Exp, LocRange)
        case Stmt(Exp, Exp, LocRange)
        case App(Exp, List[Exp], LocRange)
        case Refr(Refr, LocRange)
        case Int(Int32, LocRange)
        case Bool(Bool, LocRange)
    }

    pub enum Tpe {
        case Base(Refr)
        case BoundVar(Refr)
        case FreeVar(Handle)
        case App(Tpe, List[Tpe], LocRange)
    }

    pub enum Handle(Kind, LocString)

    mod Handle {
        use SG.Ast.ScopeGraph.Kind
        use SG.Ast.ScopeGraph.Decl
        use SG.Ast.ScopeGraph.Handle
        use SG.Ast.ScopeGraph.Refr
        use SG.Ast.ScopeGraph.Scope
        use SG.Ast.Shared.Location.LocString

        pub def matches(refr: Refr, handle: Handle): Bool =
            let Handle.Handle(handleKind, handleName) = handle;
            let refrKind = refr |> Refr.kind;
            let refrName = refr |> Refr.name;
            (handleKind == Kind.Any or refrKind == Kind.Any or handleKind == refrKind) and (coerce(handleName)#s == coerce(refrName)#s)
            
    }

    pub enum Refr {
        case Unqualified(
            Kind, // maybe we can replace this with a "filter" concept
            LocString,
            Scope // referenced in
        )
        case Qualified(
            Kind,
            Refr, // qualifier
            LocString
        )
    }

    mod Refr {
        use SG.Ast.ScopeGraph.Kind
        use SG.Ast.ScopeGraph.Scope
        use SG.Ast.Shared.Location.LocString

        pub def kind(refr: Refr): Kind =
            match refr {
                case Unqualified(kind, _, _) => kind
                case Qualified(kind, _, _) => kind
            }

        pub def name(refr: Refr): LocString =
            match refr {
                case Unqualified(_, name, _) => name
                case Qualified(_, _, name) => name
            }

        pub def referencedIn(refr: Refr): Scope =
            match refr {
                case Unqualified(_, _, referencedIn) => referencedIn
                case Qualified(_, qualifier, _) => qualifier |> referencedIn
            }
    }

    pub enum AstNode {
        case Decls(Decls)
        case Use(Use)
        case Module(Module)
        case TypeAlias(TypeAlias)
        case Enum(Enum)
        case EnumCase(EnumCase)
        case Def(Def)
        case Exp(Exp)
        case Tpe(Tpe)
        case Refr(Refr)
    }

    // Figure out what to call this... type/kind/category?
    // basically a scoping "primitive"
    pub enum Kind with Eq, Order, ToString {
        case Any
        case Module
        case Tpe
        case Var
    }

    pub enum Scope with Eq, Order, ToString {
        case Scope(Int32)
    }

    mod Scope {
        use SG.Ast.ScopeGraph.Scope

        pub def top(): Scope = Scope.Scope(0)
    }

    pub enum Decl {
        case Module(Module)
        case TypeAlias(TypeAlias)
        case Enum(Enum)
        case EnumCase(EnumCase)
        case Def(Def)
        case TpeVar(Handle)
        case Var(Handle)
        case Rename(Handle)
    }

    mod Decl {
        use SG.Ast.ScopeGraph.Decl
        use SG.Ast.ScopeGraph.Handle
        use SG.Ast.ScopeGraph.Scope

        pub def handle(decl: Decl): Handle =
            match decl {
                case Decl.Module(module) => coerce(module)#handle
                case Decl.TypeAlias(typeAlias) => coerce(typeAlias)#handle
                case Decl.Enum(eenum) => coerce(eenum)#handle
                case Decl.EnumCase(enumCase) => coerce(enumCase)#handle
                case Decl.Def(deff) => coerce(deff)#handle
                case Decl.TpeVar(handle) => handle
                case Decl.Var(handle) => handle
                case Decl.Rename(handle) => handle
            }

        // Only returns "global" scopes, ie those that can be imported
        pub def associatedScope(decl: Decl): Option[Scope] =
            match decl {
                case Module(module) => Some(coerce(module)#introduces)
                case Enum(eenum) => Some(coerce(eenum)#introduces)
                case _ => None
            }

    }

    pub enum Edge {
        case Lex(Scope)
        case Use(Refr)
        case Decl(Decl)
    }

    mod Edge {
        use SG.Ast.ScopeGraph.Decl
        use SG.Ast.ScopeGraph.EdgeType
        use SG.Ast.ScopeGraph.Scope

        pub def edgeType(edge: Edge): EdgeType =
            match edge {
                case Lex(_) => EdgeType.Lex
                case Use(_) => EdgeType.Use
                case Decl(_) => EdgeType.Dec
            }
    }

    // Instances

    instance Coerce[Decls] {
        type Out = {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            loc = LocRange
        }
    
        pub def coerce(x: Decls): {
            uses = List[Use],
            modules = List[Module],
            aliases = List[TypeAlias],
            enums = List[Enum],
            defs = List[Def],
            loc = LocRange
        } =
            let Decls.Decls(r) = x;
            r
    }

    instance Located[Decls] {
        pub def getLoc(x: Decls): LocRange = coerce(x)#loc
    }

    instance Coerce[Use] {
        type Out = {
            ac = Access,
            usage = Refr,
            renaming = Option[Handle],
            loc = LocRange
        }
    
        pub def coerce(x: Use): {
            ac = Access,
            usage = Refr,
            renaming = Option[Handle],
            loc = LocRange
        } =
            let Use.Use(r) = x;
            r
    }

    instance Located[Use] {
        pub def getLoc(x: Use): LocRange = coerce(x)#loc
    }

    instance Coerce[Module] {
        type Out = {
            ac = Access,
            // qualifiers = List[Handle],
            handle = Handle,
            introduces = Scope,
            decls = Decls,
            loc = LocRange
        }
    
        pub def coerce(x: Module): {
            ac = Access,
            // qualifiers = List[Handle],
            handle = Handle,
            introduces = Scope,
            decls = Decls,
            loc = LocRange
        } =
            let Module.Module(r) = x;
            r
    }

    instance Located[Module] {
        pub def getLoc(x: Module): LocRange = coerce(x)#loc
    }

    instance Coerce[TypeAlias] {
        type Out = {
            ac = Access,
            handle = Handle,
            args = List[Handle],
            tpe = Tpe,
            loc = LocRange
        }
    
        pub def coerce(x: TypeAlias): {
            ac = Access,
            handle = Handle,
            args = List[Handle],
            tpe = Tpe,
            loc = LocRange
        } =
            let TypeAlias.TypeAlias(r) = x;
            r
    }

    instance Located[TypeAlias] {
        pub def getLoc(x: TypeAlias): LocRange = coerce(x)#loc
    }

    instance Coerce[Enum] {
        type Out = {
            ac = Access,
            handle = Handle,
            introduces = Scope,
            args = List[Handle],
            cases = List[EnumCase],
            loc = LocRange
        }
    
        pub def coerce(x: Enum): {
            ac = Access,
            handle = Handle,
            introduces = Scope,
            args = List[Handle],
            cases = List[EnumCase],
            loc = LocRange
        } =
            let Enum.Enum(r) = x;
            r
    }

    instance Located[Enum] {
        pub def getLoc(x: Enum): LocRange = coerce(x)#loc
    }

    instance Coerce[EnumCase] {
        type Out = {
            ac = Access,
            handle = Handle,
            terms = List[Tpe],
            loc = LocRange
        }
    
        pub def coerce(x: EnumCase): {
            ac = Access,
            handle = Handle,
            terms = List[Tpe],
            loc = LocRange
        } =
            let EnumCase.EnumCase(r) = x;
            r
    }

    instance Located[EnumCase] {
        pub def getLoc(x: EnumCase): LocRange = coerce(x)#loc
    }

    instance Coerce[Def] {
        type Out = {
            ac = Access,
            handle = Handle,
            args = List[(Handle, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange
        }
    
        pub def coerce(x: Def): {
            ac = Access,
            handle = Handle,
            args = List[(Handle, Tpe)],
            tpe = Tpe,
            exp = Exp,
            loc = LocRange
        } =
            let Def.Def(r) = x;
            r
    }

    instance Located[Def] {
        pub def getLoc(x: Def): LocRange = coerce(x)#loc
    }

    instance Located[Tpe] {
        pub def getLoc(x: Tpe): LocRange =
            match x {
                case Tpe.Base(refr) => refr |> Located.getLoc
                case Tpe.BoundVar(refr) => refr |> Located.getLoc
                case Tpe.FreeVar(handle) => handle |> Located.getLoc
                case Tpe.App(_, _, loc) => loc
            }
    }

    instance Located[Exp] {
        pub def getLoc(x: Exp): LocRange =
            match x {
                case Exp.Let(_, _, _, loc) => loc
                case Exp.Stmt(_, _, loc) => loc
                case Exp.App(_, _, loc) => loc
                case Exp.Refr(_, loc) => loc
                case Exp.Int(_, loc) => loc
                case Exp.Bool(_, loc) => loc
            }
    }

    instance Located[Handle] {
        pub def getLoc(x: Handle): LocRange =
            let Handle.Handle(_, name) = x;
            Located.getLoc(name)
    }

    instance Located[Refr] {
        pub def getLoc(x: Refr): LocRange =
            x |> Refr.name |> Located.getLoc
    }

    instance Located[AstNode] {
        pub def getLoc(x: AstNode): LocRange =
            match x {
                case AstNode.Decls(decls) => decls |> Located.getLoc
                case AstNode.Use(usage) => usage |> Located.getLoc
                case AstNode.Module(module) => module |> Located.getLoc
                case AstNode.TypeAlias(typeAlias) => typeAlias |> Located.getLoc
                case AstNode.Enum(eenum) => eenum |> Located.getLoc
                case AstNode.EnumCase(enumCase) => enumCase |> Located.getLoc
                case AstNode.Def(deff) => deff |> Located.getLoc
                case AstNode.Exp(exp) => exp |> Located.getLoc
                case AstNode.Tpe(tpe) => tpe |> Located.getLoc
                case AstNode.Refr(refr) => refr |> Located.getLoc
            }
    }

}
