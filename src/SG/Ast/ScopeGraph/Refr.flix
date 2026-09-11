pub mod SG.Ast.ScopeGraph.Refr {
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
