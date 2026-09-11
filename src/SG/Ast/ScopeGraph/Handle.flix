pub mod SG.Ast.ScopeGraph.Handle {
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
