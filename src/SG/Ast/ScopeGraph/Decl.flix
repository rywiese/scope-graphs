pub mod SG.Ast.ScopeGraph.Decl {
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
