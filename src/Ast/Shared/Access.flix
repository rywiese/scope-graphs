mod ScopeGraph.Ast.Shared {

    pub enum Access with Eq, ToString {
        case Public
        case Private
    }
}
