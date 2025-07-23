mod SG.Ast.Shared {

    pub enum Access with Eq, ToString, Hash {
        case Public
        case Private
    }
}
