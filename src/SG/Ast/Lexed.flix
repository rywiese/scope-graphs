pub mod SG.Ast.Lexed {
    use SG.Ast.Shared.Location.Source
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange

    pub enum Token {
        case Int(Int32, LocRange)
        case Name(String, LocRange)
        case Space(LocRange)
        case Comment(String, LocRange)
        case ParenSoftLeft(LocRange)
        case ParenSoftRight(LocRange)
        case ParenCurlyLeft(LocRange)
        case ParenCurlyRight(LocRange)
        case ParenSquareLeft(LocRange)
        case ParenSquareRight(LocRange)
        case Comma(LocRange)
        case Period(LocRange)
        case Colon(LocRange)
        case Semicolon(LocRange)
        case Eq(LocRange)
    }


    instance ToString[Token] {
        pub def toString(x: Token): String =
            Token.toString(x)
    }

    instance Located[Token] {
        pub def getLoc(x: Token): LocRange =
            Token.getLoc(x)
    }
}
