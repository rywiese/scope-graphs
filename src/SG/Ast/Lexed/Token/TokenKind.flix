pub mod SG.Ast.Lexed.Token.TokenKind {
    pub def toString(kind: TokenKind): String = {
        def wrap(s) = "'${s}'";
        match kind {
            case Int => "<int>"
            case Name => "<word>"
            case Space => "<space>"
            case Comment => "<comment>"
            case ParenSoftLeft => "(" |> wrap
            case ParenSoftRight => ")" |> wrap
            case ParenCurlyLeft => "{" |> wrap
            case ParenCurlyRight => "}" |> wrap
            case ParenSquareLeft => "[" |> wrap
            case ParenSquareRight => "]" |> wrap
            case Comma => "," |> wrap
            case Period => "." |> wrap
            case Colon => ":" |> wrap
            case Semicolon => ";" |> wrap
            case Eq => "=" |> wrap
        }
    }
}
