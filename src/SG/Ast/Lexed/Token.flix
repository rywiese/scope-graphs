pub mod SG.Ast.Lexed.Token {
    use SG.Ast.Shared.Location.LocRange

    pub def getKind(t: Token): TokenKind = match t {
        case Int(_, _) => TokenKind.Int
        case Name(_, _) => TokenKind.Name
        case Space(_) => TokenKind.Space
        case Comment(_, _) => TokenKind.Comment
        case ParenSoftLeft(_) => TokenKind.ParenSoftLeft
        case ParenSoftRight(_) => TokenKind.ParenSoftRight
        case ParenCurlyLeft(_) => TokenKind.ParenCurlyLeft
        case ParenCurlyRight(_) => TokenKind.ParenCurlyRight
        case ParenSquareLeft(_) => TokenKind.ParenSquareLeft
        case ParenSquareRight(_) => TokenKind.ParenSquareRight
        case Comma(_) => TokenKind.Comma
        case Period(_) => TokenKind.Period
        case Colon(_) => TokenKind.Colon
        case Semicolon(_) => TokenKind.Semicolon
        case Eq(_) => TokenKind.Eq
    }

    pub def getLoc(t: Token): LocRange = match t {
        case Int(_, loc) => loc
        case Name(_, loc) => loc
        case Space(loc) => loc
        case Comment(_, loc) => loc
        case ParenSoftLeft(loc) => loc
        case ParenSoftRight(loc) => loc
        case ParenCurlyLeft(loc) => loc
        case ParenCurlyRight(loc) => loc
        case ParenSquareLeft(loc) => loc
        case ParenSquareRight(loc) => loc
        case Comma(loc) => loc
        case Period(loc) => loc
        case Colon(loc) => loc
        case Semicolon(loc) => loc
        case Eq(loc) => loc
    }

    pub def toString(t: Token): String = {
        use SG.Util.String.trimWithDots;
        let max = 10;
        def wrap(s) = "'${s}'";
        match t {
            case Int(i, _) => "${i}" |> trimWithDots(max) |> wrap
            case Name(name, _) => "${name}" |> trimWithDots(max) |> wrap
            case Space(_) => "<space>"
            case Comment(comment, _) => "//${comment}" |> trimWithDots(max) |> wrap
            case ParenSoftLeft(_) => "(" |> wrap
            case ParenSoftRight(_) => ")" |> wrap
            case ParenCurlyLeft(_) => "{" |> wrap
            case ParenCurlyRight(_) => "}" |> wrap
            case ParenSquareLeft(_) => "[" |> wrap
            case ParenSquareRight(_) => "]" |> wrap
            case Comma(_) => "," |> wrap
            case Period(_) => "." |> wrap
            case Colon(_) => ":" |> wrap
            case Semicolon(_) => ";" |> wrap
            case Eq(_) => "=" |> wrap
        }
    }

    pub enum TokenKind with Eq {
        case Int
        case Name
        case Space
        case Comment
        case ParenSoftLeft
        case ParenSoftRight
        case ParenCurlyLeft
        case ParenCurlyRight
        case ParenSquareLeft
        case ParenSquareRight
        case Comma
        case Period
        case Colon
        case Semicolon
        case Eq
    }


    instance ToString[TokenKind] {
        pub def toString(kind: TokenKind): String =
            TokenKind.toString(kind)
    }
}
