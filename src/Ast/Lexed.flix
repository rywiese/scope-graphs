mod ScopeGraph.Ast {
    use Lexed.Token

    type alias Lexed = Vector[Token]

    mod Lexed {
        use ScopeGraph.Ast.Shared.Location.LocRange

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

        mod Token {
            use ScopeGraph.Ast.Shared.Location.LocRange

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
                use ScopeGraph.Util.String.trimWithDots;
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

            mod TokenKind {
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

            instance ToString[TokenKind] {
                pub def toString(kind: TokenKind): String =
                    TokenKind.toString(kind)
            }

        }

        instance ToString[Token] {
            pub def toString(x: Token): String =
                Token.toString(x)
        }

    }

}
