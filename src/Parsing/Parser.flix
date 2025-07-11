mod ScopeGraph.Parser {
    use ScopeGraph.StringCursor.advanceIfMatchChar
    use ScopeGraph.{StringCursor => SC}
    use ScopeGraph.Ast
    use ScopeGraph.StringCursor.advanceWhile
    use ScopeGraph.StringCursor.advanceWhileAndCollect
    use ScopeGraph.StringCursor.peek
    use ScopeGraph.StringCursor.getLine
    use ScopeGraph.StringCursor.getColumn
    use ScopeGraph.StringCursor.getOffset

    pub eff Error {
        def error(msg: String): Void
    }

    pub def parseName(sc: SC[r]): Ast.Name \ r + Error = {
        sc |> parseWsOpt;
        def loop(acc) = match sc |> parseUpperOpt {
            case Some(n) =>
                match sc |> parseCharOpt('.') {
                    case true => loop(n :: acc)
                    case false => Ast.name(acc |> List.reverse, n)
                }
            case None => match sc |> parseLowerOpt {
                case Some(n) =>
                    Ast.name(acc |> List.reverse, n)
                case None =>
                    sc |> errExpect("letter")
            }
        };
        loop(Nil)
    }

    def parseUpperOpt(sc: SC[r]): Option[Ast.LocString] \ r = {
        let start = sc |> getLoc;
        let head = sc |> peek;
        if (Char.isLetter(head) and Char.isUpperCase(head)) {
            let s = sc |> advanceWhileAndCollect(c -> Char.isLetter(c) or Char.isDigit(c));
            let end = sc |> getLoc;
            Some(mkLocString(s, start = start, end))
        } else {
            None
        }
    }

    def parseLowerOpt(sc: SC[r]): Option[Ast.LocString] \ r = {
        let start = sc |> getLoc;
        let head = sc |> peek;
        if (Char.isLetter(head) and Char.isLowerCase(head)) {
            let s = sc |> advanceWhileAndCollect(c -> Char.isLetter(c) or Char.isDigit(c));
            let end = sc |> getLoc;
            Some(mkLocString(s, start = start, end))
        } else {
            None
        }
    }

    def parseWsOpt(sc: SC[r]): Unit \ r =
        sc |> advanceWhile(Char.isWhitespace)

    def _parseChar(c: Char, sc: SC[r]): Unit \ r + Error = {
        if (sc |> advanceIfMatchChar(c)) {
            ()
        } else {
            sc |> errExpectChar(c)
        }
    }

    def parseCharOpt(c: Char, sc: SC[r]): Bool \ r =
        sc |> advanceIfMatchChar(c)

    def getLoc(sc: SC[r]): Ast.Loc \ r =
        Ast.loc(line = getLine(sc) + 1, column = getColumn(sc) + 1, getOffset(sc))

    def mkLocString(s: String, start: {start = Ast.Loc}, end: Ast.Loc): Ast.LocString =
        Ast.locString(s, Ast.locRange(start, end))

    def errExpect(expected: String, sc: SC[r]): t \ r + Error =
        err("Expected ${expected}, found '${sc |> peek}'", sc)

    def errExpectChar(expected: Char, sc: SC[r]): t \ r + Error =
        err("Expected '${expected}', found '${sc |> peek}'", sc)

    def err(msg: String, sc: SC[r]): t \ r + Error = {
        let loc = "${sc |> getLine}:${sc |> getColumn}";
        Error.error("${msg} (${loc})")
    }
}
