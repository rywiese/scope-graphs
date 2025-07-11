mod ScopeGraph.Parser {
    use ScopeGraph.{StringCursor => SC}
    use ScopeGraph.Ast
    use ScopeGraph.StringCursor.advanceIfMatchChar
    use ScopeGraph.StringCursor.advanceIfMatchString
    use ScopeGraph.StringCursor.advanceWhile
    use ScopeGraph.StringCursor.advanceWhileAndCollect
    use ScopeGraph.StringCursor.getColumn
    use ScopeGraph.StringCursor.getLine
    use ScopeGraph.StringCursor.getOffset
    use ScopeGraph.StringCursor.nthIsP
    use ScopeGraph.StringCursor.peek
    use ScopeGraph.StringCursor.eof

    pub eff Error {
        def error(msg: String): Void
    }

    pub def parseProgram(sc: SC[r]): List[Ast.Module] \ r + Error = {
        let scope = sc |> parseScope;
        sc |> parseWsAny;
        if (sc |> eof) {
            scope
        } else {
            sc |> errExpect("EOF")
        }
    }

    pub def parseScope(sc: SC[r]): List[Ast.Module] \ r + Error = {
        def loop(acc) = {
            sc |> parseWsAny;
            match sc |> parseModule {
                case Some(part) => loop(part :: acc)
                case None => acc |> List.reverse
            }
        };
        loop(Nil)
    }

    ///
    /// If the cursor is at "mod " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    pub def parseModule(sc: SC[r]): Option[Ast.Module] \ r + Error = {
        if (sc |> parseKeywordOpt("mod")) {
            let name = sc !> parseWsAny |> parseName;
            sc !> parseWsAny |> parseChar('{');
            let scope = sc |> parseScope;
            sc !> parseWsAny |> parseChar('}');
            Some(Ast.module(name, scope))
        } else {
            None
        }
    }

    /// Handles prefix whitespace.
    pub def parseName(sc: SC[r]): Ast.Name \ r + Error = {
        sc |> parseWsAny;
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

    /// DOES NOT handle prefix whitespace.
    def _parseKeyword(kw: String, sc: SC[r]): Unit \ r + Error = {
        let delimited = sc |> nthIsP(String.length(kw), c -> not Char.isLetter(c) and Char.isDigit(c), true);
        if (delimited and sc |> advanceIfMatchString("mod")) {
            ()
        } else {
            sc |> err("Expected 'mod'")
        }
    }

    /// DOES NOT handle prefix whitespace.
    def parseKeywordOpt(kw: String, sc: SC[r]): Bool \ r = {
        let delimited = sc |> nthIsP(String.length(kw), c -> not (Char.isLetter(c) or Char.isDigit(c)), true);
        if (delimited and sc |> advanceIfMatchString("mod")) {
            true
        } else {
            false
        }
    }

    /// DOES NOT handle prefix whitespace.
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

    /// DOES NOT handle prefix whitespace.
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

    def parseWsAny(sc: SC[r]): Unit \ r =
        sc |> advanceWhile(Char.isWhitespace)

    /// DOES NOT handle prefix whitespace.
    def parseChar(c: Char, sc: SC[r]): Unit \ r + Error = {
        if (sc |> advanceIfMatchChar(c)) {
            ()
        } else {
            sc |> errExpectChar(c)
        }
    }

    /// DOES NOT handle prefix whitespace.
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
