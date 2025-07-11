mod ScopeGraph.Parser {
    use ScopeGraph.Parsing.{StringCursor => SC}
    use ScopeGraph.Ast
    use ScopeGraph.Parsing.StringCursor.advanceIfMatchChar
    use ScopeGraph.Parsing.StringCursor.advanceIfMatchString
    use ScopeGraph.Parsing.StringCursor.advanceWhile
    use ScopeGraph.Parsing.StringCursor.advanceWhileAndCollect
    use ScopeGraph.Parsing.StringCursor.eof
    use ScopeGraph.Parsing.StringCursor.getColumn
    use ScopeGraph.Parsing.StringCursor.getLine
    use ScopeGraph.Parsing.StringCursor.getOffset
    use ScopeGraph.Parsing.StringCursor.nthIsP
    use ScopeGraph.Parsing.StringCursor.peek

    pub eff Error {
        def error(msg: String): Void
    }

    /// Like parseScope but also errors for unconsumed input.
    pub def parseProgram(sc: SC[r]): Ast.Scope \ r + Error = {
        let scope = sc |> parseScope;
        sc |> parseWsAny;
        if (sc |> eof) {
            scope
        } else {
            sc |> errExpect("EOF")
        }
    }

    pub def parseScope(sc: SC[r]): Ast.Scope \ r + Error = {
        def loop(uses, modules) = {
            sc |> parseWsAny;
            match sc |> parseModule {
                case Some(module) =>
                    loop(uses, module :: modules)
                case None => match sc |> parseUse {
                    case Some(usage) =>
                        loop(usage :: uses, modules)
                    case None =>
                        Ast.mkScope(uses |> List.reverse, modules |> List.reverse)
                }
            }
        };
        loop(Nil, Nil)
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
            Some(Ast.mkModule(name, scope))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "use " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    pub def parseUse(sc: SC[r]): Option[Ast.Use] \ r + Error = {
        if (sc |> parseKeywordOpt("use")) {
            let name = sc !> parseWsAny |> parseName;
            sc |> parseWsAny;
            if (sc |> parseKeywordOpt("as")) {
                let renaming = sc !> parseWsAny |> parseWord;
                Some(Ast.mkRenamedUse(name, renaming))
            } else {
                Some(Ast.mkUse(name))
            }
        } else {
            None
        }
    }

    /// Handles prefix whitespace.
    pub def parseName(sc: SC[r]): Ast.Name \ r + Error = {
        sc |> parseWsAny;
        def loop(acc) = match sc |> parseUpperWordOpt {
            case Some(n) =>
                match sc |> parseCharOpt('.') {
                    case true => loop(n :: acc)
                    case false => Ast.mkName(acc |> List.reverse, n)
                }
            case None =>
                Ast.mkName(acc |> List.reverse, sc |> parseLowerWord)
        };
        loop(Nil)
    }

    /// DOES NOT handle prefix whitespace.
    def parseKeywordOpt(kw: String, sc: SC[r]): Bool \ r = {
        let delimited = sc |> nthIsP(String.length(kw), c -> not (Char.isLetter(c) or Char.isDigit(c)), true);
        if (delimited and sc |> advanceIfMatchString(kw)) {
            true
        } else {
            false
        }
    }

    /// DOES NOT handle prefix whitespace.
    def parseUpperWordOpt(sc: SC[r]): Option[Ast.LocString] \ r = {
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
    def parseLowerWord(sc: SC[r]): Ast.LocString \ r + Error = {
        let start = sc |> getLoc;
        let head = sc |> peek;
        if (Char.isLetter(head) and Char.isLowerCase(head)) {
            let s = sc |> advanceWhileAndCollect(c -> Char.isLetter(c) or Char.isDigit(c));
            let end = sc |> getLoc;
            mkLocString(s, start = start, end)
        } else {
            sc |> errExpect("letter")
        }
    }

    /// DOES NOT handle prefix whitespace.
    def parseWord(sc: SC[r]): Ast.LocString \ r + Error = {
        let start = sc |> getLoc;
        let head = sc |> peek;
        if (Char.isLetter(head)) {
            let s = sc |> advanceWhileAndCollect(c -> Char.isLetter(c) or Char.isDigit(c));
            let end = sc |> getLoc;
            mkLocString(s, start = start, end)
        } else {
            sc |> errExpect("letter")
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
        Ast.mkLoc(line = getLine(sc) + 1, column = getColumn(sc) + 1, getOffset(sc))

    def mkLocString(s: String, start: {start = Ast.Loc}, end: Ast.Loc): Ast.LocString =
        Ast.mkLocString(s, Ast.mkLocRange(start, end))

    def errExpect(expected: String, sc: SC[r]): t \ r + Error =
        err("Expected ${expected}, found '${sc |> peek}'", sc)

    def errExpectChar(expected: Char, sc: SC[r]): t \ r + Error =
        err("Expected '${expected}', found '${sc |> peek}'", sc)

    def err(msg: String, sc: SC[r]): t \ r + Error = {
        let loc = "${sc |> getLine}:${sc |> getColumn}";
        Error.error("${msg} (${loc})")
    }
}
