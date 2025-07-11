mod ScopeGraph.Parser {
    use Foldable.forEach
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

    pub def parseScope(sc: SC[r]): Ast.Scope \ r + Error = region rc {
        let uses = MutList.empty(rc);
        let modules = MutList.empty(rc);
        let aliases = MutList.empty(rc);
        let enums = MutList.empty(rc);
        let progress = Ref.fresh(rc, true);
        def loop() = {
            Ref.put(false, progress);
            sc !> parseWsAny |> parseModule |> forEach(m -> {
                modules |> MutList.push(m);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseUse |> forEach(u -> {
                uses |> MutList.push(u);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseTypeAlias |> forEach(ta -> {
                aliases |> MutList.push(ta);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseEnum |> forEach(e -> {
                enums |> MutList.push(e);
                Ref.put(true, progress)
            });
            if (Ref.get(progress)) loop() else ()
        };
        loop();
        Ast.mkScope(
            uses |> MutList.toList,
            modules |> MutList.toList,
            aliases |> MutList.toList,
            enums |> MutList.toList
        )
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
    /// If the cursor is at "type " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    pub def parseTypeAlias(sc: SC[r]): Option[Ast.TypeAlias] \ r + Error = {
        if (sc |> parseKeywordOpt("type")) {
            sc !> parseWsAny |> parseKeyword("alias");
            let name = sc !> parseWsAny |> parseWord;
            let args = if (sc |> advanceIfMatchChar('[')) {
                let args1 = sc |> parseCommaSeq(parseWord);
                sc |> parseChar(']');
                args1
            } else Nil;
            sc !> parseWsAny |> parseChar('=');
            let tpe = sc |> parseType;
            Some(Ast.mkTypeAlias(name, args, tpe))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "enum " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    pub def parseEnum(sc: SC[r]): Option[Ast.Enum] \ r + Error = {
        if (sc |> parseKeywordOpt("enum")) {
            let name = sc !> parseWsAny |> parseWord;
            let args = if (sc |> advanceIfMatchChar('[')) {
                let args1 = sc |> parseCommaSeq(parseWord);
                sc |> parseChar(']');
                args1
            } else Nil;
            sc !> parseWsAny |> parseChar('{');
            let cases = sc |> parseRepeated(parseCase);
            sc !> parseWsAny |> parseChar('}');
            Some(Ast.mkEnum(name, args, cases))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "case " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    def parseCase(sc: SC[r]): Option[(Ast.LocString, List[Ast.Tpe])] \ r + Error = {
        if (sc |> parseKeywordOpt("case")) {
            let name = sc !> parseWsAny |> parseWord;
            if (sc |> advanceIfMatchChar('(')) {
                let types = sc |> parseCommaSeq(parseType);
                sc !> parseWsAny |> parseChar(')');
                Some((name, types))
            } else {
                Some((name, Nil))
            }
        } else {
            None
        }
    }

    /// Handles prefix whitespace.
    def parseType(sc: SC[r]): Ast.Tpe \ r + Error = {
        let base = Ast.Tpe.Base(sc !> parseWsAny |> parseName);
        if (sc |> advanceIfMatchChar('[')) {
            let args = sc |> parseCommaSeq(parseType);
            sc |> parseChar(']');
            Ast.Tpe.App(base, args)
        } else {
            base
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
        def loop(acc) = match sc |> parseWordOpt {
            case Some(n) =>
                match sc |> parseCharOpt('.') {
                    case true => loop(n :: acc)
                    case false => Ast.mkName(acc |> List.reverse, n)
                }
            case None =>
                Ast.mkName(acc |> List.reverse, sc |> parseWord)
        };
        loop(Nil)
    }

    ///
    /// `f` does not need to handle prefix whitespace.
    ///
    /// Handles prefix whitespace.
    def parseCommaSeq(f: SC[r] -> t \ ef, sc: SC[r]): List[t] \ ef + r = {
        def loop(acc) = {
            let x = sc !> parseWsAny |> f;
            if (sc !> parseWsAny |> advanceIfMatchChar(',')) {
                loop(x :: acc)
            } else {
                acc |> List.reverse
            }
        };
        loop(Nil)
    }

    ///
    /// `f` does not need to handle prefix whitespace.
    ///
    /// Handles prefix whitespace.
    def parseRepeated(f: SC[r] -> Option[t] \ ef, sc: SC[r]): List[t] \ ef + r = {
        def loop(acc) = {
            match sc !> parseWsAny |> f {
                case Some(v) => loop(v :: acc)
                case None => acc |> List.reverse
            }
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
    def parseKeyword(kw: String, sc: SC[r]): Unit \ r + Error = {
        let delimited = sc |> nthIsP(String.length(kw), c -> not (Char.isLetter(c) or Char.isDigit(c)), true);
        if (delimited and sc |> advanceIfMatchString(kw)) {
            ()
        } else {
            sc |> errExpect("'${kw}'")
        }
    }

    /// DOES NOT handle prefix whitespace.
    def parseWordOpt(sc: SC[r]): Option[Ast.LocString] \ r = {
        let start = sc |> getLoc;
        let head = sc |> peek;
        if (Char.isLetter(head)) {
            let s = sc |> advanceWhileAndCollect(c -> Char.isLetter(c) or Char.isDigit(c));
            let end = sc |> getLoc;
            Some(mkLocString(s, start = start, end))
        } else {
            None
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
        let loc = "${sc |> getLine + 1}:${sc |> getColumn + 1}";
        Error.error("${msg} (${loc})")
    }
}
