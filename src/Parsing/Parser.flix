mod ScopeGraph.Parser {
    use Foldable.forEach
    use Functor.map
    use ScopeGraph.Parsing.{StringCursor => SC}
    use ScopeGraph.Ast
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

    pub def parseProgram(sc: SC[r]): Ast.Scope \ r + Error = {
        let scope = sc |> parseScope;
        sc |> parseWsAny;
        if (sc |> eof) {
            scope
        } else {
            sc |> errExpect("EOF")
        }
    }

    def parseScope(sc: SC[r]): Ast.Scope \ r + Error = region rc {
        let uses = MutList.empty(rc);
        let modules = MutList.empty(rc);
        let aliases = MutList.empty(rc);
        let enums = MutList.empty(rc);
        let defs = MutList.empty(rc);
        let progress = Ref.fresh(rc, true);
        def loop() = {
            Ref.put(false, progress);
            let (ac, _) = sc |> parseAccess;
            sc !> parseWsAny |> parseModule(ac) |> forEach(m -> {
                modules |> MutList.push(m);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseUse(ac) |> forEach(u -> {
                uses |> MutList.push(u);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseTypeAlias(ac) |> forEach(ta -> {
                aliases |> MutList.push(ta);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseEnum(ac) |> forEach(e -> {
                enums |> MutList.push(e);
                Ref.put(true, progress)
            });
            sc !> parseWsAny |> parseDef(ac) |> forEach(d -> {
                defs |> MutList.push(d);
                Ref.put(true, progress)
            });
            if (Ref.get(progress)) loop() else ()
        };
        loop();
        Ast.mkScope(
            uses |> MutList.toList,
            modules |> MutList.toList,
            aliases |> MutList.toList,
            enums |> MutList.toList,
            defs |> MutList.toList
        )
    }

    ///
    /// Treats nothing is `Ast.Access.Private`.
    /// Returns (_, true) if any non-whitespace input was consumed.
    ///
    /// Handles prefix whitespace.
    def parseAccess(sc: SC[r]): (Ast.Access, Bool) \ r = {
        sc |> parseWsAny;
        if (sc |> parseKeywordOpt("pub")) {
            (Ast.Access.Public, true)
        } else {
            (Ast.Access.Private, false)
        }
    }

    ///
    /// If the cursor is at "mod " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    def parseModule(ac: Ast.Access, sc: SC[r]): Option[Ast.Module] \ r + Error = {
        if (sc |> parseKeywordOpt("mod")) {
            let name = sc !> parseWsAny |> parseName;
            sc |> parseChar('{');
            let scope = sc |> parseScope;
            sc |> parseCharHint('}', List#{"'def'", "'type'", "'use'", "'enum'", "'pub'"});
            Some(Ast.mkModule(ac, name, scope))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "type " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    def parseTypeAlias(ac: Ast.Access, sc: SC[r]): Option[Ast.TypeAlias] \ r + Error = {
        if (sc |> parseKeywordOpt("type")) {
            sc !> parseWsAny |> parseKeyword("alias");
            let name = sc !> parseWsAny |> parseWord;
            let args = if (sc |> parseCharOpt('[')) {
                let args1 = sc |> parseCommaSeq(parseWord);
                sc |> parseCharHint(']', List#{"letter"});
                args1
            } else Nil;
            sc |> parseChar('=');
            let tpe = sc |> parseType;
            Some(Ast.mkTypeAlias(ac, name, args, tpe))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "enum " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    def parseEnum(ac: Ast.Access, sc: SC[r]): Option[Ast.Enum] \ r + Error = {
        if (sc |> parseKeywordOpt("enum")) {
            let name = sc !> parseWsAny |> parseWord;
            let args = if (sc |> parseCharOpt('[')) {
                let args1 = sc |> parseCommaSeq(parseWord);
                sc |> parseCharHint(']', List#{"letter"});
                args1
            } else Nil;
            sc |> parseChar('{');
            let cases = sc |> parseRepeated(parseEnumCase);
            sc |> parseCharHint('}', List#{"'case'", "'pub'"});
            Some(Ast.mkEnum(ac, name, args, cases))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "case " or "pub " then parsing will start, otherwise None is returned.
    ///
    /// Handles prefix whitespace.
    def parseEnumCase(sc: SC[r]): Option[Ast.EnumCase] \ r + Error = {
        let (ac, lockedIn) = sc |> parseAccess;
        sc |> parseWsAny;
        let skipCheck = if (lockedIn) {sc |> parseKeyword("case"); true} else false;
        if (skipCheck or sc |> parseKeywordOpt("case")) {
            let name = sc !> parseWsAny |> parseWord;
            if (sc |> parseCharOpt('(')) {
                let types = sc |> parseCommaSeq(parseType);
                sc |> parseCharHint(')', List#{"letter"});
                Some(Ast.mkEnumCase(ac, name, types))
            } else {
                Some(Ast.mkEnumCase(ac, name, Nil))
            }
        } else {
            None
        }
    }

    /// Handles prefix whitespace.
    def parseType(sc: SC[r]): Ast.Tpe \ r + Error = {
        let base = Ast.Tpe.Base(sc !> parseWsAny |> parseName);
        if (sc |> parseCharOpt('[')) {
            let args = sc |> parseCommaSeq(parseType);
            sc |> parseCharHint(']', List#{"letter"});
            Ast.Tpe.App(base, args)
        } else {
            base
        }
    }

    ///
    /// If the cursor is at "def " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    def parseDef(ac: Ast.Access, sc: SC[r]): Option[Ast.Def] \ r + Error = {
        if (sc |> parseKeywordOpt("def")) {
            let name = sc !> parseWsAny |> parseWord;
            let args = if (sc |> parseCharOpt('(')) {
                if (sc !> parseWsAny |> parseCharOpt(')')) {
                    Nil
                } else {
                    let args1 = sc |> parseCommaSeq(parseParam);
                    sc |> parseCharHint(')', List#{"letter"});
                    args1
                }
            } else Nil;
            sc |> parseChar(':');
            let tpe = sc |> parseType;
            sc !> parseWsAny |> parseChar('=');
            sc !> parseWsAny |> parseChar('{');
            let exp = sc |> parseExp;
            sc !> parseWsAny |> parseChar('}');
            Some(Ast.mkDef(ac, name, args, tpe, exp))
        } else {
            None
        }
    }

    /// Handles prefix whitespace.
    def parseParam(sc: SC[r]): (Ast.LocString, Ast.Tpe) \ r + Error = {
        let name = sc !> parseWsAny |> parseWord;
        sc |> parseChar(':');
        let tpe = sc |> parseType;
        (name, tpe)
    }

    /// Handles prefix whitespace.
    def parseExpSimple(sc: SC[r]): Ast.Exp \ r + Error = {
        if (sc !> parseWsAny |> parseKeywordOpt("let")) {
            let name = sc !> parseWsAny |> parseWord;
            sc !> parseWsAny |> parseChar('=');
            let binding = sc |> parseExp;
            sc !> parseWsAny |> parseChar(';');
            let body = sc |> parseExp;
            Ast.Exp.Let(name, binding, body)
        } else {
            let name = sc !> parseWsAny |> parseName;
            Ast.Exp.Name(name)
        }
    }

    /// Handles prefix whitespace.
    def parseExp(sc: SC[r]): Ast.Exp \ r + Error = {
        let exp = sc |> parseExpSimple;
        if (sc |> parseCharOpt('(')) {
            if (sc !> parseWsAny |> parseCharOpt(')')) {
                Ast.Exp.App(exp, Nil)
            } else {
                let args = sc |> parseCommaSeq(parseExp);
                sc !> parseWsAny |> parseChar(')');
                Ast.Exp.App(exp, args)
            }
        } else if (sc !> parseWsAny |> parseCharOpt(';')) {
            Ast.Exp.Stmt(exp, sc |> parseExp)
        } else {
            exp
        }
    }

    ///
    /// If the cursor is at "use " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix whitespace.
    def parseUse(ac: Ast.Access, sc: SC[r]): Option[Ast.Use] \ r + Error = {
        if (sc |> parseKeywordOpt("use")) {
            let name = sc !> parseWsAny |> parseName;
            sc |> parseWsAny;
            if (sc |> parseKeywordOpt("as")) {
                let renaming = sc !> parseWsAny |> parseWord;
                Some(Ast.mkRenamedUse(ac, name, renaming))
            } else {
                Some(Ast.mkUse(ac, name))
            }
        } else {
            None
        }
    }

    /// Handles prefix whitespace.
    def parseName(sc: SC[r]): Ast.Name \ r + Error = {
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
            if (sc !> parseWsAny |> parseCharOpt(',')) {
                loop(x :: acc)
            } else {
                (x :: acc) |> List.reverse
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

    /// Handles prefix whitespace.
    def parseChar(c: Char, sc: SC[r]): Unit \ r + Error = {
        if (sc !> parseWsAny |> parseCharOpt(c)) {
            ()
        } else {
            sc |> errExpectChar(c)
        }
    }

    /// Handles prefix whitespace.
    def parseCharHint(c: Char, hints: List[String], sc: SC[r]): Unit \ r + Error = {
        if (sc !> parseWsAny |> parseCharOpt(c)) {
            ()
        } else {
            sc |> err("Found '${peekToString(sc)}', expected ${hints |> List.join(", ")}, or '${c}'")
        }
    }

    /// DOES NOT handle prefix whitespace.
    def parseCharOpt(c: Char, sc: SC[r]): Bool \ r =
        sc |> SC.advanceIfMatchChar(c)

    def getLoc(sc: SC[r]): Ast.Loc \ r =
        Ast.mkLoc(line = getLine(sc) + 1, column = getColumn(sc) + 1, getOffset(sc))

    def mkLocString(s: String, start: {start = Ast.Loc}, end: Ast.Loc): Ast.LocString =
        Ast.mkLocString(s, Ast.mkLocRange(start, end))

    def errExpect(expected: String, sc: SC[r]): t \ r + Error =
        err("Found '${peekToString(sc)}', expected ${expected}", sc)

    def errExpectChar(expected: Char, sc: SC[r]): t \ r + Error =
        err("Found '${peekToString(sc)}', expected '${expected}'", sc)

    def peekToString(sc: SC[r]): String \ r = {
        let c = sc |> peek;
        if (c == '\u0000') "EOF" else ToString.toString(c)
    }

    def err(msg: String, sc: SC[r]): t \ r + Error = {
        let loc = "${sc |> getLine + 1}:${sc |> getColumn + 1}";
        Error.error("${msg} (${loc})")
    }
}
