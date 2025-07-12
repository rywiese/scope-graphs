mod ScopeGraph.Phase.Parser {
    use Foldable.forEach
    use Functor.map

    use ScopeGraph.Ast.Lexed.Token.TokenKind
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.Lexed
    use ScopeGraph.Ast.Lexed.Token
    use ScopeGraph.Ast.Shared.Location
    use ScopeGraph.Ast.Shared.Location.LocString
    use ScopeGraph.Util.{VectorCursor => VC}
    use ScopeGraph.Phase.Parser.{TokenCursor => TC}

    pub eff Error {
        def error(msg: String): Void
    }

    mod Error {
        pub def runUnsafe(prefix: String, f: Unit -> t \ ef): t \ ef - Error =
            run f() with handler Error {
                def error(msg, _) = bug!(prefix + " " + msg)
            }
    }

    type alias TokenCursor[r: Region] = VC[Token, r]

    pub def parse(tokens: Vector[Token]): ParsedAst.Scope \ Error = region rc {
        tokens |> VC.mk(rc) |> parseCursor
    }

    def parseCursor(tc: TC[r]): ParsedAst.Scope \ r + Error = {
        let scope = tc |> parseScope;
        tc |> parseWsAny;
        if (tc |> VC.inBounds) tc |> errExpect(expected = None |> peekToString, tc |> VC.peek |> peekToString, tc |> latestLoc)
        else scope
    }

    def parseScope(tc: TC[r]): ParsedAst.Scope \ r + Error = region rc {
        let uses = MutList.empty(rc);
        let modules = MutList.empty(rc);
        let aliases = MutList.empty(rc);
        let enums = MutList.empty(rc);
        let defs = MutList.empty(rc);
        let progress = Ref.fresh(rc, true);
        def loop() = {
            Ref.put(false, progress);
            let (ac, _) = tc |> parseAccess;
            tc !> parseWsAny |> parseModule(ac) |> forEach(m -> {
                modules |> MutList.push(m);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseUse(ac) |> forEach(u -> {
                uses |> MutList.push(u);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseTypeAlias(ac) |> forEach(ta -> {
                aliases |> MutList.push(ta);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseEnum(ac) |> forEach(e -> {
                enums |> MutList.push(e);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseDef(ac) |> forEach(d -> {
                defs |> MutList.push(d);
                Ref.put(true, progress)
            });
            if (Ref.get(progress)) loop() else ()
        };
        loop();
        ParsedAst.mkScope(
            uses |> MutList.toList,
            modules |> MutList.toList,
            aliases |> MutList.toList,
            enums |> MutList.toList,
            defs |> MutList.toList
        )
    }

    ///
    /// Treats nothing is `Ast.Access.Private`.
    /// Returns (_, true) if any non-space input was consumed.
    ///
    /// Handles prefix space.
    def parseAccess(tc: TC[r]): (ParsedAst.Access, Bool) \ r = {
        tc |> parseWsAny;
        if (tc |> parseKeywordOpt("pub")) {
            (ParsedAst.Access.Public, true)
        } else {
            (ParsedAst.Access.Private, false)
        }
    }

    ///
    /// If the cursor is at "mod " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseModule(ac: ParsedAst.Access, tc: TC[r]): Option[ParsedAst.Module] \ r + Error = {
        if (tc |> parseKeywordOpt("mod")) {
            let name = tc !> parseWsAny |> parseName;
            tc |> parseToken(TokenKind.ParenCurlyLeft);
            let scope = tc |> parseScope;
            tc |> parseTokenHint(TokenKind.ParenCurlyRight, List#{"'def'", "'type'", "'use'", "'enum'", "'pub'"});
            Some(ParsedAst.mkModule(ac, name, scope))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "type " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseTypeAlias(ac: ParsedAst.Access, tc: TC[r]): Option[ParsedAst.TypeAlias] \ r + Error = {
        if (tc |> parseKeywordOpt("type")) {
            tc !> parseWsAny |> parseKeyword("alias");
            let name = tc !> parseWsAny |> parseWord;
            let args = if (tc |> parseTokenOpt(TokenKind.ParenSquareLeft)) {
                let args1 = tc |> parseCommaSeq(parseWord);
                tc |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
                args1
            } else Nil;
            tc |> parseToken(TokenKind.Eq);
            let tpe = tc |> parseType;
            Some(ParsedAst.mkTypeAlias(ac, name, args, tpe))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "enum " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseEnum(ac: ParsedAst.Access, tc: TC[r]): Option[ParsedAst.Enum] \ r + Error = {
        if (tc |> parseKeywordOpt("enum")) {
            let name = tc !> parseWsAny |> parseWord;
            let args = if (tc |> parseTokenOpt(TokenKind.ParenSquareLeft)) {
                let args1 = tc |> parseCommaSeq(parseWord);
                tc |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
                args1
            } else Nil;
            tc |> parseToken(TokenKind.ParenCurlyLeft);
            let cases = tc |> parseRepeated(parseEnumCase);
            tc |> parseTokenHint(TokenKind.ParenCurlyRight, List#{"'case'", "'pub'"});
            Some(ParsedAst.mkEnum(ac, name, args, cases))
        } else {
            None
        }
    }

    ///
    /// If the cursor is at "case " or "pub " then parsing will start, otherwise None is returned.
    ///
    /// Handles prefix space.
    def parseEnumCase(tc: TC[r]): Option[ParsedAst.EnumCase] \ r + Error = {
        let (ac, lockedIn) = tc |> parseAccess;
        tc |> parseWsAny;
        let skipCheck = if (lockedIn) {tc |> parseKeyword("case"); true} else false;
        if (skipCheck or tc |> parseKeywordOpt("case")) {
            let name = tc !> parseWsAny |> parseWord;
            if (tc |> parseTokenOpt(TokenKind.ParenSoftLeft)) {
                let types = tc |> parseCommaSeq(parseType);
                tc |> parseTokenHint(TokenKind.ParenSoftRight, List#{"letter"});
                Some(ParsedAst.mkEnumCase(ac, name, types))
            } else {
                Some(ParsedAst.mkEnumCase(ac, name, Nil))
            }
        } else {
            None
        }
    }

    /// Handles prefix space.
    def parseType(tc: TC[r]): ParsedAst.Tpe \ r + Error = {
        let base = ParsedAst.Tpe.Base(tc !> parseWsAny |> parseName);
        if (tc |> parseTokenOpt(TokenKind.ParenSquareLeft)) {
            let args = tc |> parseCommaSeq(parseType);
            tc |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
            ParsedAst.Tpe.App(base, args)
        } else {
            base
        }
    }

    ///
    /// If the cursor is at "def " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseDef(ac: ParsedAst.Access, tc: TC[r]): Option[ParsedAst.Def] \ r + Error = {
        if (tc |> parseKeywordOpt("def")) {
            let name = tc !> parseWsAny |> parseWord;
            let args = if (tc |> parseTokenOpt(TokenKind.ParenSoftLeft)) {
                if (tc !> parseWsAny |> parseTokenOpt(TokenKind.ParenSoftRight)) {
                    Nil
                } else {
                    let args1 = tc |> parseCommaSeq(parseParam);
                    tc |> parseTokenHint(TokenKind.ParenSoftRight, List#{"letter"});
                    args1
                }
            } else Nil;
            tc |> parseToken(TokenKind.Colon);
            let tpe = tc |> parseType;
            tc !> parseWsAny |> parseToken(TokenKind.Eq);
            tc !> parseWsAny |> parseToken(TokenKind.ParenCurlyLeft);
            let exp = tc |> parseExp;
            tc !> parseWsAny |> parseToken(TokenKind.ParenCurlyRight);
            Some(ParsedAst.mkDef(ac, name, args, tpe, exp))
        } else {
            None
        }
    }

    /// Handles prefix space.
    def parseParam(tc: TC[r]): (LocString, ParsedAst.Tpe) \ r + Error = {
        let name = tc !> parseWsAny |> parseWord;
        tc |> parseToken(TokenKind.Colon);
        let tpe = tc |> parseType;
        (name, tpe)
    }

    /// Handles prefix space.
    def parseExpSimple(tc: TC[r]): (ParsedAst.Exp, Bool) \ r + Error = {
        tc |> parseWsAny;
        let peeked = match tc |> VC.peek {
            case Some(v) => v
            case None => tc |> errExpect(expected = "expression", "EOF", tc |> latestLoc)
        };
        match peeked {
            case Token.Name(tokenName, _) =>
                match tokenName {
                    case "let" =>
                        tc |> VC.advance;
                        let name = tc !> parseWsAny |> parseWord;
                        tc !> parseWsAny |> parseToken(TokenKind.Eq);
                        let binding = tc |> parseExp;
                        tc !> parseWsAny |> parseToken(TokenKind.Semicolon);
                        let body = tc |> parseExp;
                        (ParsedAst.Exp.Let(name, binding, body), true)
                    case "true" =>
                        tc |> VC.advance;
                        (ParsedAst.Exp.Bool(true), false)
                    case "false" =>
                        tc |> VC.advance;
                        (ParsedAst.Exp.Bool(false), false)
                    case _ =>
                        (tc |> parseName |> ParsedAst.Exp.Name, false)
                }
            case Token.Int(i, _) =>
                tc |> VC.advance;
                (ParsedAst.Exp.Int(i), false)
            case Token.ParenCurlyLeft(_) =>
                tc |> VC.advance;
                let exp = tc |> parseExp;
                tc |> parseToken(TokenKind.ParenCurlyRight);
                (exp, false)
            case other =>
                tc |> errExpect(expected = "expression", ToString.toString(other), other |> Token.getLoc)
        }
    }

    /// Handles prefix space.
    def parseExp(tc: TC[r]): ParsedAst.Exp \ r + Error = {
        let (exp, stop) = tc |> parseExpSimple;
        if (stop) exp
        else if (tc |> parseTokenOpt(TokenKind.ParenSoftLeft)) {
            if (tc !> parseWsAny |> parseTokenOpt(TokenKind.ParenSoftRight)) {
                ParsedAst.Exp.App(exp, Nil)
            } else {
                let args = tc |> parseCommaSeq(parseExp);
                tc !> parseWsAny |> parseToken(TokenKind.ParenSoftRight);
                ParsedAst.Exp.App(exp, args)
            }
        } else if (tc !> parseWsAny |> parseTokenOpt(TokenKind.Semicolon)) {
            ParsedAst.Exp.Stmt(exp, tc |> parseExp)
        } else {
            exp
        }
    }

    ///
    /// If the cursor is at "use " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseUse(ac: ParsedAst.Access, tc: TC[r]): Option[ParsedAst.Use] \ r + Error = {
        if (tc |> parseKeywordOpt("use")) {
            let name = tc !> parseWsAny |> parseName;
            tc |> parseWsAny;
            if (tc |> parseKeywordOpt("as")) {
                let renaming = tc !> parseWsAny |> parseWord;
                Some(ParsedAst.mkRenamedUse(ac, name, renaming))
            } else {
                Some(ParsedAst.mkUse(ac, name))
            }
        } else {
            None
        }
    }

    /// Handles prefix space.
    def parseName(tc: TC[r]): ParsedAst.Name \ r + Error = {
        tc |> parseWsAny;
        def loop(acc) = match tc |> parseWordOpt {
            case Some(n) =>
                match tc |> parseTokenOpt(TokenKind.Period) {
                    case true => loop(n :: acc)
                    case false => ParsedAst.mkName(acc |> List.reverse, n)
                }
            case None =>
                ParsedAst.mkName(acc |> List.reverse, tc |> parseWord)
        };
        loop(Nil)
    }

    ///
    /// `f` does not need to handle prefix space.
    ///
    /// Handles prefix space.
    def parseCommaSeq(f: TC[r] -> t \ ef, tc: TC[r]): List[t] \ ef + r = {
        def loop(acc) = {
            let x = tc !> parseWsAny |> f;
            if (tc !> parseWsAny |> parseTokenOpt(TokenKind.Comma)) {
                loop(x :: acc)
            } else {
                (x :: acc) |> List.reverse
            }
        };
        loop(Nil)
    }

    ///
    /// `f` does not need to handle prefix space.
    ///
    /// Handles prefix space.
    def parseRepeated(f: TC[r] -> Option[t] \ ef, tc: TC[r]): List[t] \ ef + r = {
        def loop(acc) = {
            match tc !> parseWsAny |> f {
                case Some(v) => loop(v :: acc)
                case None => acc |> List.reverse
            }
        };
        loop(Nil)
    }

    /// DOES NOT handle prefix space.
    def parseKeywordOpt(kw: String, tc: TC[r]): Bool \ r = {
        tc |> VC.advanceIf(token -> match token {
            case Token.Name(name, _) if name == kw => true
            case _ => false
        })
    }

    /// DOES NOT handle prefix space.
    def parseKeyword(kw: String, tc: TC[r]): Unit \ r + Error = {
        if (parseKeywordOpt(kw, tc)) ()
        else tc |> errExpect(expected = "'${kw}'", tc |> VC.peek |> peekToString, tc |> latestLoc)
    }

    /// DOES NOT handle prefix space.
    def parseWordOpt(tc: TC[r]): Option[LocString] \ r = {
        tc |> VC.collectIf(token -> match token {
            case Token.Name(name, loc) => Some(Location.mkLocString(name, loc))
            case _ => None
        })
    }

    /// DOES NOT handle prefix space.
    def parseWord(tc: TC[r]): LocString \ r + Error = {
        match tc |> VC.next {
            case Some(Token.Name(name, loc)) => Location.mkLocString(name, loc)
            case Some(other) => tc |> errExpect(expected = "${TokenKind.Name}", other |> ToString.toString, other |> Token.getLoc)
            case None => tc |> errExpect(expected = "${TokenKind.Name}", None |> peekToString, tc |> latestLoc)
        }
    }

    /// Consumes space and comments
    def parseWsAny(tc: TC[r]): Unit \ r =
        tc |> VC.advanceWhile(token -> match token {
            case Token.Space(_) => true
            case Token.Comment(_, _) => true
            case _ => false
        })

    /// Handles prefix space.
    def parseToken(kind: TokenKind, tc: TC[r]): Unit \ r + Error = {
        tc |> parseWsAny;
        if (tc |> parseTokenOpt(kind)) {
            ()
        } else {
            tc |> errExpect(expected = kind |> ToString.toString, tc |> VC.peek |> peekToString, tc |> latestLoc)
        }
    }

    /// Handles prefix space.
    def parseTokenOpt(tk: TokenKind, tc: TC[r]): Bool \ r = {
        tc |> VC.advanceIf(Token.getKind >> Eq.eq(tk))
    }

    /// Handles prefix space.
    def parseTokenHint(kind: TokenKind, hints: List[String], tc: TC[r]): Unit \ r + Error = {
        tc |> parseWsAny;
        if (tc |> parseTokenOpt(kind)) {
            ()
        } else {
            let expected = "${hints |> List.join(", ")}, or ${kind}";
            tc |> errExpect(expected = expected, tc |> VC.peek |> peekToString, tc |> latestLoc)
        }
    }

    def peekToString(o: Option[Token]): String = {
        match o {
            case None => "EOF"
            case Some(t) => t |> ToString.toString
        }
    }

    def latestLoc(tc: TC[r]): Location.LocRange \ r =
        match tc |> VC.peekOrPrev {
            case Some(loc) => loc |> Token.getLoc
            case None =>
                let firstLoc = Location.mkLoc(line = 1, column = 1, 0);
                Location.mkLocRange(start = firstLoc, firstLoc)
        }

    def errExpect(expected: {expected = String}, found: String, loc: Location.LocRange): t \ Error =
        err("Found ${found}, expected ${expected#expected}", loc)

    def err(msg: String, loc: Location.LocRange): t \ Error = {
        Error.error("${msg} (${loc})")
    }
}
