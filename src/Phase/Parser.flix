mod ScopeGraph.Phase.Parser {
    use ScopeGraph.Ast.Shared.Location.LocRange
    use Foldable.forEach
    use Functor.map

    use ScopeGraph.Ast.Lexed.Token.TokenKind
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Ast.Lexed
    use ScopeGraph.Ast.Lexed.Token
    use ScopeGraph.Ast.Shared.Location
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.Ast.Shared.Location.LocString
    use ScopeGraph.Ast.Shared.Location.Located
    use ScopeGraph.Util.{VectorCursor => VC}
    use ScopeGraph.Phase.Parser.{TokenCursor => TC}

    type alias TokenCursor[r: Region] = VC[Token, r]

    pub def parse(tokens: Vector[Token]): ParsedAst.Scope \ Crash = region rc {
        tokens |> VC.mk(rc) |> parseCursor
    }

    def parseCursor(tc: TC[r]): ParsedAst.Scope \ r + Crash = {
        let scope = tc |> parseScope;
        tc |> parseWsAny;
        if (tc |> VC.inBounds) tc |> errExpect(expected = None |> peekToString, tc |> VC.peek |> peekToString, tc |> latestLoc)
        else scope
    }

    def parseScope(tc: TC[r]): ParsedAst.Scope \ r + Crash = region rc {
        let uses = MutList.empty(rc);
        let modules = MutList.empty(rc);
        let aliases = MutList.empty(rc);
        let enums = MutList.empty(rc);
        let defs = MutList.empty(rc);
        let progress = Ref.fresh(rc, true);
        def loop() = {
            Ref.put(false, progress);
            let (ac, acLoc) = tc |> parseAccess;
            tc !> parseWsAny |> parseModule(ac, acLoc) |> forEach(m -> {
                modules |> MutList.push(m);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseUse(ac, acLoc) |> forEach(u -> {
                uses |> MutList.push(u);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseTypeAlias(ac, acLoc) |> forEach(ta -> {
                aliases |> MutList.push(ta);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseEnum(ac) |> forEach(e -> {
                enums |> MutList.push(e);
                Ref.put(true, progress)
            });
            tc !> parseWsAny |> parseDef(ac, acLoc) |> forEach(d -> {
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
    def parseAccess(tc: TC[r]): (ParsedAst.Access, Option[LocRange]) \ r = {
        tc |> parseWsAny;
        match tc |> parseKeywordOpt("pub") {
            case Some(loc) => (ParsedAst.Access.Public, Some(loc))
            case None => (ParsedAst.Access.Private, None)
        }
    }

    ///
    /// If the cursor is at "mod " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseModule(ac: ParsedAst.Access, acLoc: Option[LocRange], tc: TC[r]): Option[ParsedAst.Module] \ r + Crash = {
        tc |> parseKeywordOpt("mod") |> map(modLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(modLoc);
            let name = tc !> parseWsAny |> parseName;
            tc |> parseToken(TokenKind.ParenCurlyLeft);
            let scope = tc |> parseScope;
            let endLoc = tc |> parseTokenHintWithLoc(TokenKind.ParenCurlyRight, List#{"'def'", "'type'", "'use'", "'enum'", "'pub'"});
            let loc = Location.union(startLoc, endLoc);
            ParsedAst.mkModule(ac, name, scope, loc)
        })
    }

    ///
    /// If the cursor is at "type " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseTypeAlias(ac: ParsedAst.Access, acLoc: Option[LocRange], tc: TC[r]): Option[ParsedAst.TypeAlias] \ r + Crash = {
        tc |> parseKeywordOpt("type") |> map(typeLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(typeLoc);
            tc !> parseWsAny |> parseKeyword("alias");
            let name = tc !> parseWsAny |> parseWord;
            let args = if (tc |> parseTokenBool(TokenKind.ParenSquareLeft)) {
                let args1 = tc |> parseCommaSeq(parseWord);
                tc |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
                args1
            } else Nil;
            tc |> parseToken(TokenKind.Eq);
            let tpe = tc |> parseType;
            let loc = Location.union(startLoc, tpe |> Located.getLoc);
            ParsedAst.mkTypeAlias(ac, name, args, tpe, loc)
        })
    }

    ///
    /// If the cursor is at "enum " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseEnum(ac: ParsedAst.Access, tc: TC[r]): Option[ParsedAst.Enum] \ r + Crash = {
        if (tc |> parseKeywordBool("enum")) {
            let name = tc !> parseWsAny |> parseWord;
            let args = if (tc |> parseTokenBool(TokenKind.ParenSquareLeft)) {
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
    def parseEnumCase(tc: TC[r]): Option[ParsedAst.EnumCase] \ r + Crash = {
        let (ac, lockedInOpt) = tc |> parseAccess;
        let lockedIn = not Option.isEmpty(lockedInOpt);
        tc |> parseWsAny;
        let skipCheck = if (lockedIn) {tc |> parseKeyword("case"); true} else false;
        if (skipCheck or tc |> parseKeywordBool("case")) {
            let name = tc !> parseWsAny |> parseWord;
            if (tc |> parseTokenBool(TokenKind.ParenSoftLeft)) {
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
    def parseType(tc: TC[r]): ParsedAst.Tpe \ r + Crash = {
        let name = tc !> parseWsAny |> parseName;
        let base = ParsedAst.Tpe.Base(name, name |> Located.getLoc);
        if (tc |> parseTokenBool(TokenKind.ParenSquareLeft)) {
            let args = tc |> parseCommaSeq(parseType);
            tc |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
            let lastLoc = args |>
                List.last |>
                map(Located.getLoc) |>
                Option.getWithDefault(base |> Located.getLoc);
            ParsedAst.Tpe.App(base, args, lastLoc)
        } else {
            base
        }
    }

    ///
    /// If the cursor is at "def " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseDef(ac: ParsedAst.Access, acLoc: Option[LocRange], tc: TC[r]): Option[ParsedAst.Def] \ r + Crash = {
        tc |> parseKeywordOpt("def") |> map(defLoc -> {
            let firstLoc = acLoc |> Option.getWithDefault(defLoc);
            let name = tc !> parseWsAny |> parseWord;
            let args = if (tc |> parseTokenBool(TokenKind.ParenSoftLeft)) {
                if (tc !> parseWsAny |> parseTokenBool(TokenKind.ParenSoftRight)) {
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
            let lastLoc = tc !> parseWsAny |> parseTokenWithLoc(TokenKind.ParenCurlyRight);
            let loc = Location.union(firstLoc, lastLoc);
            ParsedAst.mkDef(ac, name, args, tpe, exp, loc)
        })
    }

    /// Handles prefix space.
    def parseParam(tc: TC[r]): (LocString, ParsedAst.Tpe) \ r + Crash = {
        let name = tc !> parseWsAny |> parseWord;
        tc |> parseToken(TokenKind.Colon);
        let tpe = tc |> parseType;
        (name, tpe)
    }

    /// Handles prefix space.
    def parseExpSimple(tc: TC[r]): (ParsedAst.Exp, Bool) \ r + Crash = {
        tc |> parseWsAny;
        let peeked = match tc |> VC.peek {
            case Some(v) => v
            case None => tc |> errExpect(expected = "expression", "EOF", tc |> latestLoc)
        };
        match peeked {
            case Token.Name(tokenName, nameLoc) =>
                match tokenName {
                    case "let" =>
                        tc |> VC.advance;
                        let name = tc !> parseWsAny |> parseWord;
                        tc !> parseWsAny |> parseToken(TokenKind.Eq);
                        let binding = tc |> parseExp;
                        tc !> parseWsAny |> parseToken(TokenKind.Semicolon);
                        let body = tc |> parseExp;
                        let loc = Location.union(nameLoc, body |> Located.getLoc);
                        (ParsedAst.Exp.Let(name, binding, body, loc), true)
                    case "true" =>
                        tc |> VC.advance;
                        (ParsedAst.Exp.Bool(true, nameLoc), false)
                    case "false" =>
                        tc |> VC.advance;
                        (ParsedAst.Exp.Bool(false, nameLoc), false)
                    case _ =>
                        let name = tc |> parseName;
                        let loc = Location.union(nameLoc, name |> Located.getLoc);
                        (ParsedAst.Exp.Name(name, loc), false)
                }
            case Token.Int(i, loc) =>
                tc |> VC.advance;
                (ParsedAst.Exp.Int(i, loc), false)
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
    def parseExp(tc: TC[r]): ParsedAst.Exp \ r + Crash = {
        let (exp, stop) = tc |> parseExpSimple;
        if (stop) exp
        else if (tc |> parseTokenBool(TokenKind.ParenSoftLeft)) {
            if (tc !> parseWsAny |> parseTokenBool(TokenKind.ParenSoftRight)) {
                ParsedAst.Exp.App(exp, Nil, exp |> Located.getLoc)
            } else {
                let args = tc |> parseCommaSeq(parseExp);
                tc !> parseWsAny |> parseToken(TokenKind.ParenSoftRight);
                let last = args |> List.last |> Option.getWithDefault(exp);
                let loc = Location.union(exp |> Located.getLoc, last |> Located.getLoc);
                ParsedAst.Exp.App(exp, args, loc)
            }
        } else if (tc !> parseWsAny |> parseTokenBool(TokenKind.Semicolon)) {
            let tail = tc |> parseExp;
            let loc = Location.union(exp |> Located.getLoc, tail |> Located.getLoc);
            ParsedAst.Exp.Stmt(exp, tail, loc)
        } else {
            exp
        }
    }

    ///
    /// If the cursor is at "use " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseUse(ac: ParsedAst.Access, acLoc: Option[LocRange], tc: TC[r]): Option[ParsedAst.Use] \ r + Crash = {
        tc |> parseKeywordOpt("use") |> map(useLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(useLoc);
            let name = tc !> parseWsAny |> parseName;
            tc |> parseWsAny;
            if (tc |> parseKeywordBool("as")) {
                let renaming = tc !> parseWsAny |> parseWord;
                let loc = Location.union(startLoc, renaming |> Located.getLoc);
                ParsedAst.mkRenamedUse(ac, name, renaming, loc)
            } else {
                let loc = Location.union(startLoc, name |> Located.getLoc);
                ParsedAst.mkUse(ac, name, loc)
            }
        })
    }

    /// Handles prefix space.
    def parseName(tc: TC[r]): ParsedAst.Name \ r + Crash = {
        tc |> parseWsAny;
        def loop(acc) = match tc |> parseWordOpt {
            case Some(n) =>
                match tc |> parseTokenBool(TokenKind.Period) {
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
            if (tc !> parseWsAny |> parseTokenBool(TokenKind.Comma)) {
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
    def parseKeywordOpt(kw: String, tc: TC[r]): Option[LocRange] \ r = {
        tc |> VC.collectIf(token -> match token {
            case Token.Name(name, loc) if name == kw => Some(loc)
            case _ => None
        })
    }

    /// DOES NOT handle prefix space.
    def parseKeywordBool(kw: String, tc: TC[r]): Bool \ r = {
        match parseKeywordOpt(kw, tc) {
            case Some(_) => true
            case None => false
        }
    }

    /// DOES NOT handle prefix space.
    def parseKeyword(kw: String, tc: TC[r]): Unit \ r + Crash = {
        if (parseKeywordBool(kw, tc)) ()
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
    def parseWord(tc: TC[r]): LocString \ r + Crash = {
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
    def parseTokenWithLoc(kind: TokenKind, tc: TC[r]): LocRange \ r + Crash = {
        tc |> parseWsAny;
        match tc |> parseTokenOpt(kind) {
            case Some(loc) => loc
            case None => tc |> errExpect(
                expected = kind |> ToString.toString,
                tc |> VC.peek |> peekToString,
                tc |> latestLoc
            )
        }
    }

    /// Handles prefix space.
    def parseToken(kind: TokenKind, tc: TC[r]): Unit \ r + Crash = {
        tc |> parseWsAny;
        if (tc |> parseTokenBool(kind)) {
            ()
        } else {
            tc |> errExpect(
                expected = kind |> ToString.toString,
                tc |> VC.peek |> peekToString,
                tc |> latestLoc
            )
        }
    }

    /// Handles prefix space.
    def parseTokenOpt(tk: TokenKind, tc: TC[r]): Option[LocRange] \ r = {
        tc |>
            VC.collectIf(token ->
                if (token |> Token.getKind == tk) Some(token |> Located.getLoc)
                else None
            )
    }

    /// Handles prefix space.
    def parseTokenBool(tk: TokenKind, tc: TC[r]): Bool \ r = {
        tc |> VC.advanceIf(Token.getKind >> Eq.eq(tk))
    }

    /// Handles prefix space.
    def parseTokenHint(kind: TokenKind, hints: List[String], tc: TC[r]): Unit \ r + Crash = {
        tc |> parseWsAny;
        if (tc |> parseTokenBool(kind)) {
            ()
        } else {
            let expected = "${hints |> List.join(", ")}, or ${kind}";
            tc |> errExpect(expected = expected, tc |> VC.peek |> peekToString, tc |> latestLoc)
        }
    }

    /// Handles prefix space.
    def parseTokenHintWithLoc(kind: TokenKind, hints: List[String], tc: TC[r]): LocRange \ r + Crash = {
        tc |> parseWsAny;
        match tc |> parseTokenOpt(kind) {
            case Some(loc) =>
                loc
            case None =>
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

    def errExpect(expected: {expected = String}, found: String, loc: Location.LocRange): t \ Crash =
        err("Found ${found}, expected ${expected#expected}", loc)

    def err(msg: String, loc: Location.LocRange): t \ Crash = {
        Crash.crash(Error.mkError(category = "Parser", msg, loc))
    }
}
