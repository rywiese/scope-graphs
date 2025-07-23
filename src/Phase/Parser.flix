mod SG.Phase.Parser {
    use SG.Ast.Shared.Name
    use SG.Ast.Shared.Exp
    use SG.Ast.Shared.Tpe
    use SG.Ast.Shared.mkEnumCase
    use SG.Ast.Shared.EnumCase
    use Foldable.forEach
    use Functor.map

    use SG.Ast.Lexed
    use SG.Ast.Lexed.Token
    use SG.Ast.Lexed.Token.TokenKind
    use SG.Ast.ParsedAst
    use SG.Ast.Shared.Access
    use SG.Ast.Shared.Crash
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString
    use SG.Ast.Shared.Location.Source
    use SG.Phase.Lexer
    use SG.Util.{VectorCursor => VC}

    type alias State[r: Region] = {
        vc = VC[Token, r],
        src = Source
    }

    def newState(rc: Region[r], src: Source, v: Vector[Token]): State[r] \ r = {
        {
            vc = VC.mk(rc, v),
            src = src
        }
    }

    pub def parse(input: Lexed): Validation[Error, ParsedAst] = {
        forA (
            m1 <- input#m |> Map.mapWithKey(parseOne) |> collectErrs
        ) yield { m = m1 }
    }

    def collectErrs(m: Map[Source, Result[Error, ParsedAst.Scope]]): Validation[Error, Map[Source, ParsedAst.Scope]] = {
        (Validation.Success(Map.empty()), m) ||> Map.foldLeftWithKey((acc, src, res) -> {
            match res {
                case Result.Ok(scope) =>
                    acc |> Validation.map(Map.insert(src, scope))
                case Result.Err(err) =>
                    Applicative.productLeft(acc, Validation.Failure(Nec.singleton(err)))
            }
        })
    }

    def parseOne(src: Source, tokens: Vector[Token]): Result[Error, ParsedAst.Scope] = region rc {
        run tokens |> newState(rc, src) |> parseCursor |> Result.Ok
        with Crash.runWith(Result.Err)
    }

    def parseCursor(s: State[r]): ParsedAst.Scope \ r + Crash = {
        let scope = s |> parseScope(s |> latestLoc);
        s |> parseWsAny;
        if (s#vc |> VC.inBounds) s |> errExpect(expected = None |> peekToString, s#vc |> VC.peek |> peekToString, s |> latestLoc)
        else scope
    }

    def parseScope(startLoc: LocRange, s: State[r]): ParsedAst.Scope \ r + Crash = region rc {
        let uses = MutList.empty(rc);
        let modules = MutList.empty(rc);
        let aliases = MutList.empty(rc);
        let enums = MutList.empty(rc);
        let defs = MutList.empty(rc);
        let progress = Ref.fresh(rc, true);
        def loop() = {
            Ref.put(false, progress);
            let (ac, acLoc) = s |> parseAccess;
            s !> parseWsAny |> parseModule(ac, acLoc) |> forEach(m -> {
                modules |> MutList.push(m);
                Ref.put(true, progress)
            });
            s !> parseWsAny |> parseUse(ac, acLoc) |> forEach(u -> {
                uses |> MutList.push(u);
                Ref.put(true, progress)
            });
            s !> parseWsAny |> parseTypeAlias(ac, acLoc) |> forEach(ta -> {
                aliases |> MutList.push(ta);
                Ref.put(true, progress)
            });
            s !> parseWsAny |> parseEnum(ac, acLoc) |> forEach(e -> {
                enums |> MutList.push(e);
                Ref.put(true, progress)
            });
            s !> parseWsAny |> parseDef(ac, acLoc) |> forEach(d -> {
                defs |> MutList.push(d);
                Ref.put(true, progress)
            });
            if (Ref.get(progress)) loop() else ()
        };
        loop();
        let endLoc = s |> latestLoc;
        let loc = Location.union(startLoc, endLoc);
        ParsedAst.mkScope(
            uses |> MutList.toList,
            modules |> MutList.toList,
            aliases |> MutList.toList,
            enums |> MutList.toList,
            defs |> MutList.toList,
            loc
        )
    }

    ///
    /// Treats nothing is `Ast.Access.Private`.
    /// Returns (_, true) if any non-space input was consumed.
    ///
    /// Handles prefix space.
    def parseAccess(s: State[r]): (Access, Option[LocRange]) \ r = {
        s |> parseWsAny;
        match s |> parseKeywordOpt("pub") {
            case Some(loc) => (Access.Public, Some(loc))
            case None => (Access.Private, None)
        }
    }

    ///
    /// If the cursor is at "mod " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseModule(ac: Access, acLoc: Option[LocRange], s: State[r]): Option[ParsedAst.Module] \ r + Crash = {
        s |> parseKeywordOpt("mod") |> map(modLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(modLoc);
            let name = s !> parseWsAny |> parseName;
            let scopeLoc = s |> parseTokenWithLoc(TokenKind.ParenCurlyLeft);
            let scope = s |> parseScope(scopeLoc);
            let endLoc = s |> parseTokenHintWithLoc(TokenKind.ParenCurlyRight, List#{"'def'", "'type'", "'use'", "'enum'", "'pub'"});
            let loc = Location.union(startLoc, endLoc);
            ParsedAst.mkModule(ac, name, scope, loc)
        })
    }

    ///
    /// If the cursor is at "type " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseTypeAlias(ac: Access, acLoc: Option[LocRange], s: State[r]): Option[ParsedAst.TypeAlias] \ r + Crash = {
        s |> parseKeywordOpt("type") |> map(typeLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(typeLoc);
            s !> parseWsAny |> parseKeyword("alias");
            let name = s !> parseWsAny |> parseWord;
            let args = if (s |> parseTokenBool(TokenKind.ParenSquareLeft)) {
                let args1 = s |> parseCommaSeq(parseWord);
                s |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
                args1
            } else Nil;
            s |> parseToken(TokenKind.Eq);
            let tpe = s |> parseType;
            let loc = Location.union(startLoc, tpe |> Located.getLoc);
            ParsedAst.mkTypeAlias(ac, name, args, tpe, loc)
        })
    }

    ///
    /// If the cursor is at "enum " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseEnum(ac: Access, acLoc: Option[LocRange], s: State[r]): Option[ParsedAst.Enum] \ r + Crash = {
        s |> parseKeywordOpt("enum") |> map(enumLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(enumLoc);
            let name = s !> parseWsAny |> parseWord;
            let args = if (s |> parseTokenBool(TokenKind.ParenSquareLeft)) {
                let args1 = s |> parseCommaSeq(parseWord);
                s |> parseTokenHint(TokenKind.ParenSquareRight, List#{"letter"});
                args1
            } else Nil;
            s |> parseToken(TokenKind.ParenCurlyLeft);
            let cases = s |> parseRepeated(parseEnumCase);
            let endLoc = s |> parseTokenHintWithLoc(TokenKind.ParenCurlyRight, List#{"'case'", "'pub'"});
            let loc = Location.union(startLoc, endLoc);
            ParsedAst.mkEnum(ac, name, args, cases, loc)
        })
    }

    ///
    /// If the cursor is at "case " or "pub " then parsing will start, otherwise None is returned.
    ///
    /// Handles prefix space.
    def parseEnumCase(s: State[r]): Option[EnumCase] \ r + Crash = {
        let (ac, acLocOpt) = s |> parseAccess;
        let caseLoc = match acLocOpt {
            case Some(acLoc) =>
                s !> parseWsAny |> parseKeyword("case");
                Some(acLoc)
            case None =>
                s !> parseWsAny |> parseKeywordOpt("case")
        };
        caseLoc |> map(startLoc -> {
            let name = s !> parseWsAny |> parseWord;
            if (s |> parseTokenBool(TokenKind.ParenSoftLeft)) {
                let types = s |> parseCommaSeq(parseType);
                let endLoc = s |> parseTokenHintWithLoc(TokenKind.ParenSoftRight, List#{"letter"});
                let loc = Location.union(startLoc, endLoc);
                mkEnumCase(ac, name, types, loc)
            } else {
                let loc = Location.union(startLoc, name |> Located.getLoc);
                mkEnumCase(ac, name, Nil, loc)
            }
        })
    }

    /// Handles prefix space.
    def parseType(s: State[r]): Tpe \ r + Crash = {
        let name = s !> parseWsAny |> parseName;
        let base = Tpe.Base(name, name |> Located.getLoc);
        if (s |> parseTokenBool(TokenKind.ParenSquareLeft)) {
            let args = s |> parseCommaSeq(parseType);
            let lastLoc = s |> parseTokenHintWithLoc(TokenKind.ParenSquareRight, List#{"letter"});
            let loc = Location.union(name |> Located.getLoc, lastLoc);
            Tpe.App(base, args, loc)
        } else {
            base
        }
    }

    ///
    /// If the cursor is at "def " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseDef(ac: Access, acLoc: Option[LocRange], s: State[r]): Option[ParsedAst.Def] \ r + Crash = {
        s |> parseKeywordOpt("def") |> map(defLoc -> {
            let firstLoc = acLoc |> Option.getWithDefault(defLoc);
            let name = s !> parseWsAny |> parseWord;
            let args = if (s |> parseTokenBool(TokenKind.ParenSoftLeft)) {
                if (s !> parseWsAny |> parseTokenBool(TokenKind.ParenSoftRight)) {
                    Nil
                } else {
                    let args1 = s |> parseCommaSeq(parseParam);
                    s |> parseTokenHint(TokenKind.ParenSoftRight, List#{"letter"});
                    args1
                }
            } else Nil;
            s |> parseToken(TokenKind.Colon);
            let tpe = s |> parseType;
            s !> parseWsAny |> parseToken(TokenKind.Eq);
            s !> parseWsAny |> parseToken(TokenKind.ParenCurlyLeft);
            let exp = s |> parseExp;
            let lastLoc = s !> parseWsAny |> parseTokenWithLoc(TokenKind.ParenCurlyRight);
            let loc = Location.union(firstLoc, lastLoc);
            ParsedAst.mkDef(ac, name, args, tpe, exp, loc)
        })
    }

    /// Handles prefix space.
    def parseParam(s: State[r]): (LocString, Tpe) \ r + Crash = {
        let name = s !> parseWsAny |> parseWord;
        s |> parseToken(TokenKind.Colon);
        let tpe = s |> parseType;
        (name, tpe)
    }

    /// Handles prefix space.
    def parseExpSimple(s: State[r]): (Exp, Bool) \ r + Crash = {
        s |> parseWsAny;
        let peeked = match s#vc |> VC.peek {
            case Some(v) => v
            case None => s |> errExpect(expected = "expression", "EOF", s |> latestLoc)
        };
        match peeked {
            case Token.Name(tokenName, nameLoc) =>
                match tokenName {
                    case "let" =>
                        s#vc |> VC.advance;
                        let name = s !> parseWsAny |> parseWord;
                        s !> parseWsAny |> parseToken(TokenKind.Eq);
                        let binding = s |> parseExp;
                        s !> parseWsAny |> parseToken(TokenKind.Semicolon);
                        let body = s |> parseExp;
                        let loc = Location.union(nameLoc, body |> Located.getLoc);
                        (Exp.Let(name, binding, body, loc), true)
                    case "true" =>
                        s#vc |> VC.advance;
                        (Exp.Bool(true, nameLoc), false)
                    case "false" =>
                        s#vc |> VC.advance;
                        (Exp.Bool(false, nameLoc), false)
                    case _ =>
                        let name = s |> parseName;
                        let loc = Location.union(nameLoc, name |> Located.getLoc);
                        (Exp.Name(name, loc), false)
                }
            case Token.Int(i, loc) =>
                s#vc |> VC.advance;
                (Exp.Int(i, loc), false)
            case Token.ParenCurlyLeft(_) =>
                s#vc |> VC.advance;
                let exp = s |> parseExp;
                s |> parseToken(TokenKind.ParenCurlyRight);
                (exp, false)
            case other =>
                s |> errExpect(expected = "expression", ToString.toString(other), other |> Token.getLoc)
        }
    }

    /// Handles prefix space.
    def parseExp(s: State[r]): Exp \ r + Crash = {
        let (exp, stop) = s |> parseExpSimple;
        if (stop) exp
        else if (s |> parseTokenBool(TokenKind.ParenSoftLeft)) {
            if (s !> parseWsAny |> parseTokenBool(TokenKind.ParenSoftRight)) {
                Exp.App(exp, Nil, exp |> Located.getLoc)
            } else {
                let args = s |> parseCommaSeq(parseExp);
                s !> parseWsAny |> parseToken(TokenKind.ParenSoftRight);
                let last = args |> List.last |> Option.getWithDefault(exp);
                let loc = Location.union(exp |> Located.getLoc, last |> Located.getLoc);
                Exp.App(exp, args, loc)
            }
        } else if (s !> parseWsAny |> parseTokenBool(TokenKind.Semicolon)) {
            let tail = s |> parseExp;
            let loc = Location.union(exp |> Located.getLoc, tail |> Located.getLoc);
            Exp.Stmt(exp, tail, loc)
        } else {
            exp
        }
    }

    ///
    /// If the cursor is at "use " then parsing will start, otherwise None is returned.
    ///
    /// DOES NOT handle prefix space.
    def parseUse(ac: Access, acLoc: Option[LocRange], s: State[r]): Option[ParsedAst.Use] \ r + Crash = {
        s |> parseKeywordOpt("use") |> map(useLoc -> {
            let startLoc = acLoc |> Option.getWithDefault(useLoc);
            let name = s !> parseWsAny |> parseName;
            s |> parseWsAny;
            if (s |> parseKeywordBool("as")) {
                let renaming = s !> parseWsAny |> parseWord;
                let loc = Location.union(startLoc, renaming |> Located.getLoc);
                ParsedAst.mkRenamedUse(ac, name, renaming, loc)
            } else {
                let loc = Location.union(startLoc, name |> Located.getLoc);
                ParsedAst.mkUse(ac, name, loc)
            }
        })
    }

    /// Handles prefix space.
    def parseName(s: State[r]): Name \ r + Crash = {
        s |> parseWsAny;
        def loop(acc) = match s |> parseWordOpt {
            case Some(n) =>
                match s |> parseTokenBool(TokenKind.Period) {
                    case true => loop(n :: acc)
                    case false => ParsedAst.mkName(acc |> List.reverse, n)
                }
            case None =>
                ParsedAst.mkName(acc |> List.reverse, s |> parseWord)
        };
        loop(Nil)
    }

    ///
    /// `f` does not need to handle prefix space.
    ///
    /// Handles prefix space.
    def parseCommaSeq(f: State[r] -> t \ ef, s: State[r]): List[t] \ ef + r = {
        def loop(acc) = {
            let x = s !> parseWsAny |> f;
            if (s !> parseWsAny |> parseTokenBool(TokenKind.Comma)) {
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
    def parseRepeated(f: State[r] -> Option[t] \ ef, s: State[r]): List[t] \ ef + r = {
        def loop(acc) = {
            match s !> parseWsAny |> f {
                case Some(v) => loop(v :: acc)
                case None => acc |> List.reverse
            }
        };
        loop(Nil)
    }

    /// DOES NOT handle prefix space.
    def parseKeywordOpt(kw: String, s: State[r]): Option[LocRange] \ r = {
        s#vc |> VC.collectIf(token -> match token {
            case Token.Name(name, loc) if name == kw => Some(loc)
            case _ => None
        })
    }

    /// DOES NOT handle prefix space.
    def parseKeywordBool(kw: String, s: State[r]): Bool \ r = {
        match parseKeywordOpt(kw, s) {
            case Some(_) => true
            case None => false
        }
    }

    /// DOES NOT handle prefix space.
    def parseKeyword(kw: String, s: State[r]): Unit \ r + Crash = {
        if (parseKeywordBool(kw, s)) ()
        else s |> errExpect(expected = "'${kw}'", s#vc |> VC.peek |> peekToString, s |> latestLoc)
    }

    /// DOES NOT handle prefix space.
    def parseWordOpt(s: State[r]): Option[LocString] \ r = {
        s#vc |> VC.collectIf(token -> match token {
            case Token.Name(name, loc) => Some(Location.mkLocString(name, loc))
            case _ => None
        })
    }

    /// DOES NOT handle prefix space.
    def parseWord(s: State[r]): LocString \ r + Crash = {
        match s#vc |> VC.next {
            case Some(Token.Name(name, loc)) => Location.mkLocString(name, loc)
            case Some(other) => s |> errExpect(expected = "${TokenKind.Name}", other |> ToString.toString, other |> Token.getLoc)
            case None => s |> errExpect(expected = "${TokenKind.Name}", None |> peekToString, s |> latestLoc)
        }
    }

    /// Consumes space and comments
    def parseWsAny(s: State[r]): Unit \ r =
        s#vc |> VC.advanceWhile(token -> match token {
            case Token.Space(_) => true
            case Token.Comment(_, _) => true
            case _ => false
        })

    /// Handles prefix space.
    def parseTokenWithLoc(kind: TokenKind, s: State[r]): LocRange \ r + Crash = {
        s |> parseWsAny;
        match s |> parseTokenOpt(kind) {
            case Some(loc) => loc
            case None => s |> errExpect(
                expected = kind |> ToString.toString,
                s#vc |> VC.peek |> peekToString,
                s |> latestLoc
            )
        }
    }

    /// Handles prefix space.
    def parseToken(kind: TokenKind, s: State[r]): Unit \ r + Crash = {
        s |> parseWsAny;
        if (s |> parseTokenBool(kind)) {
            ()
        } else {
            s |> errExpect(
                expected = kind |> ToString.toString,
                s#vc |> VC.peek |> peekToString,
                s |> latestLoc
            )
        }
    }

    /// Handles prefix space.
    def parseTokenOpt(tk: TokenKind, s: State[r]): Option[LocRange] \ r = {
        s#vc |>
            VC.collectIf(token ->
                if (token |> Token.getKind == tk) Some(token |> Located.getLoc)
                else None
            )
    }

    /// Handles prefix space.
    def parseTokenBool(tk: TokenKind, s: State[r]): Bool \ r = {
        s#vc |> VC.advanceIf(Token.getKind >> Eq.eq(tk))
    }

    /// Handles prefix space.
    def parseTokenHint(kind: TokenKind, hints: List[String], s: State[r]): Unit \ r + Crash = {
        s |> parseWsAny;
        if (s |> parseTokenBool(kind)) {
            ()
        } else {
            let expected = "${hints |> List.join(", ")}, or ${kind}";
            s |> errExpect(expected = expected, s#vc |> VC.peek |> peekToString, s |> latestLoc)
        }
    }

    /// Handles prefix space.
    def parseTokenHintWithLoc(kind: TokenKind, hints: List[String], s: State[r]): LocRange \ r + Crash = {
        s |> parseWsAny;
        match s |> parseTokenOpt(kind) {
            case Some(loc) =>
                loc
            case None =>
                let expected = "${hints |> List.join(", ")}, or ${kind}";
                s |> errExpect(expected = expected, s#vc |> VC.peek |> peekToString, s |> latestLoc)
        }
    }

    def peekToString(o: Option[Token]): String = {
        match o {
            case None => "EOF"
            case Some(t) => t |> ToString.toString
        }
    }

    def latestLoc(s: State[r]): Location.LocRange \ r =
        match s#vc |> VC.peekOrPrev {
            case Some(loc) => loc |> Token.getLoc
            case None =>
                let firstLoc = Location.mkLoc(line = 1, column = 1, 0, s#src);
                Location.mkLocRange(start = firstLoc, firstLoc)
        }

    def errExpect(expected: {expected = String}, found: String, loc: Location.LocRange): t \ Crash =
        err("Found ${found}, expected ${expected#expected}", loc)

    def err(msg: String, loc: Location.LocRange): t \ Crash = {
        Crash.crash(Error.mkError(category = "Parser", msg, loc))
    }
}
