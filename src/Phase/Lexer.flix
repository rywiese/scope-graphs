mod ScopeGraph.Phase.Lexer {
    use Eq.eq

    use ScopeGraph.Ast.Lexed.Token
    use ScopeGraph.Ast.Shared.Location
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.Shared.Location.Loc
    use ScopeGraph.Ast.Shared.Location.LocRange
    use ScopeGraph.Ast.Shared.Location.LocString
    use ScopeGraph.Util.{StringCursor => SC}

    def err(msg: String, loc: Loc): t \ Crash =
        Crash.crash(Error.mkError(category = "Lexer", msg, loc))

    pub def lex(s: String): Vector[Token] \ Crash = region rc {
        s |> SC.mk(rc) |> lexCursor
    }

    def lexCursor(sc: SC[r]): Vector[Token] \ r + Crash = {
        let s = empty(sc |> SC.getRegion, sc);
        lexLoop(s);
        if (sc |> SC.eof) toVector(s)
        else err("Stuck at '${sc |> SC.peek |> escapedCharString}'", sc |> getLoc)
    }

    type alias State[r: Region] = {
        tokens = MutList[Token, r],
        sc = SC[r],
        previousEnd = Ref[Loc, r]
    }

    def empty(rc: Region[r], sc: SC[r]): State[r] \ r = {
        tokens = MutList.empty(rc),
        sc = sc,
        previousEnd = Ref.fresh(rc, Location.mkLoc(line = 0, column = 0, 0))
    }

    def collect(t: LocRange -> Token, s: State[r]): Unit \ r = {
        let start = s#previousEnd |> Ref.get;
        let end = s#sc |> getLoc;
        let range = Location.mkLocRange(start = start, end);
        Ref.put(end, s#previousEnd);
        MutList.push(t(range), s#tokens)
    }

    def toVector(s: State[r]): Vector[Token] \ r =
        MutList.toVector(s#tokens)

    def lexLoop(s: State[r]): Unit \ r + Crash = {
        let startOffset = s#sc |> SC.getOffset;

        lexWs(s);
        lexWord(s);
        lexParens(s);
        lexSymbols(s);
        lexInt(s);
        lexComment(s);

        let endOffset = s#sc |> SC.getOffset;
        // Loop if progress was made.
        if (startOffset < endOffset) lexLoop(s)
        else ()
    }

    def lexWs(s: State[r]): Unit \ r = {
        if (s#sc |> SC.peekIs(Char.isWhitespace, false)) {
            s#sc |> SC.advanceWhile(Char.isWhitespace);
            s |> collect(Token.Space)
        } else ()
    }

    def lexComment(s: State[r]): Unit \ r = {
        if (s#sc |> SC.advanceIfMatchString("//"))
            let comment = s#sc |> SC.advanceWhileAndCollect(c -> not (c == '\r' or c == '\n'));
            s |> collect(Token.Comment(comment))
        else ()
    }

    def lexWord(s: State[r]): Unit \ r = {
        if (s#sc |> SC.peekIs(Char.isLetter, false)) {
            let name = s#sc |> SC.advanceWhileAndCollect(c -> Char.isLetter(c) or Char.isDigit(c));
            s |> collect(Token.Name(name))
        } else ()
    }

    def lexInt(s: State[r]): Unit \ r + Crash = {
        if (s#sc |> SC.peekIs(Char.isDigit, false)) {
            let loc = s#sc |> getLoc;
            let intRaw = s#sc |> SC.advanceWhileAndCollect(Char.isDigit);
            let int = match Int32.fromString(intRaw) {
                case Some(i) => i
                case None => err("${intRaw} is too large", loc)
            };
            s |> collect(Token.Int(int))
        } else ()
    }

    def lexParens(s: State[r]): Unit \ r = {
        if (s#sc |> SC.advanceIfMatchChar('(')) {
            s |> collect(Token.ParenSoftLeft)
        } else if (s#sc |> SC.advanceIfMatchChar(')')) {
            s |> collect(Token.ParenSoftRight)
        } else if (s#sc |> SC.advanceIfMatchChar('{')) {
            s |> collect(Token.ParenCurlyLeft)
        } else if (s#sc |> SC.advanceIfMatchChar('}')) {
            s |> collect(Token.ParenCurlyRight)
        } else if (s#sc |> SC.advanceIfMatchChar('[')) {
            s |> collect(Token.ParenSquareLeft)
        } else if (s#sc |> SC.advanceIfMatchChar(']')) {
            s |> collect(Token.ParenSquareRight)
        } else ()
    }

    def lexSymbols(s: State[r]): Unit \ r = {
        if (s#sc |> SC.advanceIfMatchChar(',')) {
            s |> collect(Token.Comma)
        } else if (s#sc |> SC.advanceIfMatchChar('.')) {
            s |> collect(Token.Period)
        } else if (s#sc |> SC.advanceIfMatchChar('=')) {
            s |> collect(Token.Eq)
        } else if (s#sc |> SC.advanceIfMatchChar(':')) {
            s |> collect(Token.Colon)
        } else if (s#sc |> SC.advanceIfMatchChar(';')) {
            s |> collect(Token.Semicolon)
        } else ()
    }

    def getLoc(sc: SC[r]): Loc \ r =
        Location.mkLoc(
            line = SC.getLine(sc) + 1,
            column = SC.getColumn(sc) + 1,
            SC.getOffset(sc)
        )

    def escapedCharString(c: Char): String = {
        if (c == '\u0000') "EOF"
        else ToString.toString(c)
    }
}
