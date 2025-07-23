mod SG {
    use SG.Ast.CheckResult
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.Source

    pub struct Manager[r] {
        files: MutMap[Source, String, r],
        mut cachedCompilation: Option[Validation[Error, CheckResult]],
        rc: Region[r]
    }
}

mod SG.Manager {
    use SG.Ast.CheckResult
    use SG.Ast.Lexed
    use SG.Ast.ParsedAst
    use SG.Ast.Shared.Crash
    use SG.Ast.Shared.Error
    use SG.Ast.Shared.Location.Source
    use SG.Phase.Lexer
    use SG.Phase.Parser
    use SG.Phase.Sanitizer
    use SG.Phase.Scoper

    pub def empty(rc: Region[r]): Manager[r] \ r = {
        new Manager @ rc {
            files = MutMap.empty(rc),
            cachedCompilation = None,
            rc = rc
        }
    }

    pub def updateSource(src: Source, program: String, m: Manager[r]): Unit \ r = {
        MutMap.put(src, program, m->files);
        m->cachedCompilation = None
    }

    pub def updateFile(path: String, m: Manager[r]): Unit \ r + FileRead = {
        updateSource(Source.File(path), FileRead.read(path), m)
    }

    pub def addFileIfNotPresent(path: String, m: Manager[r]): Unit \ r + FileRead = {
        if (m->files |> MutMap.memberOf(Source.File(path))) ()
        else updateSource(Source.File(path), FileRead.read(path), m)
    }

    pub def check(m: Manager[r]): Validation[Error, CheckResult] \ r = {
        match m->cachedCompilation {
            case Some(c) =>
                c
            case None =>
                let compilation = m->files |> MutMap.toMap |> compile;
                m->cachedCompilation = Some(compilation);
                compilation
        }
    }

    pub def compile(inputs: Map[Source, String]): Validation[Error, CheckResult] = {
        let res = forM (
            lexed <- Lexer.lex(inputs) |> Validation.toResult;
            parsed <- Parser.parse(lexed) |> Validation.toResult;
            _ <- Sanitizer.sanitize(parsed) |> Validation.toResult;
            scoped <- Scoper.resolve(parsed) |> Validation.toResult
        ) yield CheckResult.mk(parsed, scoped);
        match res {
            case Result.Ok(ok) => Validation.Success(ok)
            case Result.Err(errs) => Validation.Failure(errs)
        }
    }

}
