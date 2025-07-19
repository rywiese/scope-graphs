mod ScopeGraph {
    use ScopeGraph.Ast.Shared.Location.Source
    use ScopeGraph.Ast.CheckResult

    pub struct Manager[r] {
        files: MutMap[Source, String, r],
        mut cachedCompilation: Option[CheckResult]
    }
}

mod ScopeGraph.Manager {
    use ScopeGraph.Phase.Parser
    use ScopeGraph.Ast.ParsedAst
    use ScopeGraph.Phase.Lexer
    use ScopeGraph.Ast.Lexed
    use ScopeGraph.Ast.Shared.Location.Source
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.compile
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.CheckResult

    pub def empty(rc: Region[r]): Manager[r] \ r = {
        new Manager @ rc {
            files = MutMap.empty(rc),
            cachedCompilation = None
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
                Validation.Success(c)
            case None =>
                m->files |> MutMap.toMap |> compile
        }
    }

}
