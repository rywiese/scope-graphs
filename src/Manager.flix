mod ScopeGraph {
    use ScopeGraph.Ast.Shared.Location.Source
    use ScopeGraph.Ast.CheckResult

    pub struct Manager[r] {
        files: MutMap[Source, String, r],
        mut cachedCompilation: Option[CheckResult]
    }
}

mod ScopeGraph.Manager {
    use ScopeGraph.Ast.Shared.Location.Source
    use ScopeGraph.Phase.File
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

    pub def addSource(src: Source, program: String, m: Manager[r]): Unit \ r = {
        MutMap.put(src, program, m->files);
        m->cachedCompilation = None
    }

    pub def addFile(src: Source, path: String, m: Manager[r]): Unit \ r + FileRead = {
        addSource(src, FileRead.read(path), m)
    }

    pub def check(m: Manager[r]): CheckResult \ r + Crash = {
        match m->cachedCompilation {
            case Some(c) => c
            case None =>
                match m->files |> MutMap.toList {
                    case (src, one) :: Nil =>
                        let compilation = one |> compile(src);
                        m->cachedCompilation = Some(compilation);
                        compilation
                    case other =>
                        bug!("Can only check 1 program, not '${other |> List.length}'")
                }
        }
    }

    pub def checkRes(m: Manager[r]): Result[Error, CheckResult] \ r = checked_ecast({
        run m |> check |> Result.Ok with Crash.runWith(Result.Err)
    })

}
