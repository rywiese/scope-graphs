mod ScopeGraph {
    use ScopeGraph.Ast.CheckResult

    pub struct Manager[id, r] {
        files: MutMap[id, String, r],
        mut cachedCompilation: Option[CheckResult]
    }
}

mod ScopeGraph.Manager {
    use ScopeGraph.Phase.File
    use ScopeGraph.Ast.Shared.Error
    use ScopeGraph.compile
    use ScopeGraph.Ast.Shared.Crash
    use ScopeGraph.Ast.CheckResult

    pub def empty(rc: Region[r]): Manager[id, r] \ r = {
        new Manager @ rc {
            files = MutMap.empty(rc),
            cachedCompilation = None
        }
    }

    pub def addSource(id: id, program: String, m: Manager[id, r]): Unit \ r with Order[id], ToString[id] = {
        MutMap.put(id, program, m->files);
        m->cachedCompilation = None
    }

    pub def addFile(id: id, path: String, m: Manager[id, r]): Unit \ r + FileRead with Order[id], ToString[id] = {
        addSource(id, FileRead.read(path), m)
    }

    pub def check(m: Manager[id, r]): CheckResult \ r + Crash = {
        match m->cachedCompilation {
            case Some(c) => c
            case None =>
                match m->files |> MutMap.valuesOf {
                    case one :: Nil =>
                        let compilation = one |> compile;
                        m->cachedCompilation = Some(compilation);
                        compilation
                    case other =>
                        bug!("Can only check 1 program, not '${other |> List.length}'")
                }
        }
    }

    pub def checkRes(m: Manager[id, r]): Result[Error, CheckResult] \ r = checked_ecast({
        run m |> check |> Result.Ok with Crash.runWith(Result.Err)
    })

}
