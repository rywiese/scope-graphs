mod ScopeGraph.Ast.Shared {
    use Location.LocRange
    use Location.Loc

    pub enum Error({
        category = String,
        msg = String,
        loc = Option[LocRange]
    })

    mod Error {
        use ScopeGraph.Ast.Shared.Location.Loc
        use ScopeGraph.Ast.Shared.Location.LocRange

        pub def mkError(category: {category = String}, msg: String, loc: LocRange): Error =
            Error({category = category#category, msg = msg, loc = Some(loc)})

        pub def mkPointError(category: {category = String}, msg: String, loc: Loc): Error =
            Error({category = category#category, msg = msg, loc = Some(loc |> Loc.point)})

        pub def mkUnknownError(category: {category = String}, msg: String): Error =
            Error({category = category#category, msg = msg, loc = None})

        pub def toString(error: Error): String = {
            let {category, msg, loc = locOpt} = coerce(error);
            match locOpt {
                case Some(loc) => "[${category}] ${msg} (${loc})"
                case None => "[${category}] ${msg}"
            }
        }

        pub def toStringWithFile(file: String, error: Error): String = {
            let {category, msg, loc = locOpt} = coerce(error);
            match locOpt {
                case Some(loc) => "[${category}] ${msg} (${loc}; ${file})"
                case None => "[${category}] ${msg} (${file})"
            }
        }

    }

    instance Coerce[Error] {
        type Out = {category = String, msg = String, loc = Option[LocRange]}

        pub def coerce(x: Error): {category = String, msg = String, loc = Option[LocRange]} = {
            let Error.Error(r) = x;
            r
        }
    }

    instance ToString[Error] {
        pub def toString(x: Error): String =
            Error.toString(x)
    }



}
