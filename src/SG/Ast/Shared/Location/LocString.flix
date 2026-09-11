pub mod SG.Ast.Shared.Location.LocString {
    use SG.Ast.Shared.Location.LocRange

    pub def getString(s: LocString): String =
        coerce(s)#s

    pub def isUpperCase(s: LocString): Bool = {
        match s |> getString |> String.nth(0) {
            case Some(c) => Char.isUpperCase(c)
            case None => false
        }
    }

    pub def isLowerCase(s: LocString): Bool = {
        match s |> getString |> String.nth(0) {
            case Some(c) => Char.isLowerCase(c)
            case None => false
        }
    }

    pub def getLoc(x: LocString): LocRange =
        coerce(x)#loc
}
