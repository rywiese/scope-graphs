mod SG.Ast.Shared {
    use Hash.combine
    use Hash.hash
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString

    pub enum Name({
        qualifiers = List[LocString],
        name = LocString,
        loc = LocRange
    })

    instance Coerce[Name] {
        type Out = {qualifiers = List[LocString], name = LocString, loc = LocRange}

        pub def coerce(x: Name): {qualifiers = List[LocString], name = LocString, loc = LocRange} = {
            let Name.Name(r) = x;
            r
        }
    }

    instance Hash[Name] {
        pub def hash(x: Name): Int32 =
            let {qualifiers, name, loc} = coerce(x);
            hash("Name")
                |> combine(hash(qualifiers))
                |> combine(hash(name))
                |> combine(hash(loc))
    }

    instance Eq[Name] {
        pub def eq(x: Name, y: Name): Bool = {
            let {qualifiers = qualifiersX, name = nameX, loc = _} = coerce(x);
            let {qualifiers = qualifiersY, name = nameY, loc = _} = coerce(y);
            qualifiersX == qualifiersY and nameX == nameY
        }
    }

    instance Located[Name] {
        pub def getLoc(x: Name): LocRange =
            coerce(x)#loc
    }

    instance ToString[Name] {
        pub def toString(x: Name): String =
            Name.toString(x)
    }

    mod Name {
        pub def toString(n: Name): String = {
            let {qualifiers, name, loc = _} = coerce(n);
            List.join(".", qualifiers `List.append` List#{name})
        }
    }
}
