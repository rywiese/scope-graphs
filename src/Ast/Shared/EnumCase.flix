mod SG.Ast.Shared {
    use Hash.combine
    use Hash.hash
    use SG.Ast.Shared.Location.Located
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location.LocString

    pub enum EnumCase({
        ac = Access,
        name = LocString,
        terms = List[Tpe],
        loc = LocRange
    })

    pub def mkEnumCase(ac: Access, name: LocString, terms: List[Tpe], loc: LocRange): EnumCase =
        EnumCase.EnumCase({ac = ac, name = name, terms = terms, loc = loc})

    instance Coerce[EnumCase] {
        type Out = {
            ac = Access,
            name = LocString,
            terms = List[Tpe],
            loc = LocRange
        }

        pub def coerce(x: EnumCase): {
            ac = Access,
            name = LocString,
            terms = List[Tpe],
            loc = LocRange
        } = {
            let EnumCase.EnumCase(r) = x;
            r
        }
    }

    instance Hash[EnumCase] {
        pub def hash(x: EnumCase): Int32 =
            let {ac, name, terms, loc} = coerce(x);
            hash("EnumCase")
                |> combine(hash(ac))
                |> combine(hash(name))
                |> combine(hash(terms))
                |> combine(hash(loc))
    }

    instance Located[EnumCase] {
        pub def getLoc(x: EnumCase): LocRange =
            coerce(x)#loc
    }

}
