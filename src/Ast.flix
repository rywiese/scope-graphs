mod ScopeGraph.Ast {

    pub def loc(line: {line = Int32}, column: {column = Int32}, index: Int32): Loc =
        Loc.Loc({line = line#line, column = column#column, index = index})

    /// line is one-indexed.
    /// column is one-indexed.
    /// index is zero-indexed.
    pub enum Loc({line = Int32, column = Int32, index = Int32})

    instance Coerce[Loc] {
        type Out = {line = Int32, column = Int32, index = Int32}

        pub def coerce(x: Loc): {line = Int32, column = Int32, index = Int32} = {
            let Loc.Loc(r) = x;
            r
        }
    }

    instance Eq[Loc] {
        pub def eq(x: Loc, y: Loc): Bool =
            coerce(x)#index == coerce(y)#index
    }

    instance Order[Loc] {
        pub def compare(x: Loc, y: Loc): Comparison =
            coerce(x)#index <=> coerce(y)#index
    }

    instance ToString[Loc] {
        pub def toString(x: Loc): String = {
            let r = coerce(x);
            "${r#line}:${r#column}"
        }
    }

    pub def locRange(start: {start = Loc}, end: Loc): LocRange =
        LocRange.LocRange({start = start#start, end = end})

    /// Start is inclusive, end is exclusive.
    pub enum LocRange({start = Loc, end = Loc})

    instance Coerce[LocRange] {
        type Out = {start = Loc, end = Loc}

        pub def coerce(x: LocRange): {start = Loc, end = Loc} = {
            let LocRange.LocRange(r) = x;
            r
        }
    }

    instance Eq[LocRange] {
        pub def eq(x: LocRange, y: LocRange): Bool = {
            let {start = startX, end = endX} = coerce(x);
            let {start = startY, end = endY} = coerce(y);
            startX == startY and endX == endY
        }
    }

    instance ToString[LocRange] {
        pub def toString(x: LocRange): String = {
            let r = coerce(x);
            "${r#start} - ${r#end}"
        }
    }


    mod LocRange {
        /// Returns true if x is before y and doesn't overlap.
        pub def isBefore(x: LocRange, y: LocRange): Bool = {
            // <= since endX is exclusive.
            coerce(x)#end <= coerce(y)#start
        }

        /// Returns true if x and y overlap.
        pub def overlaps(x: LocRange, y: LocRange): Bool =
            not isBefore(x, y) and not isBefore(y, x)
    }

    pub def locString(s: String, loc: LocRange): LocString =
        LocString.LocString({s = s, loc = loc})

    pub enum LocString({s = String, loc = LocRange})

    instance Coerce[LocString] {
        type Out = {s = String, loc = LocRange}
        pub def coerce(x: LocString): {s = String, loc = LocRange} = {
            let LocString.LocString(r) = x;
            r
        }
    }

    instance Eq[LocString] {
        pub def eq(x: LocString, y: LocString): Bool =
            coerce(x)#s == coerce(y)#s
    }

    instance Order[LocString] {
        pub def compare(x: LocString, y: LocString): Comparison =
            coerce(x)#s <=> coerce(y)#s
    }

    instance ToString[LocString] {
        pub def toString(x: LocString): String = coerce(x)#s
    }

    mod LocString {
        pub def getString(s: LocString): String =
            coerce(s)#s
    }

    pub def name(qualifiers: List[LocString], name: LocString): Name =
        Name.Name({qualifiers = qualifiers, name = name})

    pub enum Name({qualifiers = List[LocString],  name = LocString})

    instance Coerce[Name] {
        type Out = {qualifiers = List[LocString], name = LocString}

        pub def coerce(x: Name): {qualifiers = List[LocString], name = LocString} = {
            let Name.Name(r) = x;
            r
        }
    }

    instance Eq[Name] {
        pub def eq(x: Name, y: Name): Bool = {
            let {qualifiers = qualifiersX, name = nameX} = coerce(x);
            let {qualifiers = qualifiersY, name = nameY} = coerce(y);
            qualifiersX == qualifiersY and nameX == nameY
        }
    }

    instance ToString[Name] {
        pub def toString(x: Name): String =
            Name.toString(x)
    }


    mod Name {
        use Functor.map
        use ScopeGraph.Ast.LocString

        pub def getSimple(n: Name): (List[String], String) =
            (coerce(n)#qualifiers |> map(LocString.getString), LocString.getString(coerce(n)#name))

        pub def toString(n: Name): String = {
            let {qualifiers, name} = coerce(n);
            "(${List.join(", ", qualifiers `List.append` List#{name})})"
        }
    }

    pub def module(name: Name, modules: List[Module]): Module =
        Module.Module({name = name, modules = modules})

    pub enum Module({name = Name, modules = List[Module]})

    instance Coerce[Module] {
        type Out = {name = Name, modules = List[Module]}

        pub def coerce(x: Module): {name = Name, modules = List[Module]} = {
            let Module.Module(r) = x;
            r
        }
    }

    mod Module {
        use ScopeGraph.Ast.Name;

        pub def getName(m: Module): Name = coerce(m)#name
    }


}
