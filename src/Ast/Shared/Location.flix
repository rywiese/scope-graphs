mod ScopeGraph.Ast.Shared.Location {
    use ScopeGraph.Ast.Shared.Location.Located

    pub trait Located[t] {
        pub def getLoc(x: t): LocRange
    }

    pub def mkLoc(line: {line = Int32}, column: {column = Int32}, index: Int32): Loc =
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

    mod Loc {
        use ScopeGraph.Ast.Shared.Location.LocRange
        use ScopeGraph.Ast.Shared.Location

        pub def point(l: Loc): LocRange = {
            let {line, column, index} = coerce(l);
            let end = Location.mkLoc(line = line, column = column, index);
            Location.mkLocRange(start = l, end)
        }

        pub def containedIn(range: LocRange, loc: Loc): Bool = {
            coerce(range)#start <= loc or loc < coerce(range)#end
        }
    }

    pub def mkLocRange(start: {start = Loc}, end: Loc): LocRange =
        LocRange.LocRange({start = start#start, end = end})

    /// Returns the smallest range containing both.
    pub def union(x: LocRange, y: LocRange): LocRange = {
        let {start = startX, end = endX} = coerce(x);
        let {start = startY, end = endY} = coerce(y);
        let start = if (startX <= startY) startX else startY;
        let end = if (endX <= endY) endY else endX;
        mkLocRange(start = start, end)
    }

    pub def unionAll(l: LocRange, ls: it): LocRange \ Iterable.Aef[it] with Iterable[it] where Iterable.Elm[it] ~ LocRange = {
        region rc {
            (l, Iterable.iterator(rc, ls)) ||> Iterator.foldLeft(union)
        }
    }

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

    instance Located[LocRange] {
        pub def getLoc(x: LocRange): LocRange =
            x
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

    pub def mkLocString(s: String, loc: LocRange): LocString =
        LocString.LocString({s = s, loc = loc})

    pub enum LocString({s = String, loc = LocRange})

    instance Coerce[LocString] {
        type Out = {s = String, loc = LocRange}
        pub def coerce(x: LocString): {s = String, loc = LocRange} = {
            let LocString.LocString(r) = x;
            r
        }
    }

    instance Located[LocString] {
        pub def getLoc(x: LocString): LocRange =
            LocString.getLoc(x)
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
        use ScopeGraph.Ast.Shared.Location.LocRange

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

}
