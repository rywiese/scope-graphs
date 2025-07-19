mod ScopeGraph.Ast.Shared.Location {
    use ScopeGraph.Ast.Shared.Location.Located

    pub enum Source with Eq, ToString, Order {
        case File(String)
    }

    pub trait Located[t] {
        pub def getLoc(x: t): LocRange
    }

    pub def mkLoc(line: {line = Int32}, column: {column = Int32}, index: Int32, src: Source): Loc = {
        if (line#line < 1) bug!("line is negative")
        else if (column#column < 1) bug!("column is negative")
        else Loc.Loc({line = line#line, column = column#column, index = index, src = src})
    }

    /// line is one-indexed.
    /// column is one-indexed.
    /// index is zero-indexed.
    pub enum Loc({line = Int32, column = Int32, index = Int32, src = Source})

    instance Coerce[Loc] {
        type Out = {line = Int32, column = Int32, index = Int32, src = Source}

        pub def coerce(x: Loc): {line = Int32, column = Int32, index = Int32, src = Source} = {
            let Loc.Loc(r) = x;
            r
        }
    }

    instance Eq[Loc] {
        pub def eq(x: Loc, y: Loc): Bool = {
            let {line = lineX, column = columnX, index = indexX, src = srcX} = coerce(x);
            let {line = lineY, column = columnY, index = indexY, src = srcY} = coerce(y);
            if (srcX != srcY){
                false
            } else if (indexX != -1 and indexY != -1) {
                indexX == indexY
            } else {
                lineX == lineY and columnX == columnY
            }
        }
    }

    instance PartialOrder[Loc] {
        pub def lessEqual(x: Loc, y: Loc): Bool = {
            let {line = lineX, column = columnX, index = indexX, src = srcX} = coerce(x);
            let {line = lineY, column = columnY, index = indexY, src = srcY} = coerce(y);
            if (srcX != srcY){
                false
            } else if (indexX != -1 and indexY != -1) {
                indexX <= indexY
            } else {
                (lineX, columnX) <= (lineY, columnY)
            }
        }
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
            let {line, column, index, src} = coerce(l);
            let end = Location.mkLoc(line = line, column = column + 1, index, src);
            Location.mkLocRange(start = l, end)
        }

        pub def containedIn(range: LocRange, loc: Loc): Bool = {
            coerce(range)#start `beforeEq` loc and loc `before` coerce(range)#end
        }

        /// Returns false for locations of different sources.
        pub def before(x: Loc, y: Loc): Bool = {
            let {line = lineX, column = columnX, index = indexX, src = srcX} = coerce(x);
            let {line = lineY, column = columnY, index = indexY, src = srcY} = coerce(y);
            if (srcX != srcY){
                false
            } else if (indexX != -1 and indexY != -1) {
                indexX < indexY
            } else {
                (lineX, columnX) < (lineY, columnY)
            }
        }

        /// Returns false for locations of different sources.
        pub def beforeEq(x: Loc, y: Loc): Bool = {
            let {line = lineX, column = columnX, index = indexX, src = srcX} = coerce(x);
            let {line = lineY, column = columnY, index = indexY, src = srcY} = coerce(y);
            if (srcX != srcY){
                false
            } else if (indexX != -1 and indexY != -1) {
                indexX <= indexY
            } else {
                (lineX, columnX) <= (lineY, columnY)
            }
        }
    }

    pub def mkLocRange(start: {start = Loc}, end: Loc): LocRange = {
        let startSrc = coerce(start#start)#src;
        if (startSrc != coerce(end)#src) bug!("${startSrc} not the same src as ${coerce(end)#src}")
        else LocRange.LocRange({start = start#start, end = end, src = startSrc})
    }

    /// Returns the smallest range containing both.
    pub def union(x: LocRange, y: LocRange): LocRange = {
        let {start = startX, end = endX, src = src} = coerce(x);
        let {start = startY, end = endY, src = srcOther} = coerce(y);
        if (src != srcOther) bug!("${src} not the same location as ${srcOther}")
        else {
            let start = if (startX `Loc.beforeEq` startY) startX else startY;
            let end = if (endX `Loc.beforeEq` endY) endY else endX;
            mkLocRange(start = start, end)
        }
    }

    pub def unionAll(l: LocRange, ls: it): LocRange \ Iterable.Aef[it] with Iterable[it] where Iterable.Elm[it] ~ LocRange = {
        region rc {
            (l, Iterable.iterator(rc, ls)) ||> Iterator.foldLeft(union)
        }
    }

    /// Start is inclusive, end is exclusive.
    pub enum LocRange({start = Loc, end = Loc, src = Source})

    instance Coerce[LocRange] {
        type Out = {start = Loc, end = Loc, src = Source}

        pub def coerce(x: LocRange): {start = Loc, end = Loc, src = Source} = {
            let LocRange.LocRange(r) = x;
            r
        }
    }

    instance Eq[LocRange] {
        pub def eq(x: LocRange, y: LocRange): Bool = {
            let {start = startX, end = endX, src = srcX} = coerce(x);
            let {start = startY, end = endY, src = srcY} = coerce(y);
            startX == startY and endX == endY and srcX == srcY
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
