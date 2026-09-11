pub mod SG.Ast.Shared.Location.Loc {
    use SG.Ast.Shared.Location.LocRange
    use SG.Ast.Shared.Location

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
