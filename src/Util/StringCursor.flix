mod ScopeGraph.Util {

/// Iterate through a string while maintaining the line and column index of the
/// cursor.
pub struct StringCursor[r] {

    data: String,

    // The cursor pointing into `data`.
    mut offset: Int32,

    // The line zero-index of `offset`.
    mut line: Int32,

    // The column zero-index of `offset`.
    mut column: Int32,

    // The max column index of the previous line or `-1` if there is no previous
    // line.
    mut prevLineMaxColumn: Int32,

    // The character returned by peek, etc. if the cursor is out of bounds.
    eof: Char,

    rc: Region[r]

}

mod StringCursor {

    /// Returns the current line index (zero-indexed).
    pub def getLine(sc: StringCursor[r]): Int32 \ r =
        sc->line

    /// Returns the current column index (zero-indexed).
    pub def getColumn(sc: StringCursor[r]): Int32 \ r =
        sc->column

    /// Returns the current source offset (zero-indexed).
    pub def getOffset(sc: StringCursor[r]): Int32 \ r =
        sc->offset

    /// Returns the region of `sc`.
    pub def getRegion(sc: StringCursor[r]): Region[r] =
      sc->rc

    ///
    /// Returns `(line, column)` (zero-indexed) where non-existent positions are preferred instead
    /// of the first position of the next line.
    ///
    /// In this example, the current position is on 'v' (1,0) but this function will then return the
    /// position just after 'Example' (0, 7). This is sometimes preferable for exclusive end
    /// positions of ranges.
    ///
    /// {{{
    ///   Example /   v
    /// }}}
    ///
    pub def getExclusiveEndPosition(sc: StringCursor[r]): {line = Int32, column = Int32} \ r =
      if (sc->line <= 0 or sc->column > 0) {
        {line = sc->line, column = sc->column}
      } else if (sc->prevLineMaxColumn < 0) {
        ?bug
      } else {
        {line = sc->line - 1, column = sc->prevLineMaxColumn + 1}
      }

    ///
    /// Advances cursor one char forward, returning the char it was previously sitting on.
    ///
    /// If the cursor has advanced past the content, EOF is returned.
    ///
    pub def advance(sc: StringCursor[r]): Unit \ r =
      discard peekAndAdvance(sc)

    ///
    /// Advances cursor one char forward, returning the char it was previously sitting on.
    ///
    /// If the cursor has advanced past the content, EOF is returned.
    ///
    pub def peekAndAdvance(sc: StringCursor[r]): Char \ r = {
      if (eof(sc)) {
        sc->eof
      } else {
        let c = sc |> getChar(sc->offset);
        if (c == '\n') {
          sc->prevLineMaxColumn = sc->column;
          sc->line = sc->line + 1;
          sc->column = 0
        } else {
          sc->column = sc->column + 1
        };
        sc->offset = sc->offset + 1;
        c
      }
    }

    /// Peeks the character that is `n` characters ahead of the cursor if available.
    pub def nth(n: Int32, sc: StringCursor[r]): Option[Char] \ r = {
      let index = sc->offset + n;
      if (0 <= index and index < String.length(sc->data)) {
        Some(getChar(index, sc))
      } else {
        None
      }
    }

    /// Returns the ith character of `sc->data` or crashes.
    def getChar(i: Int32, sc: StringCursor[r]): Char =
        String.charAt(i, sc->data)

    /// Peeks the previous character if available.
    pub def previous(sc: StringCursor[r]): Option[Char] \ r =
        nth(-1, sc)

    ///
    /// Peeks the character that cursor is currently sitting on without advancing.
    ///
    /// If the cursor has advanced past the content, EOF is returned.
    ///
    pub def peek(sc: StringCursor[r]): Char \ r =
      if (inBounds(sc)) {
        getChar(sc->offset, sc)
      } else {
        sc->eof
      }

    /// Returns true if `c` is the content at the current position.
    pub def at(c: Char, sc: StringCursor[r]): Bool \ r = {
        if (inBounds(sc)) {
            getChar(sc->offset, sc) == c
        } else {
            false
        }
    }

    /// Returns true if the cursor has moved past the end.
    pub def eof(sc: StringCursor[r]): Bool \ r =
        sc->offset >= String.length(sc->data)

    /// Returns true if the cursor has not reached end of file.
    pub def inBounds(sc: StringCursor[r]): Bool \ r =
        sc->offset < String.length(sc->data)

    ///
    /// Advance the cursor past `s` if it matches the current content.
    ///
    /// Returns true if the cursor was advanced.
    ///
    pub def advanceIfMatchString(s: String, sc: StringCursor[r]): Bool \ r = {
      if (sc->offset + String.length(s) > String.length(sc->data)) {
        false
      } else region local {
        def loop(sIndex) = {
            if (sIndex < String.length(s)) {
                if (getChar(sc->offset + sIndex, sc) != String.charAt(sIndex, s)) {
                    false
                } else {
                    loop(sIndex + 1)
                }
            } else true
        };
        if (loop(0)) {
            Iterator.range(local, 0, String.length(s)) |> Iterator.forEach(_ -> advance(sc));
            true
        } else {
            false
        }
      }
    }

    ///
    /// Advance the cursor past `c` if it matches the current char.
    ///
    /// Returns true if the cursor was advanced.
    ///
    pub def advanceIfMatchChar(c: Char, sc: StringCursor[r]): Bool \ r =
      if (inBounds(sc) and getChar(sc->offset, sc) == c) {
        advance(sc);
        true
      } else {
        false
      }

    /// Continuously advance the cursor while `p` returns true.
    pub def advanceWhile(p: Char -> Bool \ ef, sc: StringCursor[r]): Unit \ ef + r =
      if (inBounds(sc) and p(getChar(sc->offset, sc))) {
        advance(sc);
        advanceWhile(p, sc)
      } else {
        ()
      }

    /// Continuously advance the cursor while `p` returns true. Returns the number of advances.
    pub def advanceWhileWithCount(p: Char -> Bool, sc: StringCursor[r]): Int32 \ r = {
      let startingOffset = sc->offset;
      advanceWhile(p, sc);
      sc->offset - startingOffset
    }

    /// Continuously advance the cursor while `p` returns true. Returns the number of advances.
    pub def advanceWhileAndCollect(p: Char -> Bool, sc: StringCursor[r]): String \ r = {
      let startingOffset = sc->offset;
      advanceWhile(p, sc);
      String.slice(start = startingOffset, end = sc->offset, sc->data)
    }

    /// Faster equivalent of `nth(n).map(p).getOrElse(orElse)`.
    pub def nthIs(n: Int32, p: Char -> Bool, orElse: Bool, sc: StringCursor[r]): Bool \ r = {
      let index = sc->offset + n;
      if (0 <= index and index < String.length(sc->data)) {
        p(getChar(index, sc))
      } else {
        orElse
      }
    }

    pub def peekIs(p: Char -> Bool, orElse: Bool, sc: StringCursor[r]): Bool \ r = {
      if (inBounds(sc)) {
        p(getChar(sc->offset, sc))
      } else {
        orElse
      }
    }

    pub def mk(rc: Region[r], s: String): StringCursor[r] \ r = {
        new StringCursor @ rc {
            data = s,
            offset = 0,
            line = 0,
            column = 0,
            prevLineMaxColumn = -1,
            eof = '\u0000',
            rc = rc
        }
    }

    /// Returns a copy of `this`, pointing to the same underlying string.
    pub def copy(rc: Region[r1], sc: StringCursor[r2]): StringCursor[r1] \ r1 + r2 = {
        let scNew = new StringCursor @ rc {
        data = sc->data,
        offset = sc->offset,
        line = sc->line,
        column = sc->column,
        prevLineMaxColumn = sc->prevLineMaxColumn,
        eof = sc->eof,
        rc = rc
      };
      scNew
    }
}
}
