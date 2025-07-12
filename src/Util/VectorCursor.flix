mod ScopeGraph.Util {

    pub struct VectorCursor[t: Type, r: Region] {
        v: Vector[t],
        // Must always be non-negative.
        mut index: Int32
    }

    mod VectorCursor {
        use Vector.length

        pub def mk(rc: Region[r], v: Vector[t]): VectorCursor[t, r] \ r =
            new VectorCursor @ rc {
                v = v,
                index = 0
            }

        /// Returns true if the cursor has not reached end of file.
        pub def inBounds(sc: VectorCursor[t, r]): Bool \ r =
            sc->index < length(sc->v)

        pub def advance(vc: VectorCursor[t, r]): Unit \ r =
            if (inBounds(vc)) vc->index = vc->index + 1
            else ()

        pub def advanceIf(p: t -> Bool \ ef, vc: VectorCursor[t, r]): Bool \ ef + r = {
            if (peekIs(p, false, vc)) {advance(vc); true}
            else false
        }

        pub def advanceWhile(p: t -> Bool \ ef, vc: VectorCursor[t, r]): Unit \ ef + r = {
            if (advanceIf(p, vc)) advanceWhile(p, vc)
            else ()
        }

        pub def next(vc: VectorCursor[t, r]): Option[t] \ r =
            vc |> peek !> Option.forEach(_ -> advance(vc))

        pub def nextIf(p: t -> Bool \ ef, vc: VectorCursor[t, r]): Option[t] \ ef + r =
            match peek(vc) {
                case Some(elm) =>
                    if (p(elm)) {
                        advance(vc);
                        Some(elm)
                    } else None
                case None => None
            }

        pub def collect(f: a -> b \ ef, vc: VectorCursor[a, r]): Option[b] \ ef + r =
            vc |> next |> Option.map(f)

        pub def collectIf(f: a -> Option[b] \ ef, vc: VectorCursor[a, r]): Option[b] \ ef + r =
            vc |> peek |> Option.flatMap(f) !> Option.forEach(_ -> advance(vc))

        pub def peek(vc: VectorCursor[t, r]): Option[t] \ r =
            vc->v |> Vector.nth(vc->index)

        pub def peekIs(p: t -> Bool \ ef, orElse: Bool, vc: VectorCursor[t, r]): Bool \ ef + r =
            match peek(vc) {
                case Some(elm) => p(elm)
                case None => orElse
            }

        /// Returns the current element if possible,
        /// otherwise the previous if possible,
        /// otherwise None.
        pub def peekOrPrev(vc: VectorCursor[t, r]): Option[t] \ r = {
            match vc |> peek {
                case Some(elm) => Some(elm)
                case None => vc->v |> Vector.nth(vc->index - 1)
            }
        }

    }

}
