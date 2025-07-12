mod ScopeGraph.Util.String {

    /// `n` is the max length returned INCLUDING '..'.
    ///
    /// `n < 3` is treated as `n = 3`
    pub def trimWithDots(n: Int32, s: String): String = {
        let m = if (n < 3) 3 else n;
        if (String.length(s) > m) {
            String.take(m-2, s) + ".."
        } else s
    }

}
