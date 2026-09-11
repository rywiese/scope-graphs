pub mod SG.Ast.ParsedAst.Name {
    pub def toString(n: Name): String = {
        let {qualifiers, name, loc = _} = coerce(n);
        List.join(".", qualifiers `List.append` List#{name})
    }
}
