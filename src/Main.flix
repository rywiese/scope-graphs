def main(): Unit \ IO = {
    discard ScopeGraph.Parsing.File.parseFileUnsafe("./examples/misc.txt");
    println("parsed successfully")
}
