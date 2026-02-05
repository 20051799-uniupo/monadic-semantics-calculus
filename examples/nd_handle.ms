using Nondeterminism @ List {
    handle
        do x = choose();
        return x
    with {
        choose() ->c return true,
        x -> return x
    }
}
