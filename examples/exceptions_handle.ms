// { raise<...> } @ Either String
using Exceptions @ Exceptions {
    do dec = return lambda n: Nat.
        do t = iszero n;
        handle 
            if t
            then raise<underflow>()
            else
                do n' = pred n;
                return n'
        with {
            raise<underflow>() ->s return 0,
            n -> return n
        };
    dec 0
}
