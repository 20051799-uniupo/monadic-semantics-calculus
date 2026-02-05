// { raise<...> } @ Either String
using Exceptions @ Exceptions {
    do dec = return lambda n: Nat.
        do t = iszero n;
        if t
        then raise<underflow>()
        else
            do n' = pred n;
            return n';
    dec 1
}
