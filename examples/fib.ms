using Nondeterminism @ List {
    do f = return (rec f: Nat -> {} Nat. lambda n.
        do t = iszero n;
        if t
        then return n
        else
            do n' = pred n;
            do t' = iszero n';
            if t'
            then return n
            else
                do v' = f n';
                do n'' = pred n';
                do v'' = f n'';
                v' + v'');
    f 3
}
