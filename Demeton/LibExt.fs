module LibExt

[<RequireQualifiedAccess>]
module Array2DExt =
    let toSeq (a: 'a[,]) : seq<'a> =
        seq {
            for i in 0 .. a.GetLength(0) - 1 do
                for j in 0 .. a.GetLength(1) - 1 do
                    yield a.[i, j]
        }
