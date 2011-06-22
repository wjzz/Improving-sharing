fun convolutions xs ys = 
    let
        fun iter []      = (ys, [])
          | iter (x::xs) =
            let 
                val (z::zs, conv) = iter xs
            in
                (zs, (x,z) :: conv)
            end
    in
        #2 (iter xs)
    end
    
fun conv_pref xss yss = 
    let
        fun iter [] = (xss, (fn x => x), (fn x => [] :: x))
          | iter (y::ys) =
            let
                val (x :: xs, buildPred, insertEnd) = iter ys
            in
                (xs, (fn l => buildPred ((x,y) :: l)), (fn ll => insertEnd (buildPred [(x,y)] :: ll)))
            end
    in
        (#3 (iter yss)) []
    end
