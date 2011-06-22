fun suf xs = 
    let
        fun iter [] k = k [[]]
          | iter (l as (x :: xs)) k = iter xs (fn yss => k (l :: yss))
    in
        iter xs (fn x => x)
    end

fun pref xs = 
    let 
        fun iter []        f = [ f [] ]
          | iter (x :: xs) f = f [] :: iter xs (fn l => f (x :: l))
    in
        iter xs (fn x => x)
    end

fun pref_cps xs =
    let
        fun iter []        f k = k [ f [] ]
          | iter (x :: xs) f k = iter xs (fn l  => f (x :: l)) 
                                         (fn ls => k (f [] :: ls))
    in
        iter xs (fn x => x) (fn x => x)
    end
