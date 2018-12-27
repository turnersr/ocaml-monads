type 'a withCounter = int -> 'a * int


val (>>=) : 'a withCounter -> ('a -> 'b withCounter) -> 'b withCounter