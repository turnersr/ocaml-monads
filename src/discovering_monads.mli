type 'a with_counter = int -> 'a * int


val (>>=) : 'a with_counter -> ('a -> 'b with_counter) -> 'b with_counter