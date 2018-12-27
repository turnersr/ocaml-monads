open Base

let x = Set.of_list (module Int) [1;2;3] |> Set.to_list
let y = Set.union (Set.of_list (module Int) [1;2;3;2])
                  (Set.of_list (module Int) [3;5;1])
        |> Set.to_list   

type 'a tree = 
    | Leaf of 'a
    | Node of 'a tree * 'a tree


(* The Book of Mondas *)

let rec number_of_leaves tree = 
    match tree with 
    | Leaf _ -> 1 
    | Node(l,r) -> number_of_leaves l + number_of_leaves r


type 'a with_counter = int -> 'a * int


let rec relabel (tree : 'a tree) (i : int) (*: 'a tree withCounter *) = 
    match tree with 
    | Leaf x -> (Leaf (i, x), i+1)
    | Node(r, l) -> 
        let (l', i1) = relabel l i in 
        let (r', i2) = relabel r i1 in 
        Node(r', l'), i2

let (>>=) f g  =
    fun i -> 
    let (r, i') = f i in g r i'


let pure (x : 'a) (* : ('a withCounter) *)=  fun i -> (x, i)

let rec relabel (tree : 'a tree) = 
    match tree with 
    | Leaf x -> fun (i : int) -> (Leaf (i, x), i + 1)
    | Node (l, r) -> 
        relabel l >>= fun l' -> 
        relabel r >>= fun r' -> 
        let n = Node (l', r') in 
        pure n

let () =
let open Core.Printf in
    (*let x = List.map ~f:(fprintf Stdio.stdout ("%d")) y in *)
    let tree0 = Node ((Leaf 0), 
                        (Node ((Leaf 0), (Leaf 1)))) in 
    let nLeaves = number_of_leaves tree0 in 
    let rTree = relabel (Leaf 0) 0 in
    fprintf Stdio.stdout "%d\n" nLeaves;
