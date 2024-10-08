type Poly = int list

//part 1

let rec add a b =
    match a, b with
    | [], _ -> b
    | _, [] -> a
    | a :: atail, b :: btail -> a + b :: add atail btail




// let a = [ 1; 2 ]
// let b = [ 1; 2; 3; 4 ]

// let res = add a b

// printfn "%A" res


let rec mulC factor list =
    match list with
    | [] -> []
    | head :: tail -> (factor * head) :: mulC factor tail


let rec sub a b =
    match a, b with
    | [], _ -> mulC -1 b
    | _, [] -> a
    | a :: atail, b :: btail -> a - b :: sub atail btail


// let a = [ 1; 2 ]
// let b = [ 3; 4; 5; 6 ]

// let res = sub a b

// printfn "%A" res

let mulX list = 0 :: list

// let res = mulX [ 1; 2; 3 ]

// printfn "%A" res

let rec mulALEXDAGOATANDDAOG a b =
    match a, b with
    | [], _ -> []
    | _, [] -> []
    //aHead :: aTail, bHead :: bTail -> add ((mulC aHead) (bHead :: bTail) (mul (mulX aTail)))
    | aHead :: aTail, bHead :: bTail ->
        add ((mulC aHead (bHead :: bTail))) (mulX (mulALEXDAGOATANDDAOG aTail (bHead :: bTail)))



// printfn "%A" res

let rec mul a b =
    match a with
    | [] -> [] // If the first polynomial is empty, the result is an empty polynomial.
    | aHead :: aTail ->
        // Multiply the current head of 'a' with the entire polynomial 'b',
        // then add the result to the recursive multiplication of 'aTail' with 'b'.
        add (mulC aHead b) (mulX (mul aTail b))


let eval value a =
    let rec evalInner value a counter =
        match a with
        | [] -> 0
        | aHead :: aTail -> aHead * (pown value counter) + evalInner value aTail (counter + 1)

    evalInner value a 0


//Part 2



let rec lastElementOflist list =
    match list with
    | [] -> None
    | [ x ] -> Some x
    | _ :: xTail -> lastElementOflist xTail




let isLegal list =
    let lastElement = lastElementOflist list

    match lastElement with
    | Some 0 -> false
    | Some _ -> true
    | None -> true
