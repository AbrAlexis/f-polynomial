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


// let Thom = mul [2;3;0;1] [1;2;3]

// printfn "%A" Thom


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


let restt = isLegal [ 1; 2; 3; 4 ]

// printfn "%A" restt


let rec removeLastElement list =
    match list with
    | [] -> []
    | [ x ] -> []
    | x :: xTail -> x :: removeLastElement xTail

let svar = removeLastElement [ 1; 2 ]

// printfn "%A" svar

let rec ofList list =
    let legal = isLegal list

    match legal with
    | true -> list
    | false -> ofList (removeLastElement list)



// let svar1 = ofList [ 1; 2; 0; 1; 0 ]

// printfn "%A" svar1


let toString list= 
    let rec toStringCounter list counter=
        match list, counter with
        | [], _ -> ""
        | [head], 0 -> string head  + " "
        | [head], 1 -> string head  + "x "
        | [head], _ -> string head  + "x^" + string counter
        | 0::tail, _ -> toStringCounter tail (counter + 1)
        | head::tail, 0 -> string head + " " +  "+ " + toStringCounter tail (counter + 1)
        | head::tail, 1 -> string head  + "x " + "+ " + toStringCounter tail (counter + 1)
        | head::tail, _ -> string head + "x^" +  string counter + " " + "+ " + (toStringCounter tail (counter + 1))

    toStringCounter list 0


// let tester = toString [1;1;1;2;3;0;5]

// printfn "%A" tester


let derivative list=
    let newList = List.tail list
    let rec derivativeCounter derivative power=
        match derivative with
        |[] -> []
        |[head] -> (head*power)::derivativeCounter [] (power + 1)
        |head::tail ->  head*power::derivativeCounter tail (power + 1)
    
    derivativeCounter  newList 1


// let dev = derivative [1;2;2;2]

// printfn "%A" dev


let rec mulRepeated listA amount =
    match amount with
    | 0 -> [1] // Assuming [1] acts as the identity in your `mul` function
    | 1 -> listA
    | _ -> mul listA (mulRepeated listA (amount - 1))

// let potens = mulRepeated [0;0;10;10] 0

// printfn "%A" potens


let compose listA listB=
    let rec composeCounter listA listB counter =
        match listA with
        | [] ->  [] 
        | aHead::aTail -> 
            add (mulC aHead (mulRepeated listB counter)) (composeCounter aTail listB (counter + 1 )) 
        
    composeCounter listA listB 0


let compose2 listA listB =
    let rec composeCounter listA listB counter =
        match listA with
        | [] -> []
        | aHead :: aTail -> 
            add (mulC aHead (mulRepeated listB counter)) (composeCounter aTail listB (counter + 1))
    composeCounter listA listB 0


// let pleaseJESUS = compose [2;0;0;4] [0;3;2]

// printfn "%A" pleaseJESUS

