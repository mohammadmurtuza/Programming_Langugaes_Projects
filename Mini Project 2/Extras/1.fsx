// Converting non-negative decimal numbers to binary
let convertToBinary (num: int) : int list =
    if num < -128 || num > 127 then
        failwith "Number is out of the 8-bit signed integer range. Please type number -128 to 127"

    let rec toBinary num acc =
        if num = 0 && acc = [] then [ 0 ]
        else if num <= 0 then acc
        else toBinary (num / 2) ((num % 2) :: acc)

    let binaryList = toBinary num [] |> List.rev
    List.replicate (8 - List.length binaryList) 0 @ binaryList

// Converting negative decimal numbers to binary:
// Get absolute number
let absoluteValue (num: int) : int = if num < 0 then -num else num
// Invert all the values (NOT operation)
let NOT (binaryList: int list) : int list =
    List.map (fun bit -> if bit = 0 then 1 else 0) binaryList
// Add one
let addOne (binaryList: int list) : int list =
    let rec addOneHelper list carry result =
        match list with
        | [] -> if carry = 1 then [ 1 ] else result
        | bit :: tail ->
            let sum = bit + carry
            let newBit = sum % 2
            let newCarry = if sum > 1 then 1 else 0
            addOneHelper tail newCarry (newBit :: result)

    addOneHelper (List.rev binaryList) 1 []

let binaryToString (binaryList: int list) (reverse: bool) : string =
    let processedList = if reverse then List.rev binaryList else binaryList
    processedList |> List.map string |> String.concat ""
// Addition:
let addBinary (binaryList1: int list) (binaryList2: int list) : int list =
    let rec addHelper list1 list2 carry result =
        match list1, list2 with
        | [], [] -> if carry = 1 then result else result
        | [], _
        | _, [] -> failwith "Input lists must have the same length"
        | bit1 :: tail1, bit2 :: tail2 ->
            let sum = bit1 + bit2 + carry
            let newBit = sum % 2
            let newCarry = if sum > 1 then 1 else 0
            addHelper tail1 tail2 newCarry (newBit :: result)

    let result = addHelper (List.rev binaryList1) (List.rev binaryList2) 0 []
    List.rev result

// Test Binary Addition
let testBinaryAddition num1 num2 =
    let binaryList1 = convertToBinary (absoluteValue num1)
    let binaryList2 = convertToBinary (absoluteValue num2)

    if num1 < 0 && num2 < 0 then
        let complementBinary1 = addOne (NOT binaryList1)
        let complementBinary2 = addOne (NOT binaryList2)
        let result = addBinary complementBinary1 complementBinary2

        let truncatedResult =
            if List.length result > 8 then
                List.skip (List.length result - 8) result
            else
                result

        printfn "The Addition of %d and %d is:" num1 num2
        printfn "%d -> %s" (absoluteValue num1) (binaryToString binaryList1 false)
        printfn "NOT -> %s" (binaryToString (NOT binaryList1) false)
        printfn "ADD 1 -> %s" (binaryToString complementBinary1 false)
        printfn "-%d -> %s" (absoluteValue num1) (binaryToString complementBinary1 false)
        printfn "%d -> %s" (absoluteValue num2) (binaryToString binaryList2 false)
        printfn "NOT -> %s" (binaryToString (NOT binaryList2) false)
        printfn "ADD 1 -> %s" (binaryToString complementBinary2 false)
        printfn "-%d -> %s" (absoluteValue num2) (binaryToString complementBinary2 false)
        printfn "********************"
        printfn "%d -> %s" (num1 + num2) (binaryToString truncatedResult true)
    elif num1 < 0 || num2 < 0 then
        let positiveNum = if num1 > 0 then num1 else num2
        let negativeNum = if num1 < 0 then num1 else num2
        let positiveBinary = if num1 > 0 then binaryList1 else binaryList2
        let negativeBinary = NOT(if num1 < 0 then binaryList1 else binaryList2)
        let addOneResult = addOne negativeBinary
        let result = addBinary positiveBinary addOneResult

        let truncatedResult =
            if List.length result > 8 then
                List.skip (List.length result - 8) result
            else
                result

        printfn "The Addition of %d and %d is:" num1 num2
        printfn "%d -> %s" positiveNum (binaryToString positiveBinary false)

        printfn
            "%d -> %s"
            (absoluteValue negativeNum)
            (binaryToString (convertToBinary (absoluteValue negativeNum)) false)

        printfn "NOT -> %s" (binaryToString negativeBinary false)
        printfn "ADD 1 -> %s" (binaryToString addOneResult false)
        printfn "-%d -> %s" (absoluteValue negativeNum) (binaryToString addOneResult true)
        printfn "********************"
        printfn "%d -> %s" (num1 + num2) (binaryToString truncatedResult true)
    else
        let result = addBinary binaryList1 binaryList2

        let truncatedResult =
            if List.length result > 8 then
                List.skip (List.length result - 8) result
            else
                result

        printfn "The Addition of %d and %d is:" num1 num2
        printfn "%d -> %s" num1 (binaryToString binaryList1 false)
        printfn "%d -> %s" num2 (binaryToString binaryList2 false)
        printfn "********************"
        printfn "%d -> %s" (num1 + num2) (binaryToString truncatedResult true)

// Run Arithmetic Operations Test Part
testBinaryAddition 23 23
