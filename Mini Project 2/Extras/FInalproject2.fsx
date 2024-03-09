let decimalToBinaryTailRecursive num =
    let rec convertToBinary n acc i =
        if i < 0 then acc
        else
            let bit = (n >>> i) &&& 1
            convertToBinary n (bit :: acc) (i - 1)

    if num < 0 || num > 255 then failwith "Number must be between 0 and 255"
    else 
        let binaryList = convertToBinary num [] 7
        let paddedList = List.init (8 - List.length binaryList) (fun _ -> 0) @ binaryList
        List.rev paddedList

        
let binaryToDecimalTailRecursive binaryList =
    let rec binaryToDecimalTailRecursive lst pow acc =
        match lst with
        | [] -> acc
        | h :: t ->
            let value = h * pow
            binaryToDecimalTailRecursive t (pow / 2) (acc + value)

    binaryToDecimalTailRecursive binaryList (pown 2 (List.length binaryList - 1)) 0



let addBinary (binaryList1: int list) (binaryList2: int list) : int list =
    let rec addHelper list1 list2 carry result =
        match list1, list2 with
        | [], [] -> if carry = 1 then carry :: result else result
        | [], _
        | _, [] -> 
            let shorter, longer = if List.length list1 < List.length list2 then list1, list2 else list2, list1
            addHelper shorter (List.replicate (List.length longer - List.length shorter) 0 @ longer) carry result
        | bit1 :: tail1, bit2 :: tail2 ->
            let sum = bit1 + bit2 + carry
            let newBit = sum % 2
            let newCarry = if sum > 1 then 1 else 0
            addHelper tail1 tail2 newCarry (newBit :: result)

    let result = addHelper (List.rev binaryList1) (List.rev binaryList2) 0 []
    if result.[0] = 0 then result else List.rev result

let subtractBinary (binaryList1: int list) (binaryList2: int list) : int list =
    let rec subtractHelper list1 list2 borrow result =
        match list1, list2 with
        | [], [] -> if borrow = 1 then borrow :: result else result
        | [], _ -> failwith "Subtraction result is negative"
        | _, [] -> 
            let shorter, longer = if List.length list1 < List.length list2 then list1, list2 else list2, list1
            subtractHelper shorter (List.replicate (List.length longer - List.length shorter) 0 @ longer) borrow result
        | bit1 :: tail1, bit2 :: tail2 ->
            let difference = bit1 - bit2 - borrow
            let newBit = (difference + 2) % 2
            let newBorrow = if difference < 0 then 1 else 0
            subtractHelper tail1 tail2 newBorrow (newBit :: result)

    let result = subtractHelper (List.rev binaryList1) (List.rev binaryList2) 0 []
    if result.[0] = 0 then result else List.rev result

let binaryToString (binaryList: int list) (reverse: bool) : string =
    let processedList = if reverse then List.rev binaryList else binaryList
    processedList |> List.map string |> String.concat ""

// Dealing with negative numbers:
// Get absolute number
let absoluteValue (num: int) : int = if num < 0 then -num else num
// Perform NOT
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

// Logical operations Part
let convertHexToBinary (hex: int) : int list =
    let binaryString = System.Convert.ToString(hex, 2).PadLeft(8, '0')
    binaryString |> Seq.map (fun c -> int (c.ToString())) |> List.ofSeq
// AND:
let bitwiseAND (binaryList1: int list) (binaryList2: int list) : int list =
    List.map2 (fun bit1 bit2 -> if bit1 = 1 && bit2 = 1 then 1 else 0) binaryList1 binaryList2
// OR:
let bitwiseOR (binaryList1: int list) (binaryList2: int list) : int list =
    List.map2 (fun bit1 bit2 -> if bit1 = 1 || bit2 = 1 then 1 else 0) binaryList1 binaryList2
// XOR:
let bitwiseXOR (binaryList1: int list) (binaryList2: int list) : int list =
    List.map2 (fun bit1 bit2 -> if bit1 = bit2 then 0 else 1) binaryList1 binaryList2
// NOT:
let bitwiseNOT (binaryList: int list) : int list =
    List.map (fun bit -> if bit = 0 then 1 else 0) binaryList
let printBinaryOperation num1 num2 =
    let bin1 = decimalToBinaryTailRecursive num1
    let bin2 = decimalToBinaryTailRecursive num2
    let binarySum = addBinary bin1 bin2
    let sumDecimal = binaryToDecimalTailRecursive binarySum

    let padBinaryString binaryStr =
        let paddingLength = max (8 - String.length binaryStr) 0 
        String.replicate paddingLength "0" + binaryStr
    let bin1String = padBinaryString (String.concat "" (List.map string bin1))
    let bin2String = padBinaryString (String.concat "" (List.map string bin2))
    let binarySumString = padBinaryString (String.concat "" (List.map string binarySum))

    printfn "Addition: %d + %d" num1 num2
    printfn "%d -> %s" num1 bin1String
    printfn "+%2d -> %s" num2 bin2String 
    printfn "----     -------------"
    printfn "%d -> %s" sumDecimal binarySumString
    printfn "######################################################################################"
    printfn ""

let printSubtractionOperation num1 num2 =
    let bin1 = decimalToBinaryTailRecursive num1
    let bin2 = decimalToBinaryTailRecursive num2
    let binaryDifference = subtractBinary bin1 bin2
    let differenceDecimal = binaryToDecimalTailRecursive binaryDifference

    let padBinaryString binaryStr =
        let paddingLength = max (8 - String.length binaryStr) 0 
        String.replicate paddingLength "0" + binaryStr
    let bin1String = padBinaryString (String.concat "" (List.map string bin1))
    let bin2String = padBinaryString (String.concat "" (List.map string bin2))
    let binaryDifferenceString = padBinaryString (String.concat "" (List.map string binaryDifference))

    printfn "Subtraction: %d - %d" num1 num2
    printfn "%d -> %s" num1 bin1String
    printfn "-%3d -> %s" num2 bin2String 
    printfn "----     -------------"
    printfn "%d -> %s" differenceDecimal binaryDifferenceString
    printfn "######################################################################################"
    printfn ""



// print Binary Addition
let printBinaryAddition num1 num2 =
    let binaryList1 = decimalToBinaryTailRecursive (absoluteValue num1)
    let binaryList2 = decimalToBinaryTailRecursive (absoluteValue num2)

    if num1 < 0 && num2 < 0 then
        let complementBinary1 = addOne (NOT binaryList1)
        let complementBinary2 = addOne (NOT binaryList2)
        let result = addBinary complementBinary1 complementBinary2

        let truncatedResult =
            if List.length result > 8 then
                List.skip (List.length result - 8) result
            else
                result

        printfn " Addition: %d  %d = " num1 num2
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
        printfn "######################################################################################"
        printfn ""
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

        printfn "Subtraction: %d  %d " num1 num2
        printfn "%d -> %s" positiveNum (binaryToString positiveBinary false)

        printfn
            "%d -> %s"
            (absoluteValue negativeNum)
            (binaryToString (decimalToBinaryTailRecursive (absoluteValue negativeNum)) false)

        printfn "NOT -> %s" (binaryToString negativeBinary false)
        printfn "ADD 1 -> %s" (binaryToString addOneResult false)
        printfn "-%d -> %s" (absoluteValue negativeNum) (binaryToString addOneResult true)
        printfn "----     -------------"
        printfn "%d -> %s" (num1 + num2) (binaryToString truncatedResult true)
        printfn "######################################################################################"
        printf ""
    else
        let result = addBinary binaryList1 binaryList2

        let truncatedResult =
            if List.length result > 8 then
                List.skip (List.length result - 8) result
            else
                result

        printfn "Addition : %d + %d " num1 num2
        printfn "%d -> %s" num1 (binaryToString binaryList1 false)
        printfn "%d -> %s" num2 (binaryToString binaryList2 false)
        printfn "----     -------------"
        printfn "%d -> %s" (num1 + num2) (binaryToString truncatedResult true)
        printfn "######################################################################################"
        printfn ""




let printLogicalOperationsAND num1 num2 =
    let binaryList1 = convertHexToBinary num1
    let binaryList2 = convertHexToBinary num2
    printfn ""
    printfn "AND:"
    printfn "%X -> %s" num1 (binaryToString binaryList1 false)
    printfn "%X -> %s" num2 (binaryToString binaryList2 false)
    printfn "----     -------------"
    let result = bitwiseAND binaryList1 binaryList2
    printfn "%X -> %s" (binaryToDecimalTailRecursive result) (binaryToString result true)
    printfn ""
// OR
let printLogicalOperationsOR num1 num2 =
    let binaryList1 = convertHexToBinary num1
    let binaryList2 = convertHexToBinary num2
    printfn "OR:"
    printfn "%X -> %s" num1 (binaryToString binaryList1 false)
    printfn "%X -> %s" num2 (binaryToString binaryList2 false)
    printfn "----     -------------"
    let result = bitwiseOR binaryList1 binaryList2
    printfn "%X -> %s" (binaryToDecimalTailRecursive result) (binaryToString result false)
    printfn "######################################################################################"
    printfn ""
// XOR
let printLogicalOperationsXOR num1 num2 =
    let binaryList1 = convertHexToBinary num1
    let binaryList2 = convertHexToBinary num2
    printfn "XOR:"
    printfn "%X -> %s" num1 (binaryToString binaryList1 false)
    printfn "%X -> %s" num2 (binaryToString binaryList2 false)
    printfn "----     -------------"
    let result = bitwiseXOR binaryList1 binaryList2
    printfn "%X -> %s" (binaryToDecimalTailRecursive result) (binaryToString result false)
    printfn "######################################################################################"
    printfn ""
// NOT
let printLogicalOperationsNOT num =
    let binaryList = convertHexToBinary num
    printfn "NOT:"
    printfn " %X -> %s" num (binaryToString binaryList false)
    printfn "----     -------------"
    let result = bitwiseNOT binaryList
    printfn " %X -> %s" (binaryToDecimalTailRecursive result) (binaryToString result false)
    printfn "######################################################################################"
    printfn ""

// Arithmetic Operations
printBinaryOperation 30 7
printSubtractionOperation 30 10
// Run Arithmetic Operations print Part
printBinaryAddition -23 23
// Run Logical Operations print Part
printLogicalOperationsAND 0x48 0x84
printLogicalOperationsOR 0x47 0x84
printLogicalOperationsXOR 0xA5 0xF1
printLogicalOperationsNOT 0xA1




