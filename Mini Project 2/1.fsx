

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
    let rec convertToDecimal lst pow acc =
        match lst with
        | [] -> acc
        | h :: t ->
            let value = h * pow
            convertToDecimal t (pow / 2) (acc + value)

    convertToDecimal binaryList (pown 2 (List.length binaryList - 1)) 0

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

// Check the Sum is zero
let checkSumToZero (num1: int) (num2: int) : bool =
    let binaryList1 = decimalToBinaryTailRecursive num1
    let binaryList2 = decimalToBinaryTailRecursive num2
    let sumResult = addBinary binaryList1 binaryList2
    List.forall (fun bit -> bit = 0) sumResult
// Define the functions NOT, addOne, and checkSumToZero here

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
    printfn "%3d -> %s" num2 bin2String 
    printfn "----     -------------"
    printfn "%d -> %s" sumDecimal binarySumString

    // Example usage of NOT, addOne, and checkSumToZero
    let negatedBin1 = NOT bin1
    let bin1PlusOne = addOne bin1
    let isSumZero = checkSumToZero num1 num2

    printfn "NOT of %d: %s" num1 (padBinaryString (String.concat "" (List.map string negatedBin1)))
    printfn "%d + 1: %s" num1 (padBinaryString (String.concat "" (List.map string bin1PlusOne)))
    printfn "Is the sum of %d and %d zero? %b" num1 num2 isSumZero

// Example usage
printBinaryOperation 30 0
