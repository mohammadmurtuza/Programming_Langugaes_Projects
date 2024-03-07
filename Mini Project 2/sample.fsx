// Helper function: Converts a single hex digit to its binary representation as a string
let hexDigitToBinary ch =
    match ch with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' | 'a' -> "1010"
    | 'B' | 'b' -> "1011"
    | 'C' | 'c' -> "1100"
    | 'D' | 'd' -> "1101"
    | 'E' | 'e' -> "1110"
    | 'F' | 'f' -> "1111"
    | _ -> failwith "Invalid hexadecimal digit"

// Converts a hexadecimal string to a binary string
let hexToBinary hexStr =
    hexStr |> Seq.map hexDigitToBinary |> String.concat ""

// Logical operation: NOT
let bitwiseNot binaryStr =
    binaryStr |> Seq.map (fun bit -> if bit = '0' then '1' else '0') |> String

// Logical operation: AND
let bitwiseAnd binaryStr1 binaryStr2 =
    Seq.zip binaryStr1 binaryStr2
    |> Seq.map (fun (bit1, bit2) -> if bit1 = '1' && bit2 = '1' then '1' else '0')
    |> String

// Logical operation: OR
let bitwiseOr binaryStr1 binaryStr2 =
    Seq.zip binaryStr1 binaryStr2
    |> Seq.map (fun (bit1, bit2) -> if bit1 = '1' || bit2 = '1' then '1' else '0')
    |> String

// Logical operation: XOR
let bitwiseXor binaryStr1 binaryStr2 =
    Seq.zip binaryStr1 binaryStr2
    |> Seq.map (fun (bit1, bit2) -> if bit1 <> bit2 then '1' else '0')
    |> String

// Example usage
let binaryA5 = hexToBinary "A5"
let binaryF1 = hexToBinary "F1"

let resultNotF1 = bitwiseNot binaryF1
let resultAndA5F1 = bitwiseAnd binaryA5 binaryF1
let resultOrA5F1 = bitwiseOr binaryA5 binaryF1
let resultXorA5F1 = bitwiseXor binaryA5 binaryF1

// Print results for verification
printfn "NOT 0xF1 -> %s" resultNotF1
printfn "0xA5 AND 0xF1 -> %s" resultAndA5F1
printfn "0xA5 OR 0xF1 -> %s" resultOrA5F1
printfn "0xA5 XOR 0xF1 -> %s" resultXorA5F1
