(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let len       = List.length
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

let trimzeros list =
    let rec trimzeros' list' = match list' with
        | []       -> []
        | [0]      -> []
        | car::cdr ->
             let cdr' = trimzeros' cdr
             in  match car, cdr' with
                 | 0, [] -> []
                 | car, cdr' -> car::cdr'
    in trimzeros' list

    let string_of_list clist =
        match clist with
        | []   -> "0"
        | clist -> let reversed = reverse clist
                   in strcat ""
                      (map string_of_int reversed)

    let rec cmp' l1 l2 =
       if (car l1) > (car l2) then true
       else if (car l1) < (car l2) then false
       else cmp' (cdr l1) (cdr l2)

    let cmp list1 list2 = 
       if len list1 > len list2 then list1
       else if list1 = list2 then list1
       else if len list1 < len list2 then list2
       else if cmp' (reverse list1) (reverse list2) 
            then list1 (* says if list1 > list2 return list 1 *)
       else list2 (* says if list1 < list2 return list 2 *)

    let rec canon' list1 list2 result = 
       if len list1 > len list2 && len result > len list1 
       then canon' list1 list2 (cdr result)
       else if len list2 > len list1 && len result > len list2
       then canon' list1 list2 (cdr result)
       else if (car result) = 0 
       then canon' list1 list2 (cdr result) 
       else (reverse result) 

    let canon list1 list2 result = canon' list1 list2 result   
 
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
       | list1, [], 0       -> list1
       | [], list2, 0       -> list2 (*raise error*)
       | list1, [], carry   -> sub' list1 [carry] 0
       | [], list2, carry   -> sub' [carry] list2 0 
       | car1::cdr1, car2::cdr2, carry ->
         (*let diff = (car1 - car2) + radix + carry
         in diff mod radix :: sub' cdr1 cdr2 ( diff/radix - 1 ) *)
         let diff = if  car1 >= car2 then car1 - car2 - carry 
                    else car1 - car2 - carry + 10  
         in diff :: if car1 >= car2 then sub' cdr1 cdr2 0
                    else sub' cdr1 cdr2 1 

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if value1 = value2 && neg1 != neg2 then zero 
        else  if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if cmp value1 value2 = value1 
then Bigint (neg1, canon value1 value2 (reverse (sub' value1 value2 0)))
else Bigint (neg2, canon value1 value2 (reverse (sub' value2 value1 0)))

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if cmp value1 value2 = value1 && neg1 = neg2 && neg1 = Pos
then Bigint (Pos,
(canon value1 value2 (reverse (sub' value1 value2 0))))
        else if cmp value1 value2 = value1 && neg1 = neg2 && neg1 = Neg 
then Bigint (Neg, 
(canon value1 value2 (reverse (sub' value1 value2 0))))
        else if cmp value1 value2 = value2 && neg1 = neg2 && neg1 = Pos
then Bigint (Neg, 
(canon value1 value2 (reverse (sub' value2 value1 0))))
        else if cmp value1 value2 = value2 && neg1 = neg2 && neg1 = Neg
then Bigint (Pos, 
(canon value1 value2 (reverse (sub' value2 value1 0))))
        else if cmp value1 value2 = value1 && neg1 = Pos
             then Bigint (Pos, add' value1 value2 0) 
        else if cmp value1 value2 = value1 && neg1 = Neg 
             then Bigint (Neg, add' value1 value2 0)
        else if cmp value1 value2 = value2 && neg2 = Neg
             then Bigint (Pos, add' value1 value2 0)
        else Bigint (Neg, add' value1 value2 0)
 
    let digits2 d =
        let rec dig acc d =
            if d < 10 then d::acc
            else dig ((d mod 10)::acc) (d/10) in
        dig [] d

   let double number = 
        add' number number 0


 let rec mul' (multiplier, powerof2, multiplicand') =
    if cmp powerof2 multiplier = powerof2 
      then multiplier, [0]
    else let remainder, product =
      mul' (multiplier, (double powerof2), (double multiplicand'))
      in if trimzeros(remainder) = powerof2
          then ( [0] , add' product multiplicand' 0)
         else if (cmp (trimzeros( remainder)) powerof2 = powerof2) 
           then  (trimzeros( remainder)), product
         else (( trimzeros (sub' remainder powerof2 0)), 
              add' product multiplicand' 0)
 
 let mul ( Bigint(neg1, multiplier)) (Bigint( neg2, multiplicand)) =
        if cmp multiplier multiplicand = 
multiplier then let _, product =
mul' (multiplier, [1], multiplicand)in Bigint( neg1, product ) 
        else  let _, product = 
mul' (multiplicand, [1], multiplier)in Bigint( neg1, product )

    let rec divrem' (dividend, powerof2, divisor') =
      if cmp divisor' dividend = divisor'
      then [0], dividend
      else let quotient, remainder =
         divrem' (dividend, double powerof2, double divisor')
       in if cmp (trimzeros(remainder)) divisor' = divisor' 
         then quotient, (trimzeros remainder)
         else (add' quotient powerof2 0, 
               trimzeros (sub' remainder divisor' 0))

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) =
        let quotient,_ = divrem (dividend, divisor)  in
        if neg1 = neg2 then Bigint(Pos, quotient)
        else Bigint(Neg, quotient)

    let rem (Bigint(neg1, dividend)) (Bigint (neg2, divisor)) =
       let _, remainder = divrem (dividend, divisor)
            in Bigint(Pos, trimzeros remainder )
 
let even number =  (car number) mod 2 = 0


let rec pow' base expt basep = 
    let counter = trimzeros (sub' expt [1] 0) in 
    if counter = [] then base 
    else if cmp base basep = base then let _,newr =
            mul' (base, [1], basep) in pow' newr counter basep  
    else let _,newr = mul' (basep, [1], base) in pow' newr counter basep

let pow (Bigint(neg1, base)) (Bigint(neg2, expt)) = 
    if neg1 = Neg || neg2= Neg then zero 
    else match (base, expt) with 
    | _ , [0] -> Bigint( Pos, [1] )
    | [], expt -> zero 
    | base, [] -> Bigint( Pos, [1] ) 
    | base, [1] -> Bigint(neg1, base) 
    | base, expt -> Bigint(neg1, trimzeros (pow' base expt base)) 



end
