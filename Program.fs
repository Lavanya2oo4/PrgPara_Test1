// For more information see https://aka.ms/fsharp-console-apps
// Test Lavanya  Student Id: 22077323

// SECTION 1 


let salList=[75000;48000;120000;190000;300113;92000;36000]

// Filter through the list to find high-income salaries. For this list, salaries above $100,000 are considered high.

let filteredSalaries=List.filter (fun sal -> sal>100000) salList

printfn "Salary that are above 100000 are: %A" filteredSalaries


// Use the map function to calculate tax for all salaries based on the table provided.


let getTax sal =
    if sal <= 49020.0 then float sal * 0.15
    elif sal <= 98040.0 then (float sal - 49020.0) * 0.205 + (0.15 * 49020.0)
    elif sal <= 151978.0 then (float sal - 98040.0) * 0.26 + (0.15 * 49020.0) + ((98040.0 - 49020.0) * 0.205)
    elif sal <= 216511.0 then (float sal - 151978.0) * 0.29 + (0.15 * 49020.0) + ((98040.0 - 49020.0) * 0.205) + ((151978.0 - 98040.0) * 0.26)
    else (float sal - 216511.0) * 0.33 + (0.15 * 49020.0) + ((98040.0 - 49020.0) * 0.205) + ((151978.0 - 98040.0) * 0.26) + ((216511.0 - 151978.0) * 0.29)

let salListFloat = [75000.0; 48000.0; 120000.0; 190000.0; 300113.0; 92000.0; 36000.0]

let taxList = List.map getTax salListFloat

printfn "Taxes for each salary: %A" taxList


// Filter salaries less than $49,020 and add $20,000 to these salaries using the map function.

let addToSal sal=
     if sal < 49020 then sal + 20000
     else sal

let addedSal=List.map addToSal salList

printfn "Salaries after addition : %A" addedSal



// Filter salaries between $50,000 and $100,000 and sum them all using the reduce/fold function. 


let filteredSum =
    salList
    |> List.filter (fun sal -> sal >= 50000 && sal <= 100000)
    |> List.reduce (+)

printfn "Sum of salaries between $50,000 and $100,000: %d" filteredSum


// TAIL RECURSION


// Use tail recursion to write a program that will calculate the sum of all multiples of 3 up to a given number. Assume that we pass only a multiple of 3 as a parameter to this function. No validation is needed.
// Example: If the parameter is 27, then the result of the function should be: 3+6+9+12+15+18+21+24+27 = 

let sumMultiple num =
    let rec calculate currentMultiple sum =
        if currentMultiple > num then sum
        else calculate (currentMultiple + 3) (sum + currentMultiple)
    calculate 3 0

let total = sumMultiple 31
printfn "Total of multiples: %d" total
   