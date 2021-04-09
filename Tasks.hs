{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

module Tasks where

import Dataset
import Text.Printf
import Data.List
type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
-- Operatia de impartire dintre 2 int
divide :: Integer -> Integer -> Float
divide a b = (fromIntegral a) / (fromIntegral b)

-- Operatia de impartire dintre float si int
divideFloat :: Float -> Integer -> Float
divideFloat a b = a / (fromIntegral b)

-- Auxiliar pentru stringtoint
readInt :: String -> Integer
readInt "" = 0
readInt x = read x

-- Cast de la string la int
stringtoint :: [String] -> [Integer]
stringtoint = map readInt

-- Cast de la String la float
stringtofloat :: String -> Float
stringtofloat "" = 0
stringtofloat x = read x

-- Functia pentru creearea unui rand
get_comprimed_row :: Row -> Row
get_comprimed_row [] = []
get_comprimed_row (r:rs) = r : [(printf "%.2f" ((divide (sum (stringtoint (drop 0 (take 6 rs)))) 4) + (stringtofloat (last rs))))]

-- Functia care itereaza prin randuri
get_completed_rows :: Table -> Table
get_completed_rows [] = []
get_completed_rows (t:ts) = get_comprimed_row t : get_completed_rows ts

-- Functia care creeaza tabelul
compute_exam_grades :: Table -> Table
compute_exam_grades (x:xs) = ["Nume", "Punctaj Exam"] : get_completed_rows xs

-- Task 2
-- Number of students who have passed the exam:
-- Testam daca nota unui student > 2.5
checkGrade :: Row -> Int
checkGrade [] = 0
checkGrade ["Nume", "Punctaj Exam"] = 0
checkGrade h = (if ((stringtofloat (last h)) > 2.5) then 1 else 0)

-- Iteram prin toti studentii
iterateRowsGrade :: Table -> Int
iterateRowsGrade [] = 0
iterateRowsGrade (r:rs) = checkGrade r + iterateRowsGrade rs

-- Numaram studentii care au nota de trecere
get_passed_students_num :: Table -> Int
get_passed_students_num [] = 0
get_passed_students_num table = iterateRowsGrade(compute_exam_grades table)

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage t = (divide (toInteger (get_passed_students_num t)) (toInteger(length t - 1)))

-- Sum of grades
sumOfGradesAux :: Row -> Float
sumOfGradesAux [] = 0
sumOfGradesAux ["Nume", "Punctaj Exam"] = 0
sumOfGradesAux r = stringtofloat (last r)

--
sumOfGradex :: Table -> Float
sumOfGradex [] = 0
sumOfGradex (h:t) = sumOfGradesAux h + sumOfGradex t

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg t = divideFloat (sumOfGradex (compute_exam_grades t)) (toInteger(length t - 1))

-- Auxiliar pentru stringtoint
readFloat :: String -> Float
readFloat "" = 0
readFloat x = read x

-- Cast de la string la int
stringtofloatlist :: [String] -> [Float]
stringtofloatlist = map readFloat

-- Check Homework
hw_check :: Row -> Int
hw_check [] = 0
hw_check r = (if (sum (stringtofloatlist (drop 2 (take 5 r)))) >= 1.5 then 1 else 0)

-- Iterate Rows Homeworks
hw_rows_iterate :: Table -> Int
hw_rows_iterate [] = 0
hw_rows_iterate (h:t) = hw_check h + hw_rows_iterate t

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num (h:t) = hw_rows_iterate t

----
--rowSum :: Row -> Row -> Row
--rowSum a b = printf "%.2f" (zipWith (+) (stringtofloatlist a) (stringtofloatlist b))
--
----
--qs_aux_average :: Row -> Row
--qs_aux_average r = (drop 2 (take 5 r))
--
----
--qs_average :: Table -> Row -> Row
--qs_average (h:t) = rowSum (qs_aux_average h) (qs_average t)

dividelistbyvalue :: Integer -> [String] -> Row
dividelistbyvalue a [] = []
dividelistbyvalue a (x:xs) = printf "%.2f"(divideFloat (stringtofloat x) a) : dividelistbyvalue a xs

-- Sum of 2 strings
sumOfTwoStrings :: String -> String -> String
sumOfTwoStrings a b = printf "%.2f"((stringtofloat a) + (stringtofloat b))

format_qw_row :: Row -> Row
format_qw_row r = (drop 1 (take 7 r))

-- Format qs table
format_qs_table :: Table -> Table
format_qs_table [] = []
format_qs_table (h:t) = format_qw_row h : format_qs_table t

--
sumOfRow_qs :: Table -> Row
sumOfRow_qs t = (foldr (zipWith sumOfTwoStrings) ["0","0","0","0","0","0"] (format_qs_table t))

-- Task 3
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs (h:t) = ["Q1","Q2","Q3","Q4","Q5","Q6"] : [dividelistbyvalue (toInteger (length t)) (sumOfRow_qs t)]

return_count :: Integer -> Row -> Int
return_count a l = length (filter (==a) (stringtoint l))

create_summary :: Table -> Table
create_summary [] = []
create_summary list = map(\curr -> [head curr, show (return_count 0 (tail curr)), show (return_count 1 (tail curr)), show (return_count 2 (tail curr))]) list

-- Task 4
get_exam_summary :: Table -> Table
get_exam_summary l = ["Q", "0", "1", "2"] : create_summary (drop 1 (take 7 (transpose l)))

compare_aux l1 l2 = 
    if (stringtofloat(last l1) < stringtofloat(last l2)) then LT 
    else if (stringtofloat(last l1) == stringtofloat(last l2)) && ((head l1) < (head l2)) then LT
    else GT

-- Task 5
get_ranking :: Table -> Table
get_ranking l = ["Nume", "Punctaj Exam"] : sortBy compare_aux (tail (compute_exam_grades l))

addzero_tostring :: String -> String
addzero_tostring string = 
    if ((length string) == 3) then (string ++ "0")
    else string

calculate_diff :: Table -> Table
calculate_diff = map (\list -> [head list, printf "%.2f" (sum(stringtofloatlist (drop 1 (take 7 list))) / 4), addzero_tostring (last list), printf "%.2f" (abs ((sum(stringtofloatlist (drop 1 (take 7 list))) / 4) - (stringtofloat (last list))))])

-- Task 6
get_exam_diff_table :: Table -> Table
get_exam_diff_table list = ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"] : (sortBy compare_aux (calculate_diff (drop 1 list)))
