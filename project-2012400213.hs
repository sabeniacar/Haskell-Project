-- compiling: yes
-- complete: yes
-- selim abenyakar

module Main where
import Data.List


-- table list
--
-- returns true if list is a table
--
-- example:
-- >table [("name", "id", "gpa"), ("aslı", "1", "2.2"), ("ayse", "2", "1.7")]
-- =>true

table:: [(String,String,String)] ->  Bool
table [] = False
table list = True


-- createtable fields
--
-- creates an empty table whose fields are given in fields tuple
--
-- example:
-- >let students = createtable ("name", "id", "gpa")
-- > students
-- =>[("name", "id", "gpa")]

createtable :: (String,String,String) -> [(String,String,String)]
createtable fields = (fields:[])


-- get table row fields
-- 
-- returns value of field in given row
--
-- example:
-- assuming students= [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > get students ("ali", "1", "3.7") "gpa"
-- => "3.7"
--
-- cor head row field
--
-- takes fields tuple of table returns value of field in given row
-- 
-- example:
-- > cor ("name","id","gpa") ("mehmet", "1", "1.2") "gpa"
-- => "1.2"


get :: [(String,String,String)] -> (String,String,String) -> String -> String
get t r f= cor (head t) r f
 where 
  cor (x,y,z) (a,b,c) s
   | (x == s) = a
   | (y == s) = b
   | (z == s) = c

   
-- alter table row fieldsvalues
--
-- alters values given in fieldsvalues in given row
-- match field name of fieldsvalues with fields tuple of table, call recursively with same table,updated row and remaining fieldsvalues
--
-- example:
-- assuming students= [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > alter students ("ali","1","3.7") [("name", "selim"), ("gpa", "3.1")]
-- => ("selim", "1", "3.1")
   
   
alter :: [(String,String,String)] -> (String,String,String) -> [(String,String)] -> (String,String,String)
alter table row [] = row
alter ((a,b,c):t) (x,y,z) ((m,n):ls) 
   | (m == a) = alter ((a,b,c):t) (n,y,z) ls
   | (m == b) = alter ((a,b,c):t) (x,n,z) ls
   | (m == c) = alter ((a,b,c):t) (x,y,n) ls
   
   
-- addrow table row
-- 
-- adds a row to beginning of table   
--
-- example:
-- assuming students = [("name", "id", "gpa")]

-- > addrow students ("asli", "3", "2.8")
-- > students
-- => [("name", "id", "gpa"), ("asli", "3", "2.8")] 


addrow :: [(String,String,String)] -> (String,String,String) -> [(String,String,String)]
addrow (h:t) row = (h:row:t)
 

-- addrows table rows
--
-- adds many rows to a table 
--
-- example:
-- assuming students = [("name", "id", "gpa")]
--
-- > addrows students [("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > students
-- => [("name","id","gpa"),("ali","3","1.2"),("ayse","2","3.7"),("ali","1","3.7")]



addrows :: [(String,String,String)] -> [(String,String,String)] -> [(String,String,String)]
addrows table rows = foldl(\acc x -> addrow acc x) table rows 


-- deleterows table field-value
--
-- deletes rows which has given field equal to a certain value
-- foldl and find rows to be deleted, then foldl and filter those rows to be deleted
--
-- example:
-- assuming students = [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > deleterows students [("name", "ali"),("gpa", "3.7")] 
-- => [("name", "id", "gpa"),("ali","3","1.2"),("ayse","2","3.7")]
--
-- find list field-value
--
-- return rows in list which have field equal to value
--
-- example:
-- assuming students= [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > find students ("gpa","3.7")
-- => ["ali","1","3.7),("ayse","2","3.7")]

   
deleterows :: [(String,String,String)] -> [(String,String)] -> [(String,String,String)]
deleterows table [] = table   
deleterows ((a,b,c):t) m = foldl(\acc (x,y,z)->[(r,y,p)|(r,y,p) <- acc , (x,y,z)/=(r,y,p)]) ((a,b,c):t) (foldl(\acc x -> find ((a,b,c):acc) x) t m)
  where 
   find ((q,w,e):list) (k,l)
    | (k == q) = [(x,y,z) | (x,y,z) <- list , x == l]
    | (k == w) = [(x,y,z) | (x,y,z) <- list , y == l]
    | (k == e) = [(x,y,z) | (x,y,z) <- list , z == l]  
   

-- updaterows table fields-values
--
-- changes the values of second field of rows whose field equals to the first specified field
-- since first field and second field can take 3 different field names there are 9 combinations in total
--
-- example:
-- assuming students = [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > updaterows students [("name", "ali"), ("gpa", "3.3")]
-- => [("name","id","gpa"),("ali","3","3.3"),("ali","1","3.3"),("ayse","2","3.7")]


updaterows:: [(String,String,String)] -> [(String,String)] -> [(String,String,String)]  
updaterows ((a,b,c):t) [(m,n),(k,l)] 
   | (m == a) && (k == a) = (a,b,c):([(l,y,z) | (x,y,z) <-t , x == n] ++ [(x,y,z) | (x,y,z) <-t , x /= n])
   | (m == a) && (k == b) = (a,b,c):([(x,l,z) | (x,y,z) <-t , x == n] ++ [(x,y,z) | (x,y,z) <-t , x /= n])
   | (m == a) && (k == c) = (a,b,c):([(x,y,l) | (x,y,z) <-t , x == n] ++ [(x,y,z) | (x,y,z) <-t , x /= n])
   | (m == b) && (k == a) = (a,b,c):([(l,y,z) | (x,y,z) <-t , y == n] ++ [(x,y,z) | (x,y,z) <-t , y /= n])
   | (m == b) && (k == b) = (a,b,c):([(x,l,z) | (x,y,z) <-t , y == n] ++ [(x,y,z) | (x,y,z) <-t , y /= n])
   | (m == b) && (k == c) = (a,b,c):([(x,y,l) | (x,y,z) <-t , y == n] ++ [(x,y,z) | (x,y,z) <-t , y /= n])
   | (m == c) && (k == a) = (a,b,c):([(l,y,z) | (x,y,z) <-t , z == n] ++ [(x,y,z) | (x,y,z) <-t , z /= n])
   | (m == c) && (k == b) = (a,b,c):([(x,l,z) | (x,y,z) <-t , z == n] ++ [(x,y,z) | (x,y,z) <-t , z /= n])
   | (m == c) && (k == c) = (a,b,c):([(x,y,l) | (x,y,z) <-t , z == n] ++ [(x,y,z) | (x,y,z) <-t , z /= n])

   
-- selectrows table fields field-value
--
-- returns only fields given in fields for the given field value from table  
-- find rows that have given field value by list comprehension,use get function to get values of fields
-- 
-- example:
-- assuming students = [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > selectrows students ("gpa", "id") ("name", "ali")
-- => [("gpa", "id"),("1.2", "3"),("3.7", "1")]
   
   
selectrows:: [(String,String,String)] ->  (String,String) -> (String,String) -> [(String,String)]   
selectrows ((a,b,c):t) (k,l) (m,n)
   | (m == a) = (k,l):(map (\f -> ((get ((a,b,c):t) f k),(get ((a,b,c):t) f l))) [(x,y,z) | (x,y,z) <-t ,x == n])
   | (m == b) = (k,l):(map (\f -> ((get ((a,b,c):t) f k),(get ((a,b,c):t) f l))) [(x,y,z) | (x,y,z) <-t ,y == n])
   | (m == c) = (k,l):(map (\f -> ((get ((a,b,c):t) f k),(get ((a,b,c):t) f l))) [(x,y,z) | (x,y,z) <-t ,z == n])
   
   
-- allrows table field
--
-- returns field value of all rows of table   
--
-- example:
-- assuming students = [("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
-- > allrows students "gpa"
-- => [("gpa"), ("1.2"), ("3.7"),("3.7")]


allrows:: [(String,String,String)] -> String -> [(String)]
allrows ((a,b,c):t) f 
   | (f == a) = f:[ x | (x,y,z) <- t]
   | (f == b) = f:[ y | (x,y,z) <- t]
   | (f == c) = f:[ z | (x,y,z) <- t]
   
   
-- sortby table field
--
-- sorts the table by a given field in ascending order  
-- 
-- examples:
-- assuming students = [("name","id","gpa"),("ali","3","1.2"),("ayse","2","3.7"),("ali","1","3.7")]
--
-- >sortby students "name"
-- =>[("name","id","gpa"),("ali","1","3.7"),("ali","3","1.2"),("ayse","2","3.7")]
--
-- > sortby students "id"
-- =>[("name","id","gpa"),("ali","1","3.7"),("ayse","2","3.7"),("ali","3","1.2")]
--
-- > sortby students "gpa"
-- =>[("name","id","gpa"),("ali","3","1.2"),("ali","1","3.7"),("ayse","2","3.7")]

   
sortby:: [(String,String,String)] -> String -> [(String,String,String)]
sortby ((a,b,c):t) n
   | (n == a) = ((a,b,c):sort t)
   | (n == b) = ((a,b,c):[(j,h,k) |(h,j,k) <- (sort [(y,x,z) | (x,y,z) <- t ])])
   | (n == c) = ((a,b,c):[(k,j,h) |(h,j,k) <- (sort [(z,y,x) | (x,y,z) <- t ])])
   
   
-- remdup list
--
-- removes duplicates in sorted list   
--
-- example:
-- > remdup [1 ,1 ,3 ,4 ,7 ,7 ,8]
-- => [1 ,3 ,4 ,7 ,8]
   
remdup:: [(String,String,String)] -> [(String,String,String)]
remdup []=[]
remdup (h:[]) = (h:[])
remdup (h:(hh:t)) = if h == hh then remdup (hh:t) else (h:(remdup (hh:t)))  


-- distinct
--
-- removes any duplicate rows in a table
--
-- example:
-- assuming students = [("name","id","gpa"),("ali","3","1.2"),("ayse","2","3.7"),("ali","1","3.7"),("ayse","2","3.7")]
-- > distinct students
-- => [("name","id","gpa"),("ali","1","3.7"),("ali","3","1.2"),("ayse","2","3.7")]
   
distinct:: [(String,String,String)] -> [(String,String,String)]
distinct ((a,b,c):t) = remdup(sortby ((a,b,c):t) a)   






   