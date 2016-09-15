-----------------------------------------------------------------------
 --
 -- Filename: bank_v1-1.hs
 --
 -- Synopsis: Bank System
 --
 -- Just a basic bank system in Haskell inspired by Question from past 
 -- exam for Univerity of Nottingham, Function Programing course, Autumn 
 -- Semester 2006 -2007
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  21/09/2013: Initial version. 
 -- v1.1  21/09/2013: Bank system implemented and tested, looks good.
 --
 ----------------------------TO DO:-------------------------------------
 -- 
 -- v1.1 13/09/2016: Fix bankFree function, needs work and adding to Git
 --
------------------------------------------------------------------------

-- Bank details of customers
type NI = Int -- User National Insurace (Basically a uniquie number to define them)
type Age = Int -- User Age
type Balance = Int -- User Balance
type Person = (NI, Age, Balance)
type Bank = [Person] -- Bank, so list of users or customers
type Market = [Bank] -- Market, so list of banks
type Pop = [NI]


-- retired, if the person is older that 65, they are retired
retired :: Person -> Bool
retired (n,a,b) = a>=65

-- deposit, add money to a persons balance
deposit :: Person -> Int -> Person
deposit (n,a,b) x = (n,a,b+x)

-- credit, returns the list of people who are not overdrawn, where overdrawn
-- is a balance of <0.
credit :: Bank -> [Person]
credit b = filter overdrawn b
			where overdrawn (n,a,b)  =  b>=0

-- equityAge
equityAge :: Bank -> (Int,Int) -> Int
equityAge b (x,y) = sumDep (filter totalDep b) 
					where totalDep (n,a,b) | x<y = a >= x && a<=y
										   | x>=y = a <= x && a>=y
										   | x == y = a==x
										  										   
-- sum deposit from a bank for a person.									   
sumDep :: Bank -> Int
sumDep b = sum (map getBal b)
		where getBal (n,a,b) = b		
			
-- creditNI, search through each bank, in market, for person with NI n, and sum 
-- their deposits 		
creditNI :: NI -> Market ->	Int
creditNI  ni m = sumDepBank(map checkBank m)
				where checkBank b = filter checkNI b;
					  checkNI (n,a,b) =  n == ni
				
-- sum deposits from all banks for a person.
sumDepBank :: Market -> Int
sumDepBank m = sum(map getBankBal m )
			where getBankBal b = sumDep b;
			
-- bankFree, for each NI in pop, loop through each bank in market to check 
-- if its not there					  
{-bankFree :: Pop -> Market -> Pop
bankFree p m = 	map check m
				where check b = filter checkNI b;
					checkNI (n,a,b) = n != 
					
checkInPop :: Pop -> Int -> Int
checkInPop p i = head(filter check p)
					where check n = n == i

check :: Person -> Person
check (n,a,b) = (n,a,b)-}

------------------------------------------------------------------------
------ Main ------------------------------------------------------------
------------------------------------------------------------------------
main = do
	putStrLn "\nBank System\n"
	putStrLn "TEST DATA"
	putStr "gavin1: "
	print gavin1
	putStr "gavin2: "
	print gavin2
	putStr "bob: "
	print bob
	putStrLn "john: "
	print john
	putStr "james: "
	print james
	putStr "rbs: "
	print rbs
	putStr "hsbc: "
	print hsbc
	putStr "market: "
	print market
	putStr "pop: "
	print pop
	putStrLn ""
	putStrLn "OUPUTS"
	putStrLn "Input: retired john"
	putStr "Output: "
	print $ retired john
	putStrLn "Input: retired bob"
	putStr "Output: "
	print $ retired bob
	putStrLn "Input: deposit james 20"
	putStr "Output: "
	print $ deposit james 20
	putStrLn "Input: credit hsbc"
	putStr "Output: "
	print $ credit hsbc
	putStrLn "Input: equityAge hsbc"
	putStr "Output: "
	print $ equityAge hsbc (20,28)
	putStrLn "Input: creditNI hsbc"
	putStr "Output: "
	print $ creditNI 1 market
	putStrLn ""
	putStrLn "End Output, Author: Gavin Donnelly"

					  
------------------------------------------------------------------------
-----------   TEST DATA   ----------------------------------------------
------------------------------------------------------------------------			

gavin1 :: Person
gavin1 = (1,20,300)

gavin2 :: Person
gavin2 = (1,20,720)

bob :: Person
bob = (2,66,-80)

john :: Person
john = (3,26,980)

james :: Person
james = (8,26,180)

rbs :: Bank
rbs = [gavin1,bob]

hsbc :: Bank
hsbc = [john,gavin2,james,bob]

market :: Market
market = [rbs,hsbc]

pop :: Pop
pop = [1,2,3,8]