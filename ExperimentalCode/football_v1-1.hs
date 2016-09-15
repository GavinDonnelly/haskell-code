-----------------------------------------------------------------------
 --
 -- Filename: football-v1_1.hs
 --
 -- Synopsis: Scripting a Football Table System
 --
 -- In football (or soccer) leagues like Premier League, Liga BBVA etc.
 -- they are run through a points system where each team gets points for 
 -- each game they play, 3 for win, 1 for draw and nothing for a lose. At 
 -- the end of the season the team with the most points wins. Quite a simple
 -- system so perfect for Haskell practise.
 -- 
 -- Author: Gavin Donnelly
 --
 -- Version: See VERSION below
 --
 -----------------------------------------------------------------------
 
 ----------------------------REVISION HISTORY:--------------------------
 -- 
 -- v1.O  12/09/2013: Initial version. 
 -- v1.1  14/09/2013: Added the requirement for a signed 'promise'
 --       declaring that the submission is all your own work. Added the 
 --       calculation methods.
-------------------------------------------------------------------------

type Team = String
type HTeam = Team
type ATeam = Team
type Goals = Int
type Points = Int
type Match = (HTeam,Goals,ATeam,Goals)
type Table = [(Team,Points)]

-- Takes Match and uses filter to pass through the list and find the team by 
-- checking if the team equals the home or away team and display its matches

teamMatches :: [Match] -> Team -> [Match]
teamMatches ms t = filter isTeam ms
					where isTeam (hT,gH,aT,gT) = (hT == t) || (aT == t)

-- Points takes match and finds what team won lost or draw on goals and displays 
-- the team with its points 

points :: Match -> ((Team,Points),(Team,Points))
points (hT,gh,aT,ga)
	| gh > ga = ((hT,3),(aT,0))
	| gh < ga = ((hT,0),(aT,3))
	| gh == ga = ((hT,1),(aT,1))

winner :: Match -> String
winner (h,gh,a,ga)
		| gh > ga = h
		| gh < ga = a
		| gh == ga = "match was a draw"

-- Extra Function, that takes a match and splits it into home team, team and point or 
-- away team, team and points depending on character 

pair :: ((Team,Points),(Team,Points)) -> Char -> (Team,Points)
pair (hT,aT) c = if(c == 'h') 
				 then hT
				 else aT	

-- Maps the list to just display the home results with the help of the extra function pair

homeresults :: [Match] -> [(Team,Points)]
homeresults ms = map resPair ms
				where resPair (hT,gH,aT,gT) = pair (points (hT,gH,aT,gT)) 'h'
				
-- Maps the list to just display the away results with the help of the extra function pair

awayresults :: [Match] -> [(Team,Points)]
awayresults ms = map resPair ms
				where resPair (hT,gH,aT,gT) = pair (points (hT,gH,aT,gT)) 'a'

-- Adds both the results together to diaplay all results

results :: [Match] -> [(Team,Points)]
results ms = homeresults ms  ++ awayresults ms

-- maps the points that the matchs with the same team name as the one entered into a list 
-- this list is then summed to get total points

teamPoints :: [Match] -> Team -> Int
teamPoints ms t = sum(map teamP (results ms))  
					where teamP (team,p)  = if (t == team) 
											  then p
											  else 0

-- maps ever team in league and its total points (using teamPoints) into a list

mkTable :: [Match] -> [(Team,Points)]
mkTable ma = map team league
			where team t = (t,teamPoints ma t)
						



-------------------------------------------------------------------------
------    WORKER FUNCTIONS TO DO MAIPULATION AND PRESENTING -------------
-------------------------------------------------------------------------

sortTable :: Table -> Table
sortTable [] = []
sortTable ((t,p):ts) = sortTable more ++ [(t,p)] ++ sortTable less
                      where less = [(u,v) | (u,v) <- ts, v < p]
                            more = [(u,v) | (u,v) <- ts, v >= p]
   

showTable :: [Match] -> IO()
showTable = putStr . unlines . (map (\(x,y) -> x ++ "\t" ++ show y)) . sortTable . mkTable



------------------------------------------------------------------------
-----------   TEST DATA   ----------------------------------------------
------------------------------------------------------------------------


league = ["Newcastle", "Leicester", "Sunderland","Manchester Utd"]

scores :: [Match]
scores = [("Newcastle", 8,"Leicester", 0),
          ("Leicester", 1,"Newcastle", 1),
          ("Newcastle", 2,"Sunderland",0),
          ("Sunderland",2,"Newcastle", 4),
          ("Newcastle", 1,"Manchester Utd", 0),
          ("Manchester Utd", 0,"Newcastle", 0),
          ("Leicester", 3,"Sunderland",2),
          ("Sunderland",1,"Leicester", 4),
          ("Leicester", 2,"Manchester Utd", 0),
          ("Manchester Utd", 1,"Leicester", 2),
          ("Sunderland",2,"Manchester Utd", 3),
          ("Manchester Utd", 2,"Sunderland",0)]

homescores :: [(Team,Points)]
homescores = [("Newcastle",3),
              ("Leicester",1),
              ("Newcastle",3),
              ("Sunderland",0),
              ("Newcastle",3),
              ("Manchester Utd",1),
              ("Leicester",3),
              ("Sunderland",0),
              ("Leicester",3),
              ("Manchester Utd",0),
              ("Sunderland",0),
              ("Manchester Utd",3)]

resultScores :: [(Team,Points)]
resultScores = [("Newcastle",3),("Leicester",1),("Newcastle",3),("Sunderland",0),
                ("Newcastle",3),("Manchester Utd",1),("Leicester",3),("Sunderland",0),
                ("Leicester",3),("Manchester Utd",0),("Sunderland",0),("Manchester Utd",3),
                ("Leicester",0),("Newcastle",1),("Sunderland",0),("Newcastle",3),
                ("Manchester Utd",0),("Newcastle",1),("Sunderland",0),("Leicester",3),
                ("Manchester Utd",0),("Leicester",3),("Manchester Utd",3),("Sunderland",0)