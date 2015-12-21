import Data.List

data Character = Character { hp :: Int
                           , attack :: Int
                           , ac :: Int
                           } deriving (Show)

data Result
  = Win
  | Loss
  deriving (Eq, Show)

fight boss sc 
  | hp boss <= 0 = Win
  | hp sc <= 0 = Loss
  | otherwise = fight (hit boss sc) (hit sc boss)

hit tank dd = tank { hp = hp tank - damage }
  where
    damage = maximum [attack dd - ac tank, 1]

weapons = [(8,4,0),(10,5,0),(25,6,0),(40,7,0),(74,8,0)]
armor = [(13,0,1),(31,0,2),(53,0,3),(75,0,4),(102,0,5)]
rings = [(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3)]
twoRings = rings ++ (map addRings $ filter (\a -> length a == 2) $ subsequences rings)
addRings [(a,b,c),(d,e,f)] = (a+d,b+e,c+f)
items = sortBy (\(a,_,_) (b,_,_) -> compare a b) $
    [addItems w (addItems a r) | w <- weapons, a <- armor, r <- twoRings]
    ++ [addItems w r | w <- weapons, r <- twoRings]
    ++ [addItems w a | w <- weapons, a <- armor]
    ++ weapons
addItems (a,b,c) (d,e,f) = (a+d,b+e,c+f)

wield (_,a,d) char = char { attack = a + attack char, ac = d + ac char }

bruteForce :: Character -> Int
bruteForce boss = bruteForce' boss items
  where
    bruteForce' boss (i@(p,a,d):is) =
      if fight boss (wield i (Character 100 0 0)) == Win
        then p
        else bruteForce' boss is

bruteLoose :: Character -> Int
bruteLoose boss = bruteLoose' boss (reverse items)
  where
    bruteLoose' boss (i@(p,a,d):is) =
      if fight boss (wield i (Character 100 0 0)) == Loss
        then p
        else bruteLoose' boss is
  

main = do
  print $ bruteForce (Character 109 8 2)
  print $ bruteLoose (Character 109 8 2)
