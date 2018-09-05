-- Simple game as an example of Prob.hs use
-- The game is to flip a coin. If it is a heads then you get to roll a 6 sided die
-- This is done 5 times with the sum of the rolls being your score
-- 19 or more is a win!
-- Need Prob.hs 
import Prob

-- Nothing -> 0 , value otherwise
game' :: Maybe Die -> Int
game' (Just (Face n)) = n
game' Nothing = 0

-- List of flip and roll events and accumulate the value of the dice rolls
game :: [(Coin, Maybe Die)] -> Int
game = foldl (\acc (x,y) -> acc + game' y) 0

-- Game is won if the roll is greater than 18
winGame :: [(Coin, Maybe Die)] -> Bool
winGame = (>18) . game

-- String to print
winResult :: [(Coin,Maybe Die)] -> String
winResult = (\x -> if x then "You won!!!" else "You lost!!!") . winGame

main = do
    let flipNRoll = joinProb $ (\x -> (\y -> if x == Tails then (x,Nothing) else (x,Just y))) <$> coin <*> die
    putStrLn "Here's the game: "
    putStrLn "You flip a coin, if it's a heads you can roll a die! You do this 5 times! A score of 19 or better and you win!"
    events <- getEvents_ flipNRoll 5
    putStrLn "Here's what happened: "
    print events
    putStr "Your total was "
    putStrLn $ show $ game events
    putStrLn $ winResult events
