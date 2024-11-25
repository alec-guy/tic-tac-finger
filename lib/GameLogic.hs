module GameLogic where 

import Types 

import Control.Applicative (asum) 



markRow :: Mark -> Choice -> Row -> Row 
markRow shape choice (x, y, z) = 
    case choice of 
        First -> case x of 
                  Nothing -> (Just shape, y , z)
                  _       -> (x, y, z)
        Second -> case y of 
                   Nothing -> (x, Just shape, z)
                   _       -> (x, y, z)
        Third -> case z of
                  Nothing -> (x, y, Just shape)
                  _       -> (x, y, z)


winCondTwo :: TicTacToe -> Maybe Mark 
winCondTwo t = do 
    asum [winCondOne $ rowOne t, winCondOne $ rowTwo t, winCondOne $ rowThree t]
    where winCondOne :: Row -> Maybe Mark
          winCondOne r = 
            case r of 
             (Just Circle, Just Circle, Just Circle) -> Just Circle 
             (Just X, Just X ,Just X)                -> Just X
             _                                       -> Nothing 


winCondThree :: TicTacToe -> Maybe Mark 
winCondThree t = 
    case (rowOne t, rowTwo t, rowThree t) of 
        ((Just Circle,_,_), (Just Circle,_,_), (Just Circle,_,_)) ->  Just Circle 
        ((_,Just Circle,_), (_,Just Circle,_), (_,Just Circle,_)) ->  Just Circle 
        ((_,_,Just Circle), (_,_,Just Circle), (_,_,Just Circle)) ->  Just Circle 
        ((Just Circle,_,_), (_,Just Circle,_), (_,_,Just Circle)) ->  Just Circle 
        ((Just X,_,_),(Just X,_,_),(Just X,_,_))                  ->  Just X
        ((_,Just X,_),(_,Just X,_),(_,Just X,_))                  ->  Just X
        ((_,_,Just X),(_,_,Just X),(_,_,Just X))                  ->  Just X
        ((Just X,_,_),(_,Just X,_),(_,_,Just X))                  ->  Just X 
        _                                          ->  Nothing 

winCond :: TicTacToe -> Maybe Mark 
winCond t = asum [winCondTwo t, winCondThree t]

markGame :: Mark -> Choice -> Choice -> TicTacToe -> TicTacToe 
markGame shape rowChoice spotChoice game =  
         case rowChoice of 
            First  -> game {rowOne = markRow shape spotChoice (rowOne game)}
            Second -> game {rowTwo = markRow shape spotChoice (rowTwo game)}
            Third  -> game {rowThree = markRow shape spotChoice (rowThree game)}

