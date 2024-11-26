module GameLogic where 

import Types 

import Control.Applicative (asum) 



markRow :: Mark -> Choice -> Row -> Row 
markRow shape choice (Row (x, y, z)) = 
    case choice of 
        First -> case x of 
                  Nothing -> Row (Just shape, y , z)
                  _       -> Row (x, y, z)
        Second -> case y of 
                   Nothing -> Row (x, Just shape, z)
                   _       -> Row (x, y, z)
        Third -> case z of
                  Nothing -> Row (x, y, Just shape)
                  _       -> Row (x, y, z)


winCondTwo :: Rows -> Maybe Mark 
winCondTwo t = do 
    asum [winCondOne $ rowOne t, winCondOne $ rowTwo t, winCondOne $ rowThree t]
    where winCondOne :: Row -> Maybe Mark
          winCondOne r = 
            case r of 
             (Row (Just Circle, Just Circle, Just Circle)) -> Just Circle 
             (Row (Just X, Just X ,Just X))                -> Just X
             _                                             -> Nothing 


winCondThree :: Rows -> Maybe Mark 
winCondThree t = 
    case (rowOne t, rowTwo t, rowThree t) of 
        (Row (Just Circle,_,_), Row (Just Circle,_,_), Row (Just Circle,_,_)) ->  Just Circle 
        (Row (_,Just Circle,_), Row (_,Just Circle,_), Row (_,Just Circle,_)) ->  Just Circle 
        (Row (_,_,Just Circle), Row (_,_,Just Circle), Row (_,_,Just Circle)) ->  Just Circle 
        (Row (Just Circle,_,_), Row (_,Just Circle,_), Row (_,_,Just Circle)) ->  Just Circle 
        (Row (Just X,_,_),Row (Just X,_,_),Row (Just X,_,_))                  ->  Just X
        (Row (_,Just X,_),Row (_,Just X,_),Row (_,Just X,_))                  ->  Just X
        (Row (_,_,Just X),Row (_,_,Just X),Row (_,_,Just X))                  ->  Just X
        (Row (Just X,_,_),Row (_,Just X,_),Row (_,_,Just X))                  ->  Just X 
        _                                          ->  Nothing 

winCond :: Rows -> Maybe Mark 
winCond t = asum [winCondTwo t, winCondThree t]

markGame :: Mark -> Choice -> Choice -> Rows -> Rows 
markGame shape rowChoice spotChoice game =  
         case rowChoice of 
            First  -> game {rowOne = markRow shape spotChoice (rowOne game)}
            Second -> game {rowTwo = markRow shape spotChoice (rowTwo game)}
            Third  -> game {rowThree = markRow shape spotChoice (rowThree game)}

