module Types where 

data TicTacToe = Rows 
               { rowOne :: Row
               , rowTwo :: Row 
               , rowThree :: Row 
               } deriving (Eq, Show)

type Row = (Maybe Mark, Maybe Mark , Maybe Mark )

data Mark = X 
          | Circle 
          deriving (Eq, Show)

data Choice = First 
            | Second 
            | Third 
            deriving (Eq, Show, Enum, Ord)



