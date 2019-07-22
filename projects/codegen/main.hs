data Type = Type
  { typeName :: String }

data Var = Var
  { varName :: String ,
    varType :: Type }

data CNode = Empty | CNode String [CNode] deriving Show

main = putStrLn (CNode "++")
