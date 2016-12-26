
import Data.Char

testMaybe :: Int -> Maybe Int
testMaybe 1 = 1
testMaybe _ = Nothing


data Gomp = Goop
            | Geebo {
              unk :: String
              , monk :: String
              }



data Kappa = Kappa1 | Kappa2 | Kappa3 | Kappa4 String String deriving (Show, Eq)

data Catpa = Cat Kappa | Magma  deriving (Show)

m = Magma

e :: Catpa
e = Cat Kappa2

h :: Catpa -> String
h = show


data Keepo = B Kappa | C String deriving (Show, Eq)

data TestType = TestType {firstString :: String,
                          myMap :: Kappa -> String}


bar :: TestType -> String
bar TestType {firstString = "ga"} = "OOG"
bar other = "BAA"


f :: Kappa -> String
f Kappa1 = "KappaOne"
f a@(Kappa4 "goop" "gaap")  = "OO"
f other  = "WAA"

foo :: String -> String
foo "gop" = "gep"
foo "gep" = "baa"


-- switch :: TestType -> TestType
-- switch TestType one two = TestType two one


