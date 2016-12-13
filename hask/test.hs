
data Kappa = Kappa1 | Kappa2 | Kappa3 | Kappa4 String String

data TestType = TestType {firstString :: String,
                          myMap :: Kappa -> String}

f :: Kappa -> String
f Kappa1 = "KappaOne"
f a@(Kappa4 "goop" "gaap")  = "OO"
f other  = "WAA"


-- switch :: TestType -> TestType
-- switch TestType one two = TestType two one


