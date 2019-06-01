fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact $ n - 1

main = do
  print res
  where
    res = fact 10
