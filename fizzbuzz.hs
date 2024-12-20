

fizzbuzz (a,b) xs = ([x | x <- xs, x `mod` a == 0]
                    ,[x | x <- xs, x `mod` b == 0]
                    ,[x | x <- xs, x `mod` a /=0, x `mod` b /= 0])