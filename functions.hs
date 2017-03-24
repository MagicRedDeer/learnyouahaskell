factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' ( x:xs ) = x + ( sum' xs )

capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a-> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, eat up!"
    | bmi <= normal = "Normal == boring!!"
    | bmi <= fat = "fatty!"
    | otherwise = "WHALE!!!"
    where bmi = height / weight ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis hs = [bmi w h | (w,h) <- hs]
    where bmi weight height = height / weight ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + topArea * 2
