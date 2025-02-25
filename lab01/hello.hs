max3 a b c = max a (max b c)

times2 x = 2 * x
times2' x = ( * ) 2 x
times2'' = (* 2)
times2''' = (2 * )

myHead [] = error "Empty List"
myHead (h:_) = h

myHead2 l = if null l then error "Empty List" else x where x:_ = l

myHead3 l   | null l = error "Empty"
            | otherwise = x where x:_ = l





comI x = x

comS x y z = x z (y z)

comK x y = x

comB x y z = x (y z)