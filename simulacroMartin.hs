-- 1)
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((x, y) : xs) | x == y = False
                                | pertenece (x, y) xs = False
                                | pertenece (y, x) xs = False
                                | otherwise = relacionesValidas xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y : ys) | x == y = True
                     | otherwise = pertenece x ys

-- 2)
personas :: [(String, String)] -> [String]
personas [] = []
personas x = eliminarRepetidos (personasAux x)

personasAux :: [(String, String)] -> [String]
personasAux [] = []
personasAux ((x, y) : xs) = [x, y] ++ personasAux xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x : xs) = (x : eliminarRepetidos (quitarTodos x xs))

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y : xs) | x == y = quitarTodos x xs
                       | otherwise = (y: (quitarTodos x xs))

-- 3)
amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe p ((x, y) : xs) | p == x = (y : (amigosDe p xs))
                         | p == y = (x : (amigosDe p xs))
                         | otherwise = amigosDe p xs

-- 4)
personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [(x, y)] = x
personaConMasAmigos ((x, y) : xs) | longitud (amigosDe x ((x, y) : xs)) >= longitud (amigosDe y ((x, y) : xs)) && longitud (amigosDe x ((x, y) : xs)) >= longitud (personaConMasAmigos xs) = x
                                  | longitud (amigosDe x ((x, y) : xs)) < longitud (amigosDe y ((x, y) : xs)) && longitud (amigosDe y ((x, y) : xs)) >= longitud (personaConMasAmigos xs) = y
                                  | otherwise = personaConMasAmigos xs

longitud :: [t] -> Integer
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs






