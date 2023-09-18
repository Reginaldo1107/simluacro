--problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool { 
--requiere: {True}
--asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}


relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True 
relacionesValidas ((x,y): xs)| x==y = False
                             | pertenece (x,y) xs = False
                             | otherwise = relacionesValidas xs

pertenece :: (String,String) -> [(String, String)] -> Bool
pertenece (x,y) [] = False
pertenece (x,y) (z : zs) | (esIgual (x,y) z) = True
                          | otherwise = pertenece (x,y) zs

esIgual :: (String,String) -> (String,String) -> Bool 
esIgual (x,y) (u,c)| x==u && y==c = True
                    | x==c && y==u = True
                    | otherwise = False 

-- problema personas (relaciones: seq⟨String × String⟩) : seq⟨String⟩ {
--requiere: {relacionesV alidas(relaciones)}
--asegura: {resu tiene exactamente los elementos que figuran en alguna tupla de relaciones en cualquiera de las dos
--posiciones, sin repetir}

personas :: [(String,String)] -> [String]
personas [] = []
personas ((x,y):xs) = sacarRepetidos (x : y : personas xs)

sacarRepetidos :: [String] -> [String]
sacarRepetidos [] = []
sacarRepetidos [x] = [x]
sacarRepetidos (x:xs)| incluido x xs = sacarRepetidos xs
                      | otherwise = x : sacarRepetidos xs

incluido :: String -> [String] -> Bool
incluido x (y:ys)| x==y = True
                 | otherwise = incluido x ys 

-- problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
-- requiere: {relacionesValidas(relaciones)}
--asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool { 
--requiere: {True}
--asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
-- *NI TUPLAS CON AMBAS COMPONENTES IGUALES

-- [("123","123")] ---> False  , YALA 

-- *NI TUPLAS REPETIDAS 
-- [("123","456") , ("123","456") ]  --> False 

--tuplasIguales :: [(String ,String)] ->Bool
--tuplasIguales ((x,y): (xs))


relacionesValidas :: [(String ,String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((a,b) :xs)   | (a == b) = False 
                                | perteneceGeneral (a,b) xs = False 
                                | perteneceGeneral (b,a) xs = False
                                |otherwise = relacionesValidas xs

-- pertenece a la lista osea no son iguales
perteneceGeneral :: (Eq t) => t -> [t] ->Bool
perteneceGeneral x [] = False
perteneceGeneral x (y:ys)   | x == y = True 
                            |otherwise = perteneceGeneral x (ys)

-- *En iguales hayn 3 casos : 
-- *Cuando esta vacia la lista 
-- *Cuando solo hay un elemento 
-- *Cuando la lista esta llena 
perteneceInt :: Integer -> [Integer] -> Bool
perteneceInt x  []  = False -- Cuando la lista es vacia 

perteneceInt x (y:[]) | x == y  = True 
                 |otherwise = False

perteneceInt x (y:xs) | x == y  = True 
                 |otherwise = perteneceInt x (xs)




elementosEnComunLista :: [Integer] -> Bool
elementosEnComunLista [] = False
elementosEnComunLista (x:[]) = False
elementosEnComunLista (x:xs)  |perteneceGeneral x (xs) = True
                               |otherwise = elementosEnComunLista(xs)
    

--problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--requiere: {relacionesValidas(relaciones)}
-- asegura: {resu tiene exactamente los elementos que figuran en alguna 
--tupla de relaciones en cualquiera de las dos
--posiciones, sin repetir}
--}
personas :: [(String ,String)] -> [String]


--Tengo una lista repetida tengo que devolver una lista sin repetir de enteros 
listaDeEnteroSinRepetir :: [Integer]-> [Integer]
 

