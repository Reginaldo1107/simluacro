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


