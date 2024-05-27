import Data.Char (digitToInt)

-- Función para calcular la suma de los divisores propios de un número
aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..n `div` 2], n `mod` x == 0]

-- Función para clasificar un número como abundante, perfecto o deficiente 
classifyNumber :: Int -> String
classifyNumber n
    | aliquot < n = "deficient"
    | aliquot == n = "perfect"
    | otherwise = "abundant"
    where aliquot = aliquotSum n

-- Función para extraer características del carnet
extractCharacteristics :: Int -> String
extractCharacteristics carnet = 
    let ano = "20" ++ take 2 (show carnet)
        semestre = take 1 (drop 2 (show carnet))
        programa = if (digitToInt (show carnet !! 3)) == 0
            then case digitToInt (show carnet !! 4) of
                1 -> "Humanities"
                2 -> "Administrative"  
                _ -> "Engineering"  
            else case digitToInt (head semestre) of
                1 -> "Engineering"
                2 -> "Humanities"
                _ -> "Administrative"
        num = read (take 3 (drop 5 (show carnet))) :: Int
        -- Eliminar ceros a la izquierda en los últimos tres dígitos
        consecutivo = dropWhile (=='0') $ take 3 (drop 5 (show carnet))
        tipoNum = if even num then "even" else "odd"
        classification = classifyNumber num
    in ano ++ "-" ++ semestre ++ " " ++ programa ++ " num" ++ consecutivo ++ " " ++ tipoNum ++ " " 


main :: IO ()
main = do
    entrada <- getLine
    let carnet = read entrada :: Int
    putStrLn $ extractCharacteristics carnet
    main
