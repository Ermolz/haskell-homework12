{-# OPTIONS_GHC -Wall #-}
module Yermolovych12 where

type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])

type Header = [Attribute]
type Row = [AttValue]
type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

-- Задача 1 -----------------------------------------
allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (== x) xs

-- Задача 2 -----------------------------------------
remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove _ [] = []
remove v ((k,b):xs)
  | v == k    = xs
  | otherwise = (k,b) : remove v xs

-- Задача 3 -----------------------------------------
lookUpAtt :: AttName -> Header -> Row -> AttValue
-- Передумова: Імя атрибуту присутнє в заданому заголовку.
lookUpAtt _ [] _ = error "Attribute not found in header"
lookUpAtt an ((n,_):hs) (v:vs)
  | an == n   = v
  | otherwise = lookUpAtt an hs vs
lookUpAtt _ _ _ = error "Header and row length mismatch"

-- Задача 4 -----------------------------------------
removeAtt :: AttName -> Header -> Row -> Row
removeAtt _ [] _ = []
removeAtt an ((n,_):hs) (v:vs)
  | an == n   = removeAtt an hs vs
  | otherwise = v : removeAtt an hs vs
removeAtt _ _ _ = []

-- Задача 5 -----------------------------------------
buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Передумова: Кожний рядок таблиці містить одне значення заданого атрибуту
buildFrequencyTable (attName, attVals) (hd, rows) =
  [ (val, count val) | val <- attVals ]
  where
    count v =
      length [ () | row <- rows
                  , lookUpAtt attName hd row == v ]

-- Задача 6 -----------------------------------------
nodes :: DecisionTree -> Int
nodes Null           = 0
nodes (Leaf _)       = 1
nodes (Node _ kids)  = 1 + sum [ nodes t | (_, t) <- kids ]

-- Задача 7 -----------------------------------------
evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _ = ""
evalTree (Leaf v) _ _ = v
evalTree (Node an branches) hd row =
  let val = lookUpAtt an hd row
  in go val branches
  where
    go _ [] = ""
    go v ((av, t):rest)
      | v == av   = evalTree t hd row
      | otherwise = go v rest

-- Задача 8 -----------------------------------------
partitionData :: DataSet -> Attribute -> Partition
partitionData (hd, rows) (attName, attVals) =
  [ (val, (newHeader, newRowsFor val)) | val <- attVals ]
  where
    newHeader :: Header
    newHeader = filter ((/= attName) . fst) hd

    newRowsFor :: AttValue -> [Row]
    newRowsFor val =
      [ removeAtt attName hd row
      | row <- rows
      , lookUpAtt attName hd row == val
      ]

-- Задача 9 -----------------------------------------
--
-- Задається...
-- В цьому простому випадку: атрибут, що вибирається - це перший атрибут в заголовку. 
--   Зауважимо, що кваліфікуючий атрибут присутній в заголовку,
--   тому його необхідно вилучити з можливих кандидатів. 
--
nextAtt :: AttSelector
--Передумова: Заголовок містить по крайній мірі один вхідний атрибут
nextAtt (headerDS, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) headerDS)

buildTree :: Attribute -> AttSelector -> DataSet -> DecisionTree 
buildTree qat@(className, _) attSel ds@(hd, rows)
  -- порожня множина даних -> Null
  | null rows = Null
  -- всі значення кваліфікуючого атрибуту однакові -> листок
  | allSame classValues = Leaf (head classValues)
  -- інакше ділимо за вибраним атрибутом
  | otherwise =
      let att@(attName, _) = attSel ds qat
          parts             = partitionData ds att
          children          = [ (val, buildTree qat attSel subDs)
                              | (val, subDs) <- parts
                              ]
      in Node attName children
  where
    classValues :: [AttValue]
    classValues = [ lookUpAtt className hd row | row <- rows ]

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- Це таж сама таблиця, але результат у другій колонці
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]

