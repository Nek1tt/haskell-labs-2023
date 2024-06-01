import SQLHSSugar
import DBReader

-- CATEGORY:     WARE,    CLASS
-- MANUFACTURER: BILL_ID, COMPANY
-- MATERIAL:     BILL_ID, WARE,   AMOUNT
-- PRODUCT:      BILL_ID, WARE,   AMOUNT, PRICE

main = readDB' defaultDBName >>= executeSomeQueries

executeSomeQueries :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
executeSomeQueries (categories, manufacturers, materials, products) = do
  test "task 1" task5part1
  test "task 2" task5part2
  
  where
    test msg p = do
      putStrLn $ "===== execute " ++ msg ++ " ====="
      -- putStrLn . debugTable $ p & enumerate
      printResult $ p & enumerate
    
    task5part1 = 
      -- materials NL_JOIN PRODUCT ON m.BILL_ID=p.BILL_ID
      materials // "m" `njoin` products // "p" `on` "m.BILL_ID" `jeq` "p.BILL_ID"
      -- -> NL_JOIN CATEGORY ON m.WARE=c.WARE
      `njoin` categories // "c" `on` "m.WARE" `jeq` "c.WARE"
      -- -> FILTER c.CLASS='Mineral'
      `wher` col "CLASS" `eq` str "Mineral"
      -- -> SORT_BY p.WARE
      `orderby` ["p.WARE":asc]
      -- -> MAP (p.WARE)
      `select` ["p.WARE"]
      -- -> DISTINCT
      & distinct

    task5part2 =
      -- materials NL_JOIN PRODUCT ON m.BILL_ID=p.BILL_ID
      materials // "m" `njoin` products // "p" `on` "m.BILL_ID" `jeq` "p.BILL_ID"
      -- -> NL_JOIN CATEGORY ON c.WARE=p.WARE
      `njoin` categories // "cat1" `on` "p.WARE" `jeq` "cat1.WARE"
      `njoin` categories // "cat2" `on` "m.WARE" `jeq` "cat2.WARE"
      -- -> FILTER c.CLASS='Raw food'
      `wher` col "cat1.CLASS" `eq` str "Stuff"
      `wher` col "cat2.CLASS" `eq` str "Mineral"
      -- -> SORT_BY p.BILL_ID
      `orderby` ["p.BILL_ID":asc]
      -- -> MAP (p.BILL_ID, m.WARE, p.WARE)
      `select` ["p.BILL_ID", "m.WARE", "p.WARE"]
      -- -> DISTINCT
      & distinct
      -- -> TAKE 50
      & limit 0 50
      
    