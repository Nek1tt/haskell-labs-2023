-- 3.1
SELECT DISTINCT WARE, MIN(price) AS min_price, MAX(price) AS max_price FROM `PRODUCT`
GROUP BY WARE
ORDER BY WARE;

-- 3.2
SELECT DISTINCT WARE, MIN(price) AS min_price, MAX(price) AS max_price, MAX(price) - MIN(price) AS DIFF FROM `PRODUCT`
GROUP BY WARE
ORDER BY max_price DESC
LIMIT 3;

-- 3.3
SELECT COMPANY, COUNT(DISTINCT PRODUCT.WARE) AS numProd
FROM `MANUFACTURER`
INNER JOIN PRODUCT
ON PRODUCT.BILL_ID = MANUFACTURER.BILL_ID
GROUP BY COMPANY
ORDER BY numProd DESC
LIMIT 3;

-- 3.4
SELECT MAX(PRICE) AS max_price, CATEGORY.CLASS AS class FROM PRODUCT
INNER JOIN CATEGORY 
ON PRODUCT.WARE = CATEGORY.WARE
GROUP BY CATEGORY.CLASS
ORDER BY CATEGORY.CLASS;

-- 3.5

-- 3.6
SELECT COMPANY, COUNT(DISTINCT PRODUCT.WARE) AS prodWare, COUNT(DISTINCT MATERIAL.WARE) AS matWare
FROM `MANUFACTURER`
LEFT JOIN PRODUCT
ON PRODUCT.BILL_ID = MANUFACTURER.BILL_ID
LEFT JOIN MATERIAL
ON MATERIAL.BILL_ID = MANUFACTURER.BILL_ID
GROUP BY COMPANY 
HAVING prodWare > matWare;

-- 3.7
SELECT DISTINCT MANUFACTURER.COMPANY
FROM `MANUFACTURER`
INNER JOIN PRODUCT
ON PRODUCT.BILL_ID = MANUFACTURER.BILL_ID
GROUP BY MANUFACTURER.COMPANY, PRODUCT.WARE
HAVING COUNT(DISTINCT MANUFACTURER.BILL_ID) > 2
ORDER BY MANUFACTURER.COMPANY;

-- 3.8
SELECT DISTINCT MANUFACTURER.COMPANY
FROM `MANUFACTURER`
INNER JOIN PRODUCT
ON PRODUCT.BILL_ID = MANUFACTURER.BILL_ID
INNER JOIN CATEGORY
ON CATEGORY.WARE = PRODUCT.WARE
WHERE CATEGORY.CLASS IN ('Fuel', 'Food', 'Mineral')
GROUP BY MANUFACTURER.COMPANY
HAVING COUNT(DISTINCT CATEGORY.CLASS) = 3;