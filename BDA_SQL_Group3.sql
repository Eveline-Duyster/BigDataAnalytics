ALTER TABLE [tempdb].[dbo].[ESS1]
ADD wave int;

ALTER TABLE [tempdb].[dbo].[ESS2]
ADD wave int;

ALTER TABLE [tempdb].[dbo].[ESS3]
ADD wave int;

UPDATE [tempdb].[dbo].[ESS1]
SET wave=1;

UPDATE [tempdb].[dbo].[ESS2]
SET wave=2;

UPDATE [tempdb].[dbo].[ESS3]
SET wave=3;

SELECT [Country_Name]
	,[alpha3]
	,[2012]
	,[2014]
	,[2016]
INTO [tempdb].[dbo].[WorldBankClean]
FROM [tempdb].[dbo].[WorldBank]

SELECT [Country_Name]
	,WB.[alpha3]
	,[2012]
	,[2014]
	,[2016]
	,[alpha2]
INTO [tempdb].[dbo].[WorldBankAlpha]
FROM (
SELECT * 
FROM [tempdb].[dbo].[WorldBankClean]) as WB
LEFT JOIN
(SELECT [alpha2]
	,[alpha3]
FROM [tempdb].[dbo].[AlphaCodes]) as Alpha
ON WB.alpha3=Alpha.alpha3

SELECT [Country_Name]
	,[alpha3]
	,[2012] AS Unempl_Reg
	,[alpha2]
INTO [tempdb].[dbo].[WorldBank1]
FROM [tempdb].[dbo.[WorldBankAlpha]

SELECT *
INTO [tempdb].[dbo].[Data1]
FROM (
SELECT * 
FROM [tempdb].[dbo].[WorldBank1]) as WB
LEFT JOIN
(SELECT *
FROM [tempdb].[dbo].[ESS1]) as ESS
ON WB.alpha3=ESS.alpha3

SELECT [Country_Name]
	,[alpha3]
	,[2014] AS Unempl_Reg
	,[alpha2]
INTO [tempdb].[dbo].[WorldBank2]
FROM [tempdb].[dbo.[WorldBankAlpha]

SELECT *
INTO [tempdb].[dbo].[Data2]
FROM (
SELECT * 
FROM [tempdb].[dbo].[WorldBank2]) as WB
LEFT JOIN
(SELECT *
FROM [tempdb].[dbo].[ESS2]) as ESS
ON WB.alpha3=ESS.alpha3


SELECT [Country_Name]
	,[alpha3]
	,[2016] AS Unempl_Reg
	,[alpha2]
INTO [tempdb].[dbo].[WorldBank3]
FROM [tempdb].[dbo.[WorldBankAlpha]

SELECT *
INTO [tempdb].[dbo].[Data3]
FROM (
SELECT * 
FROM [tempdb].[dbo].[WorldBank3]) as WB
LEFT JOIN
(SELECT *
FROM [tempdb].[dbo].[ESS3]) as ESS
ON WB.alpha3=ESS.alpha3

SELECT *
INTO [tempdb].[dbo].[Data]
FROM (
SELECT *
FROM [tempdb].[dbo].[Data1]
UNION ALL
SELECT *
FROM [tempdb].[dbo].[Data2]
UNION ALL
SELECT *
FROM [tempdb].[dbo].[Data3]);