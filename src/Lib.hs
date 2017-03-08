-- ---
-- title: Exploring survival on the Titanic
-- subtitle: A port of a Kaggle's notebook 
-- author: Nikita Tchayka
-- 
-- ---
-- 
-- 1. Introduction
-- ===============
-- 
-- This is a direct port of the [Exploring survival on the titanic notebook](https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic) by Megan Risdal.
-- The intention of this is to have an example of what can be achieved with the data science tools we have in Haskell as for **February 27th, 2017**.

{-# LANGUAGE OverloadedStrings #-}
module Lib where
-- Be sure to fire your repl with `stack repl` and loading the
--  `OverloadedStrings` extension by issuing `:set -XOverloadedStrings` 
--  into it.

import           Data.List
import           Data.Maybe

import qualified Control.Foldl                as L
import qualified Data.Text                    as T
import qualified Data.ByteString.Lazy.Char8   as BL
import qualified Analyze.Csv                  as CSV
import qualified Analyze.RFrame               as RF
import qualified Data.Vector                  as V
import           Data.Text                    (Text)
import           Analyze.RFrame               (RFrame, RFrameUpdate)
import           Data.Vector                  (Vector)
import           Text.Regex
import           Text.Regex.Base
-- 1.1 Load and check data
-- -----------------------
-- 
-- Let's begin by loading our data

trainingSet :: IO (RFrame Text Text)
trainingSet = do
  train <- readFile "input/train.csv" >>=  loadCSV
  test  <- readFile "input/test.csv"  >>= loadCSV
  RF.appendRows (removeSurvived train) test
 where
  removeSurvived = RF.dropCols (=="Survived")
  loadCSV = CSV.decodeWithHeader . BL.pack
-- Lets check what's there, from our REPL:
-- 
-- ```
-- *Lib> ts <- trainingSet
-- *Lib> RF._rframeKeys ts
-- ["PassengerId","Pclass","Name","Sex","Age","SibSp","Parch","Ticket","Fare","Cabin","Embarked"]
-- *Lib> V.head $ RF._rframeData ts
-- ["1","3","Braund, Mr. Owen Harris","male","22","1","0","A/5 21171","7.25","","S"]
-- *Lib> V.length $ RF._rframeData ts
-- 902
-- ```
-- 
-- From here we see what we have to deal with:
-- 
-- | **Variable Name** | **Description**                  |
-- |-------------------|----------------------------------|
-- | Survived          | True or False                    | 
-- | Pclass            | Passenger's class                |
-- | name              | Passenger's name                 |
-- | sex               | Passenger's sex                  |
-- | age               | Passenger's age                  |
-- | SibSp             | Number of siblings/spouses aboard|
-- | Parch             | Number of parents/children aboard|
-- | Ticket            | Ticket number                    |
-- | Fare              | Fare                             |
-- | Cabin             | Cabin                            |
-- | Embarked          | Port of embarkation              |
-- 
-- 2. Feature Engineering
-- ======================
-- 
-- 2.1 What's in a name?
-- ---------------------
-- 
-- We can see that in the passenger name we have the *passenger title*, so we
-- can break it down into additional variables to have better predictions. Also,
-- we can break it into *surname* too to represent families.

getTitleFromName :: Text -> Text
getTitleFromName name = T.pack
                      $ subRegex titleRegex unpackedName ""
 where
  titleRegex = mkRegex "(.*, )|(\\..*)"
  unpackedName = T.unpack name
-- Now we can use this function in our REPL to extract the title:
-- 
-- ```
-- *Lib> getTitleFromName "The best, Capt. Obvious"
-- "Capt"
-- *Lib> getTitleFromName "No title here"
-- ""
-- ```
-- 
-- What if we wanted to count how many people of each title are there?
-- Well, we can construct it very easily using Haskell!
-- 
-- ```
-- countPrefixes :: Text -> Vector Text -> Int
-- countPrefixes title names = length $ filter (isInfixOf title) names
-- ```
-- 
-- But we can do better and make a synonym of the function by omitting the
-- last argument.
countPrefix :: Text -> Vector Text -> Int
countPrefix p = V.length . V.filter (T.isInfixOf p)
-- Also, let's make a function that counts how many times a title appears
-- in the names:

countedTitles :: Vector Text -> Vector (Text, Int)
countedTitles names = V.zip titles counts
  where
    counts = flip countPrefix names <$> titles
    titles = removeDupes $ getTitleFromName <$> names
    removeDupes = V.fromList . nub . V.toList
-- From our REPL, now we can run:
-- ```
-- *Lib> ts <- trainingSet
-- *Lib> countedTitles <$> RF.col "Name" ts
-- [("Mr",657),("Mrs",132),("Miss",183),("Master",40),("Don",2),("Rev",6)
-- ,("Dr",11),("Mme",1),("Ms",1),("Major",2),("Lady",1),("Sir",3),("Mlle",2)
-- ,("Col",10),("Capt",1),("the Countess",1),("Jonkheer",1)
-- ]
-- ```
--
-- Let's take the rare titles out by replacing them with "Rare Title",
-- "Mlle" and "Ms" by "Miss" and "Mme" by "Mrs"
rareTitles :: [Text]
rareTitles = [ "Dona"
             , "Lady"
             , "the Countess"
             , "Capt"
             , "Col"
             , "Don"
             , "Dr"
             , "Major"
             , "Rev"
             , "Sir"
             , "Jonkheer"
             ]

addTitleColumn :: RFrame Text Text -> IO (RFrame Text Text)
addTitleColumn namesFrame = do
  nameColumn <- RF.col "Name" namesFrame
  let newNameFrameUpdate = RF.RFrameUpdate (V.singleton "Title")
                         $ V.singleton 
                        <$> updateName
                        <$> nameColumn
  RF.fromUpdate newNameFrameUpdate
 where
  updateName n
    | getTitleFromName n `elem` rareTitles     = "Rare Title"
    | getTitleFromName n `elem` ["Mlle", "Ms"] = "Miss"
    | getTitleFromName n == "Mme"              = "Mrs"
    | otherwise                                = getTitleFromName n
