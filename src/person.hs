module Person
( Person(..)
, getName
, getFirstName
, getLastName
) where

data Person = Person String String deriving (Show)

getName :: Person -> String
getName (Person first last) = first ++ " " ++ last

getFirstName :: Person -> String
getFirstName (Person first last) = first

getLastName :: Person -> String
getLastName (Person first last) = last
