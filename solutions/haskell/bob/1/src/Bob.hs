module Bob (responseFor) where
import Data.Char (isSpace, isLowerCase, isAlpha)


stringUpperCase :: String -> Bool
stringUpperCase "" = True
stringUpperCase str
  | isLowerCase (head str) = False
  | otherwise = stringUpperCase (drop 1 str)

responseFor :: String -> String
responseFor xs
  | lastLetter == '?' && isCapitalized = "Calm down, I know what I'm doing!"
  | lastLetter == '?' = "Sure."
  | isCapitalized = "Whoa, chill out!"
  | whiteSpace = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    trimmed = reverse . dropWhile isSpace . reverse $ xs
    lastLetter = if null trimmed then ' ' else last trimmed
    isCapitalized = not (null (filter isAlpha xs)) && stringUpperCase xs
    whiteSpace = all isSpace xs
  
