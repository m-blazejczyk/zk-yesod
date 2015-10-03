{-# LANGUAGE OverloadedStrings #-}

import Prelude
import qualified Data.Text as T

data XEdVal = XEdNone | XEdValOne T.Text | XEdValArr [T.Text] | XEdValMap [(T.Text, T.Text)] deriving Show

main :: IO ()
main = do
    print $ getXEdValues [("pk", "aaa"), ("value", "V"), ("value", "2"), ("value[a]", "A"), ("value[]", "[]")]

getXEdValues :: [(T.Text, T.Text)] -> XEdVal
getXEdValues pp = foldr process XEdNone pp
    where
        process ("value", pv) XEdNone = XEdValOne pv
        process ("value[]", pv) XEdNone = XEdValArr [pv]
        process (pn, pv) XEdNone | T.isPrefixOf "value[" pn && T.isSuffixOf "]" pn = XEdValMap [(T.drop 6 pn, pv)]
                                 | otherwise = XEdNone
        process _ vOne@(XEdValOne v) = vOne
        process (pn, pv) (XEdValArr vArr) = undefined
        process (pn, pv) (XEdValMap vMap) = undefined
