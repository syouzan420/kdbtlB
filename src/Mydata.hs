
module Mydata(State(..), Mana(..), Ply(..), Enm(..), Bul(..), Mes, Eai(..)
             ,T(..), Ta(..), Bu(..), Dr(..), Fun, toMana, initstate, (.>), maxY, youM) where

import qualified Data.Map.Strict as M
import Data.List (findIndex, isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isDigit)


data Mana = Mana T Y

data T = T Na Ta deriving (Eq, Show)

type Y = [T] -> T -> [T] 

type Na = String
type Mes = String

data Ta = Kaz Int
        | Zyo Char
        | Hou [(Dr,Int)]
        | Tam [(Bu,Int)]
        | Dou [String] [String] [T] [T]
        deriving (Eq, Show)

data State = State {pl  :: !Ply
                   ,ens :: ![Enm]
                   ,tms  :: ![Bul]
                   ,mes :: !Mes
                   ,mns :: ![Mana]
                   } deriving (Eq, Show)

-- ki:genki, mki: max genki, rt: recover time, mrt: max recover time
data Ply = Ply {pki :: !Int, pmki :: !Int, prt :: !Int, pmrt :: !Int
               ,py :: !Int, px :: !Int, pw :: !Int, pdx :: !Int, ing :: !Bool} deriving (Eq, Show)
data Enm = Enm {ena :: !String, eki :: !Int, emki :: !Int, ert :: !Int, emrt :: !Int
               ,ey :: !Int, ex :: !Int, ew :: !Int, edx :: !Int, eai :: !Eai} deriving (Eq, Show)
data Bul = Bul {bt :: !Bu,bs :: !Int,by :: !Int,bx :: !Int,bdy :: !Int,bdx :: !Int
               ,bmt :: !Int,btc :: !Int} deriving (Eq, Show)

-- plp: player position (y,x,w)
-- atm: max action time
-- atn: action time number
-- tic: elasped time after recognizing the player
-- dam: max danger level
-- dan: danger level (knowing the player shot bullets and feel like moving)
-- ipr: initial probability (for choosing acts)
-- spr: probabilty after seeing
data Eai = Eai {plp :: !(Maybe (Int,Int,Int)), atm :: !Int, atn :: !Int
               ,tic :: !Int, dam :: !Int, dan :: !Int
               ,ipr :: !(M.Map String Int), spr :: !(M.Map String Int)}
                                                                      deriving (Eq, Show)

type Fun = Int -> [T] -> [T] -> State -> (State,Int)

data Bu = Ho | Mi deriving (Eq, Show)           -- Tama Type Hodama, Mizutama
data Dr = Mg | Hd | Ue | Si deriving (Eq, Show) -- Direction Migi, Hidari, Ue, Sita

instance Eq Mana where
  (==) (Mana t1 _) (Mana t2 _) = t1==t2

instance Show Mana where
  show (Mana t _) = show t

(.>) :: [Mana] -> Mana -> [Mana]
(.>) [] mn = [mn]
(.>) ms (Mana t y) = makeManas (y (getTs ms) t) y

getTs :: [Mana] -> [T]
getTs ms = map (\(Mana t' _) -> t') ms

makeManas :: [T] -> Y -> [Mana]
makeManas ts y = map (\t -> Mana t y) ts

kazElem :: M.Map String Int
kazElem = M.fromList [("hi",1),("fu",2),("mi",3),("yo",4),("yi",5)
                     ,("mu",6),("na",7),("ya",8),("ko",9),("so",10)]

toKaz :: String -> Maybe Int
toKaz str = if (isInfixOf "so" str) then
                let sps = splitOn "so" str
                    mbs = map (istoKaz . (++ "so")) (init sps)
                    mbs' = if(last sps=="") then mbs else mbs++[istoKaz (last sps)]
                 in foldl (flip (<*>)) (Just 0) $ ((+) <$>) <$> mbs' 
                                    else if (isDigits str) then Just ((read str)::Int)
                                                           else istoKaz str

isDigits :: String -> Bool
isDigits [] = True
isDigits (x:xs) = (isDigit x) && (isDigits xs)

istoKaz :: String -> Maybe Int
istoKaz [] = Nothing
istoKaz [_] = Nothing
istoKaz (a:b:xs) = let res = M.lookup (a:b:[]) kazElem 
                    in case res of
                         Just 10 -> (+) <$> res <*> (if(xs==[]) then Just 0 else istoKaz xs)
                         Just _  -> (*) <$> res <*> (if(xs==[]) then Just 1 else istoKaz xs)
                         Nothing -> Nothing

manas :: M.Map String Ta 
manas = M.fromList [("to",Zyo 't'),("ga",Zyo 'g'),("de",Zyo 'd')
                   ,("hodama",Tam [(Ho,1)]),("mizutama",Tam [(Mi,1)])
                   ,("migi",Hou [(Mg,1)]),("hidari",Hou [(Hd,1)])
                   ,("nageru",Dou ["Tam"] ["Hou","Kaz"] [] [])
                   ,("ugoku",Dou ["Hou"] ["Kaz"] [] [])
                   ,("miru",Dou [] ["Hou"] [] [])]

toMana :: String -> Maybe Mana
toMana str = let ta = case toKaz str of
                        Just i  -> Just (Kaz i)
                        Nothing -> M.lookup str manas
              in (\t -> (Mana (T str t) youM)) <$> ta 

maxY :: Int
maxY = 10

initstate :: State 
initstate = State player [enemy] [] "" [] 

player :: Ply
player = Ply{pki=50, pmki=50, prt=10, pmrt=10, py=0, px=0, pw=1, pdx=0, ing=False}

enemy :: Enm
enemy = Enm{ena="douchou", eki=20, emki=20, ert=15, emrt=15, ey=10, ex=0, ew=2, edx=0, eai=eai0}

eai0 :: Eai
eai0 = Eai{plp=Nothing, atm=5, atn=0, tic=0, dam=10, dan=0, ipr=makeProb 80 10 10, spr=makeProb 10 10 80}

makeProb :: Int -> Int -> Int -> M.Map String Int
makeProb a b c = M.fromList [("miru",a),("ugoku",b),("nageru",c)]

youM :: Y
youM [] _ = []
youM ts@((T _ ta1):_) t@(T _ ta2)
  | toConstr ta1 == toConstr ta2 = ts .+. t
  | otherwise                    = ts .*. t

toConstr :: Ta -> String
toConstr = head . words . show

(.+.) :: [T] -> T -> [T]
(.+.) ((T n1 (Kaz a)):ts) (T n2 (Kaz b)) = (T (n1++"+"++n2) (Kaz (a+b))):ts
(.+.) ((T n1 (Hou la)):ts) (T n2 (Hou lb)) = (T (n1++"+"++n2) (Hou (la++lb))):ts
(.+.) ((T n1 (Tam la)):ts) (T n2 (Tam lb)) = (T (n1++"+"++n2) (Tam (la++lb))):ts
(.+.) (t1@(T _ (Dou _ _ _ _)):ts) t2@(T _ (Dou _ _ _ _)) = t2:t1:ts 
(.+.) ts t = t:ts

(.*.) :: [T] -> T -> [T]
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Kaz b)) = (T (n1++"*"++n2) (Kaz (a*b))):ts
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Hou ls)) = (T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Kaz a)):ts) (T n2 (Tam ls)) = (T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Hou ls)):ts) (T n2 (Kaz a)) = (T (n1++"*"++n2) (Hou (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ((T n1 (Tam ls)):ts) (T n2 (Kaz a)) = (T (n1++"*"++n2) (Tam (map (\(s,n)-> (s,n*a)) ls))):ts
(.*.) ts t@(T _ (Dou _ _ _ _)) = makeDou t ts
(.*.) ((T _ (Zyo ch)):ts) t = applZyo ch t ts 
(.*.) (t:ts) (T _ (Zyo 'd')) = uniSameTai t ts
(.*.) ts t = t:ts

applZyo :: Char -> T -> [T] -> [T]
applZyo 't' t ts = t:ts
applZyo 'g' t ts = ts .*. t
applZyo _ t ts = t:ts

uniSameTai :: T -> [T] -> [T]
uniSameTai t [] = [t]
uniSameTai t@(T _ ta1) ts@((T _ ta2):_) 
  | toConstr ta1 == toConstr ta2 = case ts .+. t of (nt:nts) -> uniSameTai nt nts; [] -> []
  | otherwise = t:ts

makeDou :: T -> [T] -> [T]
makeDou  d [] = [d] 
makeDou d@(T na (Dou ca cb t1 t2)) tal@(t3@(T nat ta):ts) 
  | elem cstr ca = let ca' = eraseFrom cstr ca
                    in makeDou (T (na++" "++nat) (Dou ca' cb (t3:t1) t2)) ts
  | elem cstr cb = let cb' = eraseFrom cstr cb
                    in makeDou (T (na++" "++nat) (Dou ca cb' t1 (t3:t2))) ts
  | otherwise = d:tal 
    where cstr = toConstr ta
makeDou _ ts = ts

eraseFrom :: Eq a => a -> [a] -> [a]
eraseFrom t ls = let ind = findIndex (== t) ls
                  in case ind of
                       Nothing -> ls
                       Just i  -> take i ls ++ drop (i+1) ls

-----

