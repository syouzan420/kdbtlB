
module Mydata(State(..), Mana(..), Ply(..), Enm(..), Bul(..), Mes, Eai(..) ,T(..), Ta(..)
             , Bu(..), Dr(..), Ki(..), Fun, toMana, initstate, (.>), minX, maxX, maxY, youM
             , addKi, timKi, subKi, kiToList, isKiLow, newKi) where

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
data Ply = Ply {pki :: !Ki, pmki :: !Ki, prt :: !Int, pmrt :: !Int, py :: !Int, px :: !Int, pw :: !Int
               ,pdx :: !Int, ing :: !Bool, look :: ![(Int,Int)], ltc :: !Int} deriving (Eq, Show)
data Enm = Enm {ena :: !String, eki :: !Ki, emki :: !Ki, ert :: !Int, emrt :: !Int
               ,ey :: !Int, ex :: !Int, ew :: !Int, edx :: !Int, eai :: !Eai} deriving (Eq, Show)
data Bul = Bul {bt :: !Bu,bs :: !Int,by :: !Int,bx :: !Int,bdy :: !Int,bdx :: !Int
               ,bmt :: !Int,btc :: !Int} deriving (Eq, Show)

data Ki = Ki Int Int Int Int Int deriving (Eq, Show)

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

type Fun = Int -> [T] -> [T] -> State -> (State,Ki)

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

minX, maxX, maxY :: Int
minX = -10; maxX = 10; maxY = 10

initstate :: State 
initstate = State player [enemy0,enemy1] [] "" [] 

pki0, eki0, eki1 :: Ki
pki0 = Ki 7 7 7 7 7; eki0 = Ki 4 4 4 5 5; eki1 = Ki 5 5 4 4 4

player :: Ply
player = Ply{pki=pki0, pmki=pki0, prt=10, pmrt=10, py=0, px=0, pw=1, pdx=0, ing=False, look=[], ltc=0}

enemy0 :: Enm
enemy0 = Enm{ena="douchou", eki=eki0, emki=eki0, ert=12, emrt=12, ey=10, ex=0, ew=2, edx=0, eai=eai0}

enemy1 :: Enm
enemy1 = Enm{ena="kyakkan", eki=eki1, emki=eki1, ert=10, emrt=10, ey=8, ex=3, ew=1, edx=0, eai=eai1}

eai0 :: Eai
eai0 = Eai{plp=Nothing, atm=5, atn=0, tic=0, dam=10, dan=0, ipr=makeProb 80 10 10, spr=makeProb 10 10 80}

eai1 :: Eai
eai1 = Eai{plp=Nothing, atm=4, atn=0, tic=0, dam=20, dan=0, ipr=makeProb 90 5 5, spr=makeProb 5 5 90}

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

addKi :: Ki -> Ki -> Ki
addKi (Ki k00 k01 k02 k03 k04) (Ki k10 k11 k12 k13 k14) =
                      Ki (k00+k10) (k01+k11) (k02+k12) (k03+k13) (k04+k14)

timKi :: Int -> Ki -> Ki
timKi k (Ki k0 k1 k2 k3 k4) = Ki (k*k0) (k*k1) (k*k2) (k*k3) (k*k4)

subKi :: Ki -> Ki -> Ki
subKi ka kb = addKi ka (timKi (-1) kb)

kiToList :: Ki -> [Int]
kiToList (Ki k0 k1 k2 k3 k4) = [k0,k1,k2,k3,k4]

listToKi :: [Int] -> Ki
listToKi ls = Ki (ls!!0) (ls!!1) (ls!!2) (ls!!3) (ls!!4)

isKiLow :: Ki -> Bool
isKiLow (Ki k0 k1 k2 k3 k4) = k0<1 || k1<1 || k2<1 || k3<1 || k4<1

sftList :: [a] -> [a] 
sftList [] = []
sftList [x] = [x]
sftList (x:xs) = xs ++ [x]

addList :: Num a => [a] -> [a] -> [a]
addList [] _ = []
addList _ [] = []
addList (x:xs) (y:ys) = (x+y):(addList xs ys)

subList :: Num a => [a] -> [a] -> [a]
subList l1 l2 = addList l1 (map ((-1)*) l2)

dealKi :: [Int] -> [Int] -> Ki
dealKi kl mkl = listToKi$tryDealKi 5 kl mkl

addMizu :: [Int] -> [Int]
addMizu kl = (take 4 kl) ++ [(last kl)+1]

tryDealKi :: Int -> [Int] -> [Int] -> [Int]
tryDealKi 0 kl mkl = let diff = subList mkl kl 
                         tlst = map (\x -> if(x<0) then (-x) else 0) diff
                      in subList kl tlst
tryDealKi i kl mkl = 
  let zlst = [0,0,0,0,0]
      diff = subList mkl kl
      tlst = map (\x -> if(x<0) then (-x) else 0) diff
      kl' = subList kl tlst
   in if (tlst==zlst) then kl else tryDealKi (i-1) (addList kl' (sftList tlst)) mkl

newKi :: Ki -> Ki -> Ki
newKi k mk = dealKi (map (\x -> let nx = nki x klst in if(nx<0) then 0 else nx) [0..4]) mklst 
  where klst = addMizu (kiToList k)
        mklst = kiToList mk
        dec :: Int -> Int
        dec 0 = 4
        dec n = n-1
        nki :: Int -> [Int] -> Int
        nki i kl = let tk = kl!!i
                       ptk = kl!!(dec i)
                       d1 = ptk - (kl!!(dec$dec i))
                       d2 = tk - ptk 
                       tk' = if(d1>0) then tk+1 else tk
                    in if(d2>0) then tk'-1 else tk'
-----

