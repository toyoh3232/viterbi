import Data.Bits
import Data.List

type State = [Int]
type StateWithInput = (State,Int)
type StateWithOutput = (State,[Int]) 
type TrellisGraph = [(StateWithInput,StateWithOutput)]
type Polynomial = [Int]

dl :: [a] -> [a]
dl (x:[])   = []
dl (x:xs) = x:dl xs 

add :: Int -> State -> State
add i xs = i:dl xs 

xorl :: [Int] -> Int
xorl [x]      = x
xorl (x:y:[]) = xor x y
xorl (x:xs)   = xor x (xorl xs)

xorlp :: [Int] -> Polynomial -> Int
xorlp xs is = xorl [xs !! i | i <- is] 

gs :: Int -> [[Int]]
gs 0 = [[]]
gs n = [0:p | p <- gs (n - 1)] ++ [1:p | p <- gs (n - 1)]

gsn :: Int -> Int
gsn n = gsn' n 1 
    
gsn' :: Int -> Int -> Int 
gsn' n x = if (2 ^ x) == n then x - 1 else gsn' n (x+1)
gaswi :: Int -> [StateWithInput]
gaswi n = zip (gs n) [0,0..] ++ zip (gs n) [1,1..]

gswo :: StateWithInput -> [Polynomial] -> StateWithOutput
gswo (s, i) ps = let newState = (add i s)
                 in  (newState, [xorlp newState p | p <- ps])

-- input: -Polynomial for each ouput
--        -number of register 
tl :: [Polynomial] -> Int -> TrellisGraph
tl ps l = [(swi, gswo swi ps) | swi <- gaswi l]

-- input: - Trellis Graph
--        -           
bm :: TrellisGraph -> State -> State -> [Int] -> Int
bm t si so i = let o = head [o_ |((si_,i_),(so_,o_)) <- t,si_==si,so_==so]
               in  length $ i \\ o

pm :: TrellisGraph ->State -> [[Int]] -> Int
pm t s [] = if all (\x -> x==0) s then 0 else 9999
pm t s i = let [a,b] =[si |((si,i),(so,o)) <- t,so==s]
               fi = dl i
           in minimum [pm t a fi + bm t a s (last i), 
                       pm t b fi + bm t b s (last i)]

ec :: TrellisGraph -> [Int] -> [[Int]]
ec t cs = ec' t  [n-n| n<-[1..gsn $ length t]] cs

ec' :: TrellisGraph -> State -> [Int] -> [[Int]]
ec' t s []     = []  
ec' t s (i:is) = let (sn,o) =head [swo|((si_,i_),swo) <- t,si_==s,i_== i]
                 in  o:ec' t sn is

dc :: TrellisGraph -> [[Int]] -> [Int]
dc t cs = dc' t [n-n| n<-[1..gsn $ length t]] [(take n cs) | n <- [1..length cs]]

dc' :: TrellisGraph -> State -> [[[Int]]] -> [Int]
dc' t s []     = []
dc' t s (c:cs) = let s0         = head [(so,i) | ((si,i),(so,o)) <- t,si == s,i==0]
                     s1         = head [(so,i) | ((si,i),(so,o)) <- t,si == s,i==1]
                     (sn,i,_) = head $ sortBy (\(s1,i1,p1) (s2,i2,p2) -> compare p1 p2) [(s_, i, pm t s_ c) | (s_,i) <- [s0,s1]]
                  in i : (dc' t sn cs)

printT :: TrellisGraph -> IO ()
printT (((si,i),(so,o)):sws) = do 
                                (putStr.show) si
                                (putStr.show) "--"
                                (putStr.show) (i,o)
                                (putStr.show) "-->"
                                (putStrLn.show) so
                                printT sws
printT []                    = return ()