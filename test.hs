import Data.Bits (xor)
import Data.List 

type Bit             = Int
type Bits            = [Bit]
type State           = [Bit]
type StateWithInput  = (State, Bit)
type StateWithOutput = (State, Bits) 
type TrellisGraph    = [(StateWithInput, StateWithOutput)]
type Polynomial      = [Int]

-- delete the last element from the list
dl :: [a] -> [a]
dl (x:[])   = []
dl (x:xs) = x:dl xs 

-- right shift the state and add one bit
add :: Bit -> State -> State
add i xs = i:dl xs 

-- exclusive or on all the bits
xorl :: [Bit] -> Bit
xorl [x]      = x
xorl (x:y:[]) = xor x y
xorl (x:xs)   = xor x (xorl xs)

-- exclusive or on all the bits assigned by the polynomial
xorlp :: [Bit] -> Polynomial -> Bit
xorlp xs is = xorl [xs !! i | i <- is] 

-- generate all states using permutation
gs :: Int -> [State]
gs 0 = [[]]
gs n = [0:p | p <- gs (n - 1)] ++ [1:p | p <- gs (n - 1)]

-- generate the number of registers from the state of states
gsn :: Int -> Int
gsn n = gsn' n 1 

gsn' :: Int -> Int -> Int 
gsn' n x = if (2 ^ x) == n then x - 1 else gsn' n (x + 1)

-- generate all states with input
gaswi :: Int -> [StateWithInput]
gaswi n = zip (gs n) [0,0..] ++ zip (gs n) [1,1..]

-- generate all states with output
gswo :: StateWithInput -> [Polynomial] -> StateWithOutput
gswo (s, i) ps = let newState = (add i s)
                 in  (newState, [xorlp newState p | p <- ps])

-- generate the trellis graph with input of polynomials and number of registers
tl :: [Polynomial] -> Int -> TrellisGraph 
tl ps l = [(swi, gswo swi ps) | swi <- gaswi l]

-- branch metrics
bm :: TrellisGraph -> State -> State -> Bits -> Int
bm t si so i = let o = head [o' | ((si', i'), (so', o')) <- t, si' == si, so' == so]
               in  length $ i \\ o

-- path metric
pm :: TrellisGraph ->State -> [Bits] -> Int
pm t s [] = if all (\x -> x == 0) s then 0 else 99999
pm t s i  = let [a, b] = [si | ((si, i), (so, o)) <- t, so == s]
                fi     = dl i
            in minimum [pm t a fi + bm t a s (last i), 
                        pm t b fi + bm t b s (last i)]

-- encode
ec :: TrellisGraph -> Bits -> [Bits]
ec t cs = ec' t  [n-n| n <- [1..gsn $ length t]] cs

ec' :: TrellisGraph -> State -> Bits -> [Bits]
ec' t s []     = []
ec' t s (i:is) = let (sn, o) = head [swo | ((si', i'), swo) <- t, si' == s,i' == i]
                 in  o:ec' t sn is

-- decode
dc :: TrellisGraph -> [Bits] -> Bits
dc t cs = dc' t [n-n | n<-[1..gsn $ length t]] [(take n cs) | n <- [1..length cs]]

dc' :: TrellisGraph -> State -> [[Bits]] -> Bits
dc' t s []     = []
dc' t s (c:cs) = let s0         = head [(so, i) | ((si, i),(so, o)) <- t,si == s,i == 0]
                     s1         = head [(so, i) | ((si, i),(so, o)) <- t,si == s,i == 1]
                     (s',i,_)   = head $ sortBy (\(s, i, p) (s', i', p') -> compare p p') 
                                                [(s', i', pm t s' c) | (s', i') <- [s0, s1]]
                 in i : (dc' t s' cs)

printT :: TrellisGraph -> IO ()
printT []                    = return ()
printT (((si,i),(so,o)):sws) = do 
                                (putStr.show) si
                                (putStr.show) "--"
                                (putStr.show) (i,o)
                                (putStr.show) "-->"
                                (putStrLn.show) so
                                printT sws
