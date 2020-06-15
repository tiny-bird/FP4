import Data.Typeable

type Function = Int -> Bool

checkCond :: [Int] -> Function -> Bool
checkCond l f = True

main :: IO()
main = do print  (typeOf checkCond)