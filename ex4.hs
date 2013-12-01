--inc :: a -> IO a
inc x = do
    let y = x+1
    print y
    return y

--dec :: a -> IO a
dec x = do
    let y = x-1
    print y
    return y

loop :: a -> (a -> IO a) -> IO ()
loop x f = do
     y <- f x
     loop y f

menu :: [(String,a -> IO a)] -> a -> IO a
menu opts x = do
              putStrLn "---- Menu ----"
              mapM_ 
                putStrLn [ show i ++ ". " ++ (show(fst(opts!!(i-1)))) | i<-[1..(length opts)]]
              putStrLn "==> "
              c <- getLine
              snd (opts!!((read c)-1)) x

-- Examenvraag
-- TODO, niet gewoon minimum nemen, originele volgorde wordt niet behouden zo

oorsprong :: Ord a => [[a]] -> [a]
oorsprong [] = []
oorsprong lx = let y = minimum [ minimum a | a<-lx] 
               in [y] ++ (oorsprong (map (filter (>y)) lx))
