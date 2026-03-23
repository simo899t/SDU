type IOish a = IO a

bind :: IOish a -> (a -> IOish b) -> IOish b
bind m k = undefined
main :: IO ()
main = return ()
