bind :: IOish a -> (a -> IOish b) -> IOish b
bind m k = undefined