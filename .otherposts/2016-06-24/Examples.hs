module Examples(

    ) where

import OnErrorResumeNext

safeReturn :: IO Int
newValue = onErrorResumeNext $ do
    x <- safe basicErrorExample
    return 2

basicErrorExample :: IO Int
basicErrorExample = do
    error "Not going to work!"