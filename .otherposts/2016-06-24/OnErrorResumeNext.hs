module OnErrorResumeNext(
        OERN, safe, onErrorResumeNext
    ) where

import Control.Exception

newtype OERN a = OERN {onErrorResumeNext :: IO a}

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

instance Functor OERN where
    fmap f val = do
        x <- val
        return $ f x

instance Applicative OERN where
    pure = return
    f <*> x = do
        f' <- f
        x' <- x
        return $ f' x'

instance Monad OERN where
    return = OERN . return
    OERN a >>= f = OERN $ do
        val <- catchAny a (\x -> return undefined)
        case f val of
            OERN x -> x

safe :: IO a -> OERN a
safe = OERN