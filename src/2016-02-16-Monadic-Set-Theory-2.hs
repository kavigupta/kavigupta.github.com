import Prelude hiding ((.))
import Control.Category

data Funcad a b = Funcad (a -> [b])
instance Category Funcad where
    id = Funcad return
    Funcad a . Funcad b = Funcad $ \x -> b x >>= a

