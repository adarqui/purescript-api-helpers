module Test.Main where

import Prelude (Unit, const, pure, unit, bind, void, (<$>), (<<<), ($))
import Control.Monad.Aff
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception
-- import Control.Monad.Eff.Console (CONSOLE, log)
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax.Response (class Respondable)
import Purescript.Api.Helpers
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Trans.Class



-- rd :: forall a b c. Affable a b => ReaderT ApiOptions (Aff a) c -> b c
rd = fromAff <<< rD

-- main :: forall e. Eff (err :: EXCEPTION, console :: CONSOLE | e) Unit
{-
main :: forall t35 t36 t43.
  ( Respondable t36
  , Respondable t35
  ) => Eff
         ( err :: EXCEPTION
         , console :: CONSOLE
         , ajax :: AJAX
         | t43
         )
         (Canceler
            ( err :: EXCEPTION
            , console :: CONSOLE
            , ajax :: AJAX
            | t43
            )
         )
-}
-- main :: forall e. Eff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  void $ runAff throwException (const (pure unit)) do
    log "You should add some tests => Ok."
    -- void $ rd (handleError <$> getAt ([] :: Array Boolean) ["me"])
    rd (handleError <$> getAt ([] :: Array Boolean) ["me"])
  pure unit
