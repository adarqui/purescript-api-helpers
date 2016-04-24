module Network.Api.Helpers (
  ApiOptions (..),
  ApiEff,
  class By,
  by,
  defaultApiOptions,
  route,
  flattenParams,
  mkQueryString,
  routeQueryBy,
  runDebug,
  urlFromReader,
  getAt,
  postAt,
  putAt,
  deleteAt
) where



import Control.Monad.Aff            (Aff)
import Control.Monad.Aff.Class      (liftAff)
import Control.Monad.Aff.Console    (log)
import Control.Monad.Eff.Console    (CONSOLE())
import Control.Monad.Reader.Trans   (ReaderT, lift, ask)
import Data.Array                   (cons)
import Data.Either                  (Either(Left, Right))
import Data.Foreign                 (ForeignError)
import Data.Maybe                   (maybe)
import Data.String                  (joinWith, stripSuffix)
import Data.Tuple                   (Tuple(Tuple))
import Network.HTTP.Affjax          (AJAX)
import Network.HTTP.Affjax          as AJ
import Network.HTTP.Affjax.Request  (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable, fromResponse)
import Prelude                      (class Show, Unit, return, ($), bind, show, (++), (<>), unit, map, id)



data ApiOptions = ApiOptions {
  apiUrl     :: String,
  apiPrefix  :: String,
  apiDebug   :: Boolean
}



type ApiEff a = forall eff. ReaderT ApiOptions (Aff (ajax :: AJAX, console :: CONSOLE | eff)) a



class Show a <= By a where
  by :: a -> String



defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions {apiUrl: "//", apiPrefix: "api", apiDebug: true}



route :: String -> Array String -> String
route url paths = joinWith "/" (url `cons` paths)



flattenParams :: Array (Tuple String String) -> Array String
flattenParams [] = []
flattenParams params = map (\(Tuple k v) -> k <> "=" <> v) params



mkQueryString :: Array String -> String
mkQueryString [] = ""
mkQueryString params = "?" <> joinWith "&" params



routeQueryBy :: forall by. By by => String -> Array String -> Array (Tuple String String) -> Array by -> String
routeQueryBy url paths params by = route url paths <> mkQueryString (by' <> flattenParams params)
  where
  by' = map show by



runDebug :: ApiEff Unit -> ApiEff Unit
runDebug fn = do
  (ApiOptions opts) <- ask
  if opts.apiDebug
     then do
       fn
       return unit
     else return unit



urlFromReader :: ApiEff String
urlFromReader = do
  (ApiOptions opts) <- ask
  let
    apiUrl'    = maybe opts.apiUrl id $ stripSuffix "/" opts.apiUrl
    apiPrefix' = maybe opts.apiPrefix id $ stripSuffix "/" opts.apiPrefix
  return $ apiUrl' <> "/" <> apiPrefix'



-- | getAt
--
getAt ::
      forall a by.
      (Respondable a, By by)
      => Array (Tuple String String)
      -> Array by
      -> Array String
      -> ApiEff (Either ForeignError a)
getAt params by paths = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params by
  runDebug (liftAff $ log ("getAt: " <> url'))
  { response: response } <- lift $ AJ.get url'
  let r = fromResponse response
  case r of
         (Left err) -> do
           runDebug (liftAff $ log $ ("getAt: Error: " ++ show err))
           return $ Left err
         (Right js) -> do
           runDebug (liftAff $ log "getAt: Success.")
           return $ Right js



-- | postAt
--
postAt ::
       forall a b by.
       (Respondable a, Requestable b, By by)
       => Array (Tuple String String)
       -> Array by
       -> Array String
       -> b
       -> ApiEff (Either ForeignError a)
postAt params by paths body = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params by
  runDebug (liftAff $ log ("postAt: " <> url'))
  { response: response } <- lift $ AJ.post url' body
  let r = fromResponse response
  case r of
         (Left err) -> do
           runDebug (liftAff $ log $ ("postAt: Error: " ++ show err))
           return $ Left err
         (Right js) -> do
           runDebug (liftAff $ log "postAt: Success.")
           return $ Right js




-- | updateAt
--
putAt ::
         forall a b by.
         (Respondable a, Requestable b, By by)
         => Array (Tuple String String)
         -> Array by
         -> Array String
         -> b
         -> ApiEff (Either ForeignError a)
putAt params by paths body = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params by
  runDebug (liftAff $ log ("putAt: " <> url'))
  { response: response } <- lift $ AJ.put url' body
  let r = fromResponse response
  case r of
         (Left err) -> do
           runDebug (liftAff $ log $ ("putAt: Error: " ++ show err))
           return $ Left err
         (Right js) -> do
           runDebug (liftAff $ log "putAt: Success.")
           return $ Right js



-- | deleteAt
--
deleteAt ::
         forall by.
         (By by)
         => Array (Tuple String String)
         -> Array by
         -> Array String
         -> ApiEff (Either ForeignError Unit)
deleteAt params by paths = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params by
  runDebug (liftAff $ log ("deleteAt: " <> url'))
  { response: response } <- lift $ AJ.delete url'
  let r = fromResponse response
  case r of
         (Left err) -> do
           runDebug (liftAff $ log $ ("deleteAt: Error: " ++ show err))
           return $ Left err
         (Right unit) -> do
           runDebug (liftAff $ log "deleteAt: Success.")
           return $ Right unit
