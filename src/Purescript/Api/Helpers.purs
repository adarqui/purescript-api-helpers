module Purescript.Api.Helpers (
  ApiOptions (..),
  ApiError (..),
  ApiEff,
  class QueryParam,
  qp,
  defaultApiOptions,
  route,
  flattenParams,
  mkQueryString,
  routeQueryBy,
  runDefault,
  rD,
  runWith,
  rW,
  runDebug,
  urlFromReader,
  handleError,
  getAt,
  postAt,
  putAt,
  deleteAt
) where



import Control.Monad.Aff            (Aff)
import Control.Monad.Aff.Class      (liftAff)
import Control.Monad.Aff.Console    (log)
import Control.Monad.Eff.Console    (CONSOLE())
import Control.Monad.Except         (runExcept)
import Control.Monad.Reader.Trans   (ReaderT, lift, ask, runReaderT)
import Data.Array                   (cons)
import Data.Either                  (Either(Left, Right))
import Data.Foreign                 (Foreign, ForeignError (..))
import Data.List                    (head)
import Data.List.Types              (NonEmptyList (..))
import Data.Maybe                   (Maybe (..), maybe)
import Data.NonEmpty                (NonEmpty (..))
import Data.String                  (joinWith, stripSuffix, Pattern (..))
import Data.Tuple                   (Tuple(..), fst, snd)
import Network.HTTP.Affjax          (AJAX)
import Network.HTTP.Affjax          as AJ
import Network.HTTP.Affjax.Request  (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable, fromResponse)
import Prelude                      (class Show, Unit, pure, ($), bind, show, (<>) , unit, map, id, void)



type ApiEff a = forall eff. ReaderT ApiOptions (Aff (ajax :: AJAX, console :: CONSOLE | eff)) a



data ApiOptions = ApiOptions {
  apiUrl     :: String,
  apiPrefix  :: String,
  apiDebug   :: Boolean
}



data ApiError b
  = ServerError ForeignError b
  | ServerErrorUnknown ForeignError
  | DecodeError String



data ApiMethod
  = GET
  | POST
  | PUT
  | DELETE



instance apiMethodShow :: Show ApiMethod where
  show GET    = "get"
  show POST   = "post"
  show PUT    = "put"
  show DELETE = "delete"



instance apiErrorShow :: Show (ApiError b) where
  show (DecodeError err)   = err
  show (ServerError err _) = show err
  show (ServerErrorUnknown err) = show err



class QueryParam a where
  qp :: a -> Tuple String String



instance tupleBoolQueryParam :: QueryParam Boolean where
  qp b = Tuple "bool" (show b)



instance tupleStringQueryParam :: QueryParam (Tuple String String) where
  qp = id



defaultApiOptions :: ApiOptions
defaultApiOptions = ApiOptions {apiUrl: "/", apiPrefix: "api", apiDebug: true}



route :: String -> Array String -> String
route url paths = joinWith "/" (url `cons` paths)



flattenParams :: forall qp. QueryParam qp => Array qp -> Array String
flattenParams [] = []
flattenParams params = map (\par -> let tup = qp par in fst tup <> "=" <> snd tup) params



mkQueryString :: Array String -> String
mkQueryString [] = ""
mkQueryString params = "?" <> joinWith "&" params



routeQueryBy :: forall qp. QueryParam qp => String -> Array String -> Array qp -> String
routeQueryBy url paths params = route url paths <> mkQueryString (flattenParams params)



runDefault :: forall m a. ReaderT ApiOptions m a -> m a
runDefault actions = runReaderT actions defaultApiOptions



rD :: forall m a. ReaderT ApiOptions m a -> m a
rD = runDefault



runWith :: forall m a. ReaderT ApiOptions m a -> ApiOptions -> m a
runWith actions state = runReaderT actions state



rW :: forall m a. ReaderT ApiOptions m a -> ApiOptions -> m a
rW = runWith



runDebug :: forall a. ApiEff a -> ApiEff Unit
runDebug fn = do
  (ApiOptions opts) <- ask
  if opts.apiDebug
     then do
       void $ fn
       pure unit
     else pure unit



runDebugError :: ApiMethod -> ForeignError -> String -> ApiEff Unit
runDebugError method err url  = do
  runDebug (liftAff $ log $ (show method <> "At: " <> url <> ", error: " <> show err))
  pure unit



runDebugSuccess :: ApiMethod -> String -> ApiEff Unit
runDebugSuccess method url = do
  runDebug (liftAff $ log (show method <> "At: " <> url <> ", success"))
  pure unit



runDebugAnnounce :: ApiMethod -> String -> ApiEff Unit
runDebugAnnounce method url = do
  runDebug (liftAff $ log (show method <> "At: " <> url))
  pure unit



urlFromReader :: ApiEff String
urlFromReader = do
  (ApiOptions opts) <- ask
  let
    apiUrl'    = maybe opts.apiUrl id $ stripSuffix (Pattern "/") opts.apiUrl
    apiPrefix' = maybe opts.apiPrefix id $ stripSuffix (Pattern "/") opts.apiPrefix
  pure $ apiUrl' <> "/" <> apiPrefix'



handleError :: forall a b. (Respondable a, Respondable b) => Either (Tuple ForeignError b) a -> Either (ApiError b) a
handleError (Left (Tuple status body)) = Left $ ServerError status body
handleError (Right js)                 = Right js



-- | baseAt
--
-- Base function which performs logging and such.
--
baseAt ::
  forall eff a b.
  (Respondable b)
  => ApiMethod
  -> String
  -> Aff ( ajax :: AJAX , console :: CONSOLE | eff) { response :: Foreign | a }
  -> ReaderT ApiOptions (Aff (ajax :: AJAX , console :: CONSOLE | eff)) (Either ForeignError b)
baseAt who url fn = do
  runDebugAnnounce who url
  { response: response } <- lift fn
  let r = runExcept (fromResponse response)
  case r of
         (Left (NonEmptyList (NonEmpty _ err))) -> do
           case head err of
             Just h -> pure $ Left h
             _      -> pure $ Left (ForeignError "unknown")
         (Right js) -> do
           runDebugSuccess who url
           pure $ Right js



-- | getAt
--
getAt ::
      forall a qp.
      (Respondable a, QueryParam qp)
      => Array qp
      -> Array String
      -> ApiEff (Either ForeignError a)
getAt params paths = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params
  baseAt GET url' (AJ.get url')



-- | postAt
--
postAt ::
       forall a b qp.
       (Respondable a, Requestable b, QueryParam qp)
       => Array qp
       -> Array String
       -> b
       -> ApiEff (Either ForeignError a)
postAt params paths body = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params
  baseAt POST url' (AJ.post url' body)



-- | updateAt
--
putAt ::
      forall a b qp.
      (Respondable a, Requestable b, QueryParam qp)
      => Array qp
      -> Array String
      -> b
      -> ApiEff (Either ForeignError a)
putAt params paths body = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params
  baseAt PUT url' (AJ.put url' body)



-- | deleteAt
--
deleteAt ::
         forall qp.
         (QueryParam qp)
         => Array qp
         -> Array String
         -> ApiEff (Either ForeignError Unit)
deleteAt params paths = do
  url <- urlFromReader
  let url' = routeQueryBy url paths params
  baseAt DELETE url' (AJ.delete url')
