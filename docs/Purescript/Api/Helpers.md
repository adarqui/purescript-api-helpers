## Module Purescript.Api.Helpers

#### `ApiEff`

``` purescript
type ApiEff a = forall eff. ReaderT ApiOptions (Aff (ajax :: AJAX, console :: CONSOLE | eff)) a
```

#### `ApiOptions`

``` purescript
data ApiOptions
  = ApiOptions { apiUrl :: String, apiPrefix :: String, apiDebug :: Boolean }
```

#### `ApiError`

``` purescript
data ApiError
  = ServerError ForeignError
  | DecodeError String
```

##### Instances
``` purescript
Show ApiError
```

#### `QueryParam`

``` purescript
class QueryParam a where
  qp :: a -> Tuple String String
```

##### Instances
``` purescript
QueryParam Boolean
QueryParam (Tuple String String)
```

#### `defaultApiOptions`

``` purescript
defaultApiOptions :: ApiOptions
```

#### `route`

``` purescript
route :: String -> Array String -> String
```

#### `flattenParams`

``` purescript
flattenParams :: forall qp. QueryParam qp => Array qp -> Array String
```

#### `mkQueryString`

``` purescript
mkQueryString :: Array String -> String
```

#### `routeQueryBy`

``` purescript
routeQueryBy :: forall qp. QueryParam qp => String -> Array String -> Array qp -> String
```

#### `runDefault`

``` purescript
runDefault :: forall m a. ReaderT ApiOptions m a -> m a
```

#### `rD`

``` purescript
rD :: forall m a. ReaderT ApiOptions m a -> m a
```

#### `runWith`

``` purescript
runWith :: forall m a. ReaderT ApiOptions m a -> ApiOptions -> m a
```

#### `rW`

``` purescript
rW :: forall m a. ReaderT ApiOptions m a -> ApiOptions -> m a
```

#### `runDebug`

``` purescript
runDebug :: forall a. ApiEff a -> ApiEff Unit
```

#### `urlFromReader`

``` purescript
urlFromReader :: ApiEff String
```

#### `handleError`

``` purescript
handleError :: forall a. Respondable a => Either ForeignError a -> Either ApiError a
```

#### `getAt`

``` purescript
getAt :: forall a qp. (Respondable a, QueryParam qp) => Array qp -> Array String -> ApiEff (Either ForeignError a)
```

getAt

#### `postAt`

``` purescript
postAt :: forall a b qp. (Respondable a, Requestable b, QueryParam qp) => Array qp -> Array String -> b -> ApiEff (Either ForeignError a)
```

postAt

#### `putAt`

``` purescript
putAt :: forall a b qp. (Respondable a, Requestable b, QueryParam qp) => Array qp -> Array String -> b -> ApiEff (Either ForeignError a)
```

updateAt

#### `deleteAt`

``` purescript
deleteAt :: forall qp. QueryParam qp => Array qp -> Array String -> ApiEff (Either ForeignError Unit)
```

deleteAt


