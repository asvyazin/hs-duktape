module Scripting.Duktape where


import Control.Monad
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import Data.Vector
import Scripting.Duktape.Raw


pushValue :: CDukContext -> Value -> IO ()
pushValue ctx Null =
  dukPushNull ctx
pushValue ctx (Bool b) =
  dukPushBoolean ctx b
pushValue ctx (Number scientific) =
  dukPushNumber ctx $ toRealFloat scientific
pushValue ctx (String str) =
  void $ dukPushString ctx $ T.unpack str
pushValue ctx (Array arr) = do
  arrIdx <- dukPushArray ctx
  imapM_ (\ i elem -> do
             pushValue ctx elem
             dukPutPropIndex ctx arrIdx i) arr
pushValue ctx (Object obj) = undefined
