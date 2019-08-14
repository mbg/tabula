--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.MyWarwick.API where 

--------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.MyWarwick.Response
import Warwick.MyWarwick.StreamItem

--------------------------------------------------------------------------------

type MyWarwickAuth = BasicAuth "" ()

type MyWarwick =
      MyWarwickAuth :>
      "streams" :> 
      Capture "provider" Text :> 
      "alerts" :>
      ReqBody '[JSON] StreamItem :>
      Post '[JSON] MyWarwickResponse
 :<|> MyWarwickAuth :>
      "streams" :> 
      Capture "provider" Text :> 
      "activities" :>
      ReqBody '[JSON] StreamItem :>
      Post '[JSON] MyWarwickResponse

-- | A proxy value for the `MyWarwick` type.
mywarwick :: Proxy MyWarwick
mywarwick = Proxy

--------------------------------------------------------------------------------

postAlert :: 
    BasicAuthData ->
    Text -> 
    StreamItem ->
    ClientM MyWarwickResponse

postActivity :: 
    BasicAuthData ->
    Text -> 
    StreamItem ->
    ClientM MyWarwickResponse


postAlert :<|> postActivity = client mywarwick

--------------------------------------------------------------------------------
    