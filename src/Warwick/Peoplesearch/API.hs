--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Peoplesearch.API where 

--------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Servant.API
import Servant.Client

import Warwick.Peoplesearch.Response
import Warwick.Peoplesearch.Profile

--------------------------------------------------------------------------------

type PeoplesearchAuth = BasicAuth "" ()

type PeoplesearchApi =
      PeoplesearchAuth :>
      "search" :> 
      "search.json" :>
      QueryParam "query" Text :>
      Get '[JSON] (PeoplesearchResponse Profile)
 :<|> PeoplesearchAuth :>
      "profiles" :> 
      CaptureAll "endpoint" Text :>
      Get '[JSON] (PeoplesearchResponse Profile)

-- | A proxy value for the `API` type.
peoplesearch :: Proxy PeoplesearchApi
peoplesearch = Proxy

--------------------------------------------------------------------------------

searchProfiles :: 
    BasicAuthData -> 
    Maybe Text -> 
    ClientM (PeoplesearchResponse Profile)

lookupProfile :: 
    BasicAuthData -> 
    [Text] -> 
    ClientM (PeoplesearchResponse Profile)

searchProfiles :<|> lookupProfile = client peoplesearch

--------------------------------------------------------------------------------
