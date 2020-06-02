--------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                        --
-- Copyright 2019 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Warwick.Sitebuilder.API where 

--------------------------------------------------------------------------------

import Data.Text
import Data.Proxy

import Text.XML as C

import Servant.API
import Servant.Client

import Warwick.Sitebuilder.Atom
import Warwick.Sitebuilder.PageInfo
import Warwick.Sitebuilder.PageUpdate

--------------------------------------------------------------------------------
        
instance MimeUnrender ATOM () where 
    mimeUnrender _ input = case parseLBS def input of 
        Left ex -> Left $ show ex
        Right d -> pure ()

type SitebuilderAuth = BasicAuth "" ()

type SitebuilderAPI =
      SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "atom.htm" :>
      QueryParam "page" Text :>
      QueryParam "type" Text :>
      ReqBody '[ATOM] PageUpdate :>
      Put '[ATOM] ()
 :<|> SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "atom.htm" :>
      QueryParam "page" Text :>
      ReqBody '[ATOM] PageUpdate :>
      POST '[ATOM] ()
 :<|> SitebuilderAuth :>
      "api" :>
      "page.json" :>
      QueryParam "page" Text :>
      Get '[JSON] PageInfo
 :<|> SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "atom.htm" :>
      QueryParam "page" Text :> 
      QueryParam "type" Text :>
      Delete '[ATOM] ()

-- | A proxy value for the 'SitebuilderAPI' type.
sitebuilder :: Proxy SitebuilderAPI
sitebuilder = Proxy

--------------------------------------------------------------------------------

editPage :: BasicAuthData -> Maybe Text -> Maybe Text -> PageUpdate -> ClientM ()
createPage :: BasicAuthData -> Maybe Text -> PageCreate -> ClientM ()
pageInfo :: BasicAuthData -> Maybe Text -> ClientM PageInfo
purge :: BasicAuthData -> Maybe Text -> Maybe Text -> ClientM ()

editPage :<|> createPage :<|> pageInfo :<|> purge = client sitebuilder

--------------------------------------------------------------------------------