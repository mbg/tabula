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
import Warwick.Sitebuilder.Page
import Warwick.Sitebuilder.FileOptions

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
      ReqBody '[ATOM] Page :>
      Post '[ATOM] ()
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
 :<|> SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "file.htm" :>
      QueryParam "page" Text :> 
      QueryParam "type" Text :>
      ReqBody '[ATOM] FileOptions :>
      Put '[ATOM] ()
 :<|> SitebuilderAuth :>
      "edit" :>
      "atom" :>
      "file.htm" :>
      QueryParam "page" Text :> 
      QueryParam "type" Text :>
      Delete '[ATOM] ()

-- | A proxy value for the 'SitebuilderAPI' type.
sitebuilder :: Proxy SitebuilderAPI
sitebuilder = Proxy

--------------------------------------------------------------------------------

editPage :: BasicAuthData -> Maybe Text -> Maybe Text -> PageUpdate -> ClientM ()
createPage :: BasicAuthData -> Maybe Text -> Page -> ClientM ()
pageInfo :: BasicAuthData -> Maybe Text -> ClientM PageInfo
purgePage :: BasicAuthData -> Maybe Text -> Maybe Text -> ClientM ()
editFileProps :: BasicAuthData -> Maybe Text -> Maybe Text -> FileOptions -> ClientM ()
purgeFile :: BasicAuthData -> Maybe Text -> Maybe Text -> ClientM ()

editPage :<|> createPage :<|> pageInfo :<|> purgePage :<|> editFileProps :<|> purgeFile = client sitebuilder

--------------------------------------------------------------------------------