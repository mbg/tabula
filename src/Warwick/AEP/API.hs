-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.AEP.API (
    AEP,
    uploadFile
) where

--------------------------------------------------------------------------------

import Data.ByteString.Lazy ( ByteString )
import Data.Proxy
import Data.Text
import Data.UUID.Types

import Servant.API
import Servant.Client
import Servant.Multipart

import Warwick.AEP.FileUpload

-------------------------------------------------------------------------------

type AEPAuth = Header' '[Required, Strict] "Cookie" Text

type AEP =
      -- POST /assessment/:uuid/upload
      -- Attach a file to the assessment identified by `uuid`
      AEPAuth :>
      "assessment" :>
      Capture "assessmentID" UUID :>
      "upload" :>
      Header' '[Required, Strict] "OnlineExams-Upload" Bool :>
      Header' '[Required, Strict] "User-Agent" Text :>
      MultipartForm Tmp FileUpload :>
      Post '[*] NoContent

aep :: Proxy AEP
aep = Proxy

--------------------------------------------------------------------------------

uploadFile ::
    Text ->
    UUID ->
    Bool ->
    Text ->
    (ByteString, FileUpload) ->
    ClientM NoContent

uploadFile = client aep

--------------------------------------------------------------------------------
