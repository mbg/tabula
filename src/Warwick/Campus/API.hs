-------------------------------------------------------------------------------
-- Haskell bindings for the University of Warwick APIs                       --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Warwick.Campus.API (
    CampusAPI,
    campusAPI,
    listRooms
) where 

-------------------------------------------------------------------------------

import Data.Proxy
import Data.Text 

import Servant.API
import Servant.Client

import Warwick.Campus.Room

-------------------------------------------------------------------------------

type CampusAPI
    = "api" :>
      "v1" :>
      "projects" :>
      "1" :>
      "autocomplete.json" :>
      Header "Authorization" Text :>
      QueryParam "exact_limit" Int :>
      QueryParam "limit" Int :>
      QueryParam "term" Text :>
      QueryParam "substring" Bool :>
      Get '[JSON] [Room]

-- | A proxy value for the 'SitebuilderAPI' type.
campusAPI :: Proxy CampusAPI
campusAPI = Proxy

-- | 'listRooms' @token exactLimit limit term substring@ searches for rooms
-- on campus based on @term@. At most @limit@-many results are returned.
listRooms :: Maybe Text -- ^ The authorisation token.
          -> Maybe Int -- ^ The max. number of exact matches?
          -> Maybe Int -- ^ The max. number of results.
          -> Maybe Text -- ^ The text to search for, may be incomplete.
          -> Maybe Bool -- ^ "substring", set to false?
          -> ClientM [Room]
listRooms = client campusAPI

-------------------------------------------------------------------------------
