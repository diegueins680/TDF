{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerFanClub
  ( fanClubPublicGetClub
  , fanClubPublicGetEvents
  , fanClubSecureArtistHandlers
  , fanClubSecureListMyClubs
  ) where

import           Control.Monad.Reader (ReaderT)
import           Data.ByteString.Lazy (fromStrict)
import           Data.Int (Int64)
import           Data.Text.Encoding (encodeUtf8)
import           Servant

import           TDF.Auth (AuthedUser)
import           TDF.DB (Env)
import           TDF.DTO

type AppM = ReaderT Env Handler

type FanClubArtistAPI =
       Get '[JSON] FanClubDTO
  :<|> "posts" :> Get '[JSON] [FanClubPostDTO]
  :<|> "posts" :> ReqBody '[JSON] FanClubCreatePostReq :> Post '[JSON] FanClubPostDTO
  :<|> "posts" :> Capture "postId" Int64 :> "pin" :> Post '[JSON] NoContent
  :<|> "posts" :> Capture "postId" Int64 :> "unpin" :> Post '[JSON] NoContent
  :<|> "posts" :> Capture "postId" Int64 :> "hide" :> Post '[JSON] NoContent
  :<|> "posts" :> Capture "postId" Int64 :> "unhide" :> Post '[JSON] NoContent
  :<|> "events" :> Get '[JSON] [FanClubEventDTO]
  :<|> "events" :> ReqBody '[JSON] FanClubCreateEventReq :> Post '[JSON] FanClubEventDTO
  :<|> "elections" :> Get '[JSON] [FanClubElectionDTO]
  :<|> "elections" :> ReqBody '[JSON] FanClubCreateElectionReq :> Post '[JSON] FanClubElectionDTO
  :<|> "elections" :> Capture "electionId" Int64 :> "candidacy" :> ReqBody '[JSON] FanClubCreateCandidacyReq :> Post '[JSON] FanClubCandidacyDTO
  :<|> "elections" :> Capture "electionId" Int64 :> "vote" :> ReqBody '[JSON] FanClubVoteReq :> Post '[JSON] NoContent

fanClubPublicGetClub :: Int64 -> AppM FanClubDTO
fanClubPublicGetClub _ =
  fanClubUnavailable

fanClubPublicGetEvents :: Int64 -> AppM [FanClubEventDTO]
fanClubPublicGetEvents _ =
  fanClubUnavailable

fanClubSecureListMyClubs :: AuthedUser -> AppM [FanClubDTO]
fanClubSecureListMyClubs _ =
  fanClubUnavailable

fanClubSecureArtistHandlers :: AuthedUser -> Int64 -> ServerT FanClubArtistAPI AppM
fanClubSecureArtistHandlers _ _ =
       fanClubUnavailable
  :<|> fanClubUnavailable
  :<|> (\_ -> fanClubUnavailable)
  :<|> (\_ -> fanClubUnavailable)
  :<|> (\_ -> fanClubUnavailable)
  :<|> (\_ -> fanClubUnavailable)
  :<|> (\_ -> fanClubUnavailable)
  :<|> fanClubUnavailable
  :<|> (\_ -> fanClubUnavailable)
  :<|> fanClubUnavailable
  :<|> (\_ -> fanClubUnavailable)
  :<|> (\_ _ -> fanClubUnavailable)
  :<|> (\_ _ -> fanClubUnavailable)

fanClubUnavailable :: AppM a
fanClubUnavailable =
  throwError err501
    { errBody = fromStrict (encodeUtf8 "Fan club backend is not configured in this build")
    }
