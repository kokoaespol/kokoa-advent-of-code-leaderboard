{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad                 (forM_, forever, guard, void)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Function                 (on)
import           Data.List                     (sortBy)
import           Data.Maybe                    (isJust)
import           GHC.Generics                  (Generic)
import           System.Environment            (lookupEnv)

import           Configuration.Dotenv          (defaultConfig, loadFile)
import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar

import           Control.Monad.STM             (atomically)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.ByteString               (ByteString)
import           Data.FileEmbed                (embedFile)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as TE
import           Network.HTTP.Req
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5.Attributes   as A
import           Web.Scotty                    hiding (header)

data Member = Member
  { id           :: Int
  , global_score :: Int
  , name         :: Text
  , local_score  :: Int
  , stars        :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data LeaderBoard = LeaderBoard
  { owner_id :: Int
  , members  :: Map Text Member
  , event    :: Text
  }
  deriving (Show, Generic, FromJSON)

-- TODO: Add exception handling to this.
-- | Get the leaderboard from the advent of code API.
getLeaderBoard :: ByteString -> IO LeaderBoard
getLeaderBoard cookie = runReq defaultHttpConfig $ do
  r <- req
        GET
        (https "adventofcode.com" /: "2023" /: "leaderboard" /: "private" /: "view" /: "1468863.json")
        NoReqBody
        jsonResponse
        (header "Cookie" cookie)
  return $ responseBody r

-- | Start a loop to sync the leaderboard every 15 minutes.
syncLeaderBoard :: ByteString -> TVar LeaderBoard -> IO ()
syncLeaderBoard cookie leaderBoard = void $ forkIO $ forever $ do
  newLeaderBoard <- getLeaderBoard cookie
  atomically $ writeTVar leaderBoard newLeaderBoard
  threadDelay 900

-- | Fetch the initial leaderboard.
newLeaderBoard :: ByteString -> IO (TVar LeaderBoard)
newLeaderBoard cookie =  getLeaderBoard cookie >>= newTVarIO

leaderBoardCss :: ByteString
leaderBoardCss = $(embedFile "styles.css")

leaderBoardView :: LeaderBoard -> Html
leaderBoardView leaderBoard = do
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href "/styles.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/fontawesome.min.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/regular.min.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/solid.min.css"
    H.style . H.toHtml . TE.decodeUtf8 $ leaderBoardCss
  H.body $ do
    H.h1 ! A.class_ "shiny" $ "Advent of Code 2023!"
    H.h2 "Leaderboard"
    H.p $ do
      H.span "Join our leaderboard with the code: "
      H.span ! A.class_ "shiny" $ "1468863-c36b5be4"
    H.ul $ forM_ (zip [1..] . sortBy (compare `on` local_score) . M.elems . members $ leaderBoard) $ \(n, member) -> do
      H.li $ do
        H.div ! A.class_ "user-item user-name" $ do
          H.span ! A.class_ "shiny" $ H.toHtml (n :: Integer)
          H.span $ H.toHtml $ name member
        H.div ! A.class_ "user-item user-score" $ do
          H.span "Local Score:"
          H.span $ H.toHtml $ local_score member
        H.div ! A.class_ "user-item user-stars" $ do
          H.i ! A.class_ "fa-solid fa-star star" $ mempty
          H.span $ H.toHtml $ stars member

server :: TVar LeaderBoard -> IO ()
server leaderBoardRef = scotty 3000 $ do
  get "/" $ do
    leaderBoard <- liftIO $ readTVarIO leaderBoardRef
    html $ renderHtml $ leaderBoardView leaderBoard

dummyLeaderBoard :: LeaderBoard
dummyLeaderBoard = LeaderBoard
  { owner_id = 1468863
  , members = M.fromList
    [ ("james", Member 1 100 "james" 100 100)
    , ("james2", Member 2 200 "james2" 200 200)
    ]
  , event = "2023"
  }

main :: IO ()
main = do
  loadFile defaultConfig
  cookie <- lookupEnv "AOC_COOKIE"
  guard $ isJust cookie
  -- leaderBoardRef <- newLeaderBoard
  -- syncLeaderBoard leaderBoardRef
  leaderBoardRef <- newTVarIO dummyLeaderBoard
  server leaderBoardRef
