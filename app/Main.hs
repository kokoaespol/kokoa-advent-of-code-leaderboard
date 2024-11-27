{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Exception
import           Control.Monad                        (forM_, forever, void)
import           Data.Function                        (on)
import           Data.List                            (sortBy)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           GHC.Generics                         (Generic)
import           System.Environment                   (lookupEnv)

import           Configuration.Dotenv                 (defaultConfig, loadFile,
                                                       onMissingFile)
import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM                    (atomically)
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as B8
import           Data.FileEmbed                       (embedFile,
                                                       makeRelativeToProject)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as TE
import           Network.HTTP.Req
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (static)
import           Text.Blaze.Html                      (Html)
import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import qualified Text.Blaze.Html5                     as H
import           Text.Blaze.Html5                     ((!))
import qualified Text.Blaze.Html5.Attributes          as A
import           Web.Scotty                           hiding (catch, header)

data StarData = StarData
  { get_star_ts :: !Int
  , star_index  :: !Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type Day = Map Int StarData

data Member = Member
  { id                   :: !Int
  , global_score         :: !Int
  , name                 :: !Text
  , local_score          :: !Int
  , stars                :: !Int
  , completion_day_level :: !(Map Int Day)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data LeaderBoard = LeaderBoard
  { owner_id :: !Int
  , members  :: !(Map Text Member)
  , event    :: !Text
  }
  deriving (Show, Generic, FromJSON)

data State
  = WithLeaderBoard LeaderBoard
  -- ^ The application state when the leaderboard was successfully retrieved.
  | NoLeaderBoard
  -- ^ The application state when the leaderboard could not be retrieved.
  deriving (Show)

-- | Prepare the initial application state.
initState :: ByteString -> IO (TVar State)
initState cookie = getLeaderBoard cookie >>= newTVarIO . maybe NoLeaderBoard WithLeaderBoard

-- | Get the leaderboard from the advent of code API.
getLeaderBoard :: ByteString -> IO (Maybe LeaderBoard)
getLeaderBoard cookie = (Just <$> runReq defaultHttpConfig (do
  r <- req
        GET
        (https "adventofcode.com" /: "2024" /: "leaderboard" /: "private" /: "view" /: "1468863.json")
        NoReqBody
        jsonResponse
        (header "Cookie" cookie)
  return $ responseBody r)) `catch` (\(_ :: SomeException) -> pure Nothing)

-- | Start a loop to sync the leaderboard every 15 minutes.
syncLeaderBoard :: ByteString -> TVar State -> IO ()
syncLeaderBoard cookie leaderBoard = void $ forkIO $ forever $ do
  newLeaderBoard <- getLeaderBoard cookie
  case newLeaderBoard of
    Just newLeaderBoard' -> do
      atomically $ writeTVar leaderBoard (WithLeaderBoard newLeaderBoard')
      threadDelay (15 * 60 * 1000 * 1000)
    Nothing -> threadDelay (15 * 60 * 1000 * 1000)

leaderBoardCss :: ByteString
leaderBoardCss = $(makeRelativeToProject "styles.css" >>= embedFile)

viewHead :: String -> Html
viewHead title =
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href "/adventofcode/styles.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/fontawesome.min.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/regular.min.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/solid.min.css"
    H.link ! A.rel "stylesheet" ! A.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/brands.min.css"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.style . H.toHtml . TE.decodeUtf8 $ leaderBoardCss
    H.title $ H.toHtml title

viewLeaderBoard :: LeaderBoard -> Html
viewLeaderBoard leaderBoard = do
  H.ul $ forM_ (zip [1..] . sortBy (flip compare `on` local_score) . Map.elems . members $ leaderBoard) $ \(n, member) -> do
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

viewBody :: State -> Html
viewBody state = do
  H.div ! A.id "leaderboard-header" $ do
    H.div ! A.id "leaderboard-header-info" $ do
      H.img ! A.src "images/turtle.png" ! A.alt "turtle" ! A.width "150"
      H.div $ do
        H.h1 ! A.class_ "shiny" $ "Advent of Kokoa 2024"
        H.p ! A.class_ "shiny" $
          H.a ! A.href "/adventofcode/progress" $ "Ver progreso"
        H.p $ do
          H.p $ do
            H.i ! A.class_ "fa-solid fa-tree" $ mempty
            " Únete a nuestro leaderboard con el código: "
            H.span ! A.class_ "shiny" $ "1468863-c36b5be4 "
            H.a ! A.href "/how-to-join" $ "¿Cómo me uno?"
          H.p $ do
            H.i ! A.class_ "fab fa-instagram" $ mempty
            " Sigue al club Kokoa en Instagram "
            H.a ! A.href "https://www.instagram.com/kokoa_espol/" $ "@kokoa_espol"
            " para más"
    H.img ! A.src "images/gecko.png" ! A.alt "gecko" ! A.width "150"
  case state of
    NoLeaderBoard -> H.p "Oops! We can't retrieve the leaderboard right now. Try again in 15 minutes!"
    WithLeaderBoard leaderBoard -> viewLeaderBoard leaderBoard

view :: State -> Html
view state = viewHead "LeaderBoard | Advent of Kokoa" >> H.body (viewBody state)

viewHowToJoin :: Html
viewHowToJoin = viewHead "¿Cómo unirme? | Advent of Kokoa" >> H.body (do
  H.h1 "¿Cómo unirme?"
  H.p $ do
    "Visita la página web del Advent of Code y crea una cuenta si no la tienes: "
    H.a ! A.href "https://adventofcode.com/2024" $ "https://adventofcode.com/2024. "
    "Haz clic en \"Leaderboard\" en la barra de opciones:"
  H.img ! A.src "images/01.png" ! A.alt "step 01"
  H.p "Ahora, haz clic en \"Private Leaderboard\":"
  H.img ! A.src "images/02.png" ! A.alt "step 02"
  H.p $ do
    "Por último, ingresa el código en el campo de texto: "
    H.span ! A.class_ "shiny" $ "1468863-c36b5be4"
  H.img ! A.src "images/03.png" ! A.alt "step 03"
  H.p "¡Eso es todo! Happy hacking!")

viewChart :: Html
viewChart = viewHead "Progreso | Advent of Kokoa" >> H.body (do
  H.h1 ! A.class_ "shiny" $ "Progreso"
  H.canvas ! A.id "chart-p1" $ mempty
  H.canvas ! A.id "chart-p2" $ mempty
  H.script ! A.src "https://cdn.jsdelivr.net/npm/chart.js" $ mempty
  H.script ! A.src "/chart.js" $ mempty)

renderRoot :: TVar State -> ActionM ()
renderRoot stateRef = do
  state <- liftIO $ readTVarIO stateRef
  html $ renderHtml $ view state

getMembers :: TVar State -> ActionM ()
getMembers stateRef = do
  state <- liftIO $ readTVarIO stateRef
  case state of
    NoLeaderBoard               -> json ([] :: [()])
    WithLeaderBoard leaderBoard -> json . Map.elems . members $ leaderBoard

server :: Int -> TVar State -> IO ()
server port stateRef = scotty port $ do
  middleware static
  middleware logStdout

  get "/" $ renderRoot stateRef
  get "/adventofcode" $ renderRoot stateRef

  get "/how-to-join" . html . renderHtml $ viewHowToJoin
  get "/adventofcode/how-to-join" . html . renderHtml $ viewHowToJoin

  get "/progress" . html . renderHtml $ viewChart
  get "/adventofcode/progress" . html . renderHtml $ viewChart

  get "/members" $ getMembers stateRef
  get "/adventofcode/members" $ getMembers stateRef


main :: IO ()
main = do
  loadFile defaultConfig `onMissingFile` return ()
  cookie <- lookupEnv "AOC_COOKIE"
  port <- fmap (fmap read) (lookupEnv "PORT")
  case (cookie, port) of
    (Nothing, _) -> error "AOC_COOKIE is not set!"
    (_, Nothing) -> error "PORT is not set!"
    (Just cookie', Just port) -> do
      let cookie'' = B8.pack cookie'
      stateRef <- initState cookie''
      syncLeaderBoard cookie'' stateRef
      server port stateRef
