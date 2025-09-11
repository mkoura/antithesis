{-
This is a module to check emails for test results.
-}
module User.Agent.PublishResults.Email
    ( readEmails
    , readEmail
    , printEmails
    )
where

import Control.Applicative (asum, (<|>))
import Control.Lens (view, (&))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except
    ( ExceptT
    , except
    , runExceptT
    , withExceptT
    )
import Core.Types.Basic (Username (..))
import Data.Attoparsec.ByteString.Char8 (takeByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.IMF
    ( BodyHandler (RequiredBody)
    , Message
    , body
    , headerDate
    , message
    , parse
    )
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
    ( LocalTime
    , UTCTime (..)
    , ZonedTime (zonedTimeToLocalTime)
    , addUTCTime
    , getCurrentTime
    , getCurrentTimeZone
    , secondsToNominalDiffTime
    , toGregorian
    , utcToLocalTime
    )
import Data.Typeable (Typeable)
import Network.HaskellNet.IMAP
    ( SearchQuery
    , fetch
    , login
    , search
    , select
    )
import Network.HaskellNet.IMAP.SSL
    ( SearchQuery (..)
    , connectIMAPSSL
    )
import Network.HaskellNet.IMAP.Types (MailboxName)
import Streaming
    ( Of
    , Stream
    )
import Streaming.Prelude (each)
import Streaming.Prelude qualified as S
import System.Directory.Internal.Prelude (Exception, try)
import System.Time
    ( CalendarTime (..)
    , ClockTime
    , Day (..)
    , TimeDiff (..)
    , addToClockTime
    , getClockTime
    , noTimeDiff
    , toCalendarTime
    , toClockTime
    )
import Text.HTML.Parser (Attr (..), Token (..), parseTokens)
import Text.JSON.Canonical (FromJSON (..), parseCanonicalJSON)
import User.Agent.PushTest (TestRunWithId (..))
import User.Types (TestRun)

newtype Password = Password String
    deriving (Show, Eq)

newtype Hours = Hours Int
    deriving (Show, Eq)

tryX :: Exception e' => (e' -> e) -> IO c -> ExceptT e IO c
tryX f a = withExceptT f $ lift (try a) >>= except

data EmailException where
    ConnectionFailed :: IOError -> EmailException
    LoginFailed :: IOError -> EmailException
    ListFailed :: IOError -> EmailException
    SelectFailed :: IOError -> EmailException
    SearchFailed :: IOError -> EmailException
    FetchFailed :: IOError -> EmailException
    deriving (Show, Typeable, Eq)

instance Exception EmailException

data Result = Result
    { description :: TestRun
    , date :: ZonedTime
    , links :: T.Text
    }
    deriving (Show)

compareResults
    :: Either ParsingError Result -> Either ParsingError Result -> Ordering
compareResults (Right a) (Right b) =
    compare
        (zonedTimeToLocalTime $ date b)
        (zonedTimeToLocalTime $ date a)
compareResults (Left (WithDate d' _)) (Left (WithDate d'' _)) =
    compare (zonedTimeToLocalTime d'') (zonedTimeToLocalTime d')
compareResults (Left (WithDate d' _)) (Right (Result _ d'' _)) =
    compare (zonedTimeToLocalTime d'') (zonedTimeToLocalTime d')
compareResults (Right (Result _ d' _)) (Left (WithDate d'' _)) =
    compare (zonedTimeToLocalTime d'') (zonedTimeToLocalTime d')
compareResults (Right _) _ = GT
compareResults (Left (WithDate _ _)) _ = GT
compareResults _ _ = LT

sortResults
    :: [Either ParsingError Result] -> [Either ParsingError Result]
sortResults = sortBy compareResults

keepInLimit
    :: LocalTime
    -> [Either ParsingError Result]
    -> [Either ParsingError Result]
keepInLimit limit = takeWhile $ not . isOld
  where
    isOld (Right (Result _ d _)) = zonedTimeToLocalTime d < limit
    isOld (Left (WithDate d _)) = zonedTimeToLocalTime d < limit
    isOld _ = False

data Parameters
    = Parameters
    { paramHours :: Hours
    , paramFrom :: String
    }
    deriving (Show, Eq)

parameters :: Parameters
parameters =
    Parameters
        { paramHours = Hours 24
        , paramFrom = "antithesis@cardanofoundation.org"
        }

printEmails
    :: Stream
        (Of (Either ParsingError Result))
        (ExceptT EmailException IO)
        ()
    -> IO (Either EmailException ())
printEmails emails = runExceptT $ S.mapM_ (liftIO . print) emails

readEmails
    :: Username
    -> Password
    -> Int
    -- ^ days
    -- ^ limit to emails since this time
    -> Stream
        (Of (Either ParsingError Result))
        (ExceptT EmailException IO)
        ()
readEmails (Username username) (Password password) past = do
    conn <- lift $ tryX ConnectionFailed $ connectIMAPSSL "imap.gmail.com"
    _ <- lift $ tryX LoginFailed $ login conn username password
    _ <- lift $ tryX SelectFailed $ select conn allMail
    now <- liftIO getCurrentTime
    let limit =
            addUTCTime
                (negate $ secondsToNominalDiffTime $ fromIntegral $ past * 24 * 60 * 60)
                now
    let clockLimit = toClockTime $ utcTimeToCalendarTime limit
    tz <- liftIO getCurrentTimeZone -- wrong, should be the email server's timezone
    let localTimeLimit = utcToLocalTime tz limit
    let go from
            | from < clockLimit = pure ()
            | otherwise = do
                (timeSearch, to) <- liftIO $ nextSlot from (paramHours parameters)
                uids <-
                    lift
                        $ tryX SearchFailed
                        $ search conn
                        $ antithesisMail : timeSearch
                results <- forM uids $ \uid -> do
                    content <- lift $ tryX FetchFailed $ fetch conn uid
                    if B.null content
                        then pure Nothing
                        else pure $ Just $ readEmail content
                each $ keepInLimit localTimeLimit $ sortResults $ catMaybes results
                go to
    liftIO getClockTime >>= go

nextSlot :: ClockTime -> Hours -> IO ([SearchQuery], ClockTime)
nextSlot now (Hours hours) = do
    let oldestTime = addToClockTime noTimeDiff{tdHour = -hours} now
    newestCalendar <- toCalendarTime now
    oldestCalendar <- toCalendarTime oldestTime
    pure
        ([NOTs $ BEFOREs oldestCalendar, BEFOREs newestCalendar], oldestTime)

antithesisMail :: SearchQuery
antithesisMail = FROMs (paramFrom parameters)

allMail :: MailboxName
allMail = "[Gmail]/All Mail"

data ParsingError
    = ParsingError String B.ByteString
    | WithDate ZonedTime WithDateError
    | NoDate
    deriving (Show)

data WithDateError
    = DescriptionMissingOrUnusable
    | LinkMissing
    deriving (Show, Eq)

readEmail
    :: B.ByteString -> Either ParsingError Result
readEmail input =
    case parse (message (const $ RequiredBody takeByteString)) input of
        Left errMsg -> Left $ ParsingError errMsg input
        Right msg -> parseBody msg

toTokens :: B.ByteString -> [Token]
toTokens = parseTokens . T.decodeUtf8

nothingLeft :: a -> Maybe b -> Either a b
nothingLeft = flip maybe Right . Left

parseBody :: Message ctx B.ByteString -> Either ParsingError Result
parseBody content =
    view headerDate content & \mdate -> do
        date <- nothingLeft NoDate mdate
        toTokens (view body content) & \ts ->
            Result
                <$> nothingLeft
                    (WithDate date DescriptionMissingOrUnusable)
                    (findDescription ts)
                <*> pure date
                <*> nothingLeft
                    (WithDate date LinkMissing)
                    (listToMaybe (findLinks ts))

findDescription :: [Token] -> Maybe TestRun
findDescription = asum . fmap f
  where
    f (ContentText d)
        | " Description:" `T.isPrefixOf` d = do
            json <-
                BL.fromStrict . T.encodeUtf8 . T.strip
                    <$> T.stripPrefix " Description:" d
            case parseCanonicalJSON json of
                Left _ -> Nothing
                Right value -> testRun <$> fromJSON value <|> fromJSON value
    f _ = Nothing

findLinks :: [Token] -> [T.Text]
findLinks = mapMaybe f
  where
    f (TagOpen "a" [Attr "href" l]) = Just l
    f _ = Nothing

-- UTCTime -> CalendarTime (UTC-based)
utcTimeToCalendarTime :: UTCTime -> CalendarTime
utcTimeToCalendarTime utc =
    let day = utctDay utc
        diffTime = utctDayTime utc
        (seconds :: Int, picos) = properFraction (realToFrac diffTime :: Double) -- Seconds and fractional part
        (year, month, dayOfMonth) = toGregorian day
        -- Note: For full accuracy, use Data.Time.Calendar.Private helpers or adjust
        -- This is a basic mapping; test with known dates
        calTime =
            CalendarTime
                { ctYear = fromIntegral year
                , ctMonth = toEnum $ month `mod` 12 - 1
                , ctDay = fromIntegral dayOfMonth
                , ctHour = seconds `div` 3600 `mod` 24
                , ctMin = seconds `div` 60 `mod` 60
                , ctSec = seconds `mod` 60
                , ctPicosec = floor picos
                , ctWDay = Sunday
                , ctYDay = 0
                , ctTZName = "UTC"
                , ctTZ = 0 -- UTC offset
                , ctIsDST = False
                }
    in  calTime -- Refine fields with accurate Gregorian breakdown if needed
