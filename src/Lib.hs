{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Lib
  ( runJob
  , runCommand
  ) where

import GHC.TypeLits

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.List.Split
import Turtle ((<>), date, empty, procStrictWithErr, ExitCode(..))

-- XMPP imports
import Data.XML.Types
import Network
import Network.Protocol.XMPP

type a :?  (b::Symbol) = a

runJob :: T.Text :? "Server hostname"
       -> JID    :? "JID"
       -> T.Text :? "Password"
       -> T.Text :? "Recipient"
       -> T.Text :? "Hostname"
       -> T.Text :? "Command" -> IO ()
runJob hostname jid password messageTo localHostName command = do
  username <-
    case strNode `fmap` jidNode jid of
      Just x -> return x
      Nothing -> error $ "JID must include a username"
  let server =
        Server
        { serverHostname = T.unpack hostname
        , serverJID = JID Nothing (jidDomain jid) Nothing
        , serverPort = PortNumber 5222
        }

  let statusText = localHostName <> ": running `" <> command <> "`"
  result <- liftIO $ newEmptyMVar
  notified <- liftIO $ newEmptyMVar

  liftIO $ flip forkFinally (\_ -> putMVar
    notified ()) $
    void $ runClient server jid username password $ do
      boundJID <- bindJID jid
      -- Send a "ping" every 60 seconds
      getSession >>= liftIO . forkIO . sendPings 60
      liftIO $ putStrLn $ "Server bound our session to: " ++ show boundJID
      -- Populate presence with relevant info
      putStanza (status statusText)
      (exitCode, results) <- liftIO $ takeMVar result
      case exitCode of
        ExitFailure n -> mapM_ (putStanza . sendMsg (parseJID messageTo)) results
        ExitSuccess -> liftIO $
          print $ "`" <> command <> "` completed successfuly"
      -- TODO: There seems to be a problem with IO flushing in
      -- network-protocol-xmpp, so I had to make an ugly hack for now
      liftIO . threadDelay . fromInteger $ 1000000 * 15

  -- runCommand is executed in a separate thread so it doesn't crash with xmpp thread in case of
  -- connectivity issues or other protocol errors
  putMVar result =<< runCommand command localHostName
  takeMVar notified

runCommand :: T.Text :? "Command"
           -> T.Text :? "Hostname"
           -> IO (ExitCode, [T.Text] :? "Report")
runCommand command localHostName = do
  let chunk = 15000
  let nL = "\n"
  let separator = nL <> T.concat (replicate 60 "-") <> nL
  dateBegin <- date
  (exitCode, stdout, stderr) <- procStrictWithErr command [] empty
  dateEnd <- date
  let results = map T.pack $ chunksOf chunk $ T.unpack $ T.concat
        [ "Hosname:\n\t"
        , localHostName
        , nL
        , "Command:\n\t"
        , command
        , nL
        , "ExitCode:\n\t"
        , (T.pack $ show exitCode)
        , nL
        , "Started:\n\t"
        , (T.pack $ show dateBegin)
        , nL
        , "Finished:\n\t"
        , (T.pack $ show dateEnd)
        , nL
        , separator
        , "Error output:"
        , separator
        , stderr
        , separator
        , "Standart output:"
        , separator
        , stdout
        ]
  return (exitCode, results)

sendMsg :: Maybe JID -> T.Text -> Message
sendMsg messageTo content =
  Message
  { messageType = MessageNormal
  , messageTo = messageTo
  , messageFrom = Nothing
  , messageID = Nothing
  , messageLang = Nothing
  , messagePayloads =
      [ Element
        { elementName =
            Name
            { nameLocalName = "body"
            , nameNamespace = Just "jabber:client"
            , namePrefix = Nothing
            }
        , elementAttributes = []
        , elementNodes = [NodeContent (ContentText content)]
        }
      ]
  }

status :: T.Text -> Presence
status payload =
  Presence
  { presenceType = PresenceAvailable
  , presenceTo = Nothing
  , presenceFrom = Nothing
  , presenceID = Nothing
  , presenceLang = Nothing
  , presencePayloads =
      [ Element
        { elementName =
            Name
            { nameLocalName = "priority"
            , nameNamespace = Just "jabber:client"
            , namePrefix = Nothing
            }
        , elementAttributes = []
        , elementNodes = [NodeContent (ContentText "50")]
        }
      , Element
        { elementName =
            Name
            { nameLocalName = "status"
            , nameNamespace = Just "jabber:client"
            , namePrefix = Nothing
            }
        , elementAttributes = []
        , elementNodes = [NodeContent (ContentText payload)]
        }
      ]
  }

-- Send a "ping" occasionally, to prevent server timeouts from
-- closing the connection.
sendPings :: Integer -> Session -> IO ()
sendPings seconds s = forever send
  where
    send
    -- Ignore errors
     = do
      runXMPP s $ putStanza ping
      threadDelay . fromInteger $ 1000000 * seconds
    ping = (emptyIQ IQGet) {iqPayload = Just (Element pingName [] [])}

pingName :: Name
pingName = Name "ping" (Just "urn:xmpp:ping") Nothing
