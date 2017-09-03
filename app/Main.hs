module Main where

import Lib
import Data.Either
import Data.Either.Unwrap
import Data.Ini
import Data.Semigroup ((<>))
import Network.HostName (getHostName)
import Options.Applicative
import qualified Data.Text as T

configFile :: FilePath
configFile = "/etc/cronwrap.conf"

data Cronwrap = Cronwrap
  { command    :: String }

arguments :: Parser Cronwrap
arguments = Cronwrap
  <$> strOption
      ( long "command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help "Specify a command that will be invoked" )

cronwrap :: Cronwrap -> IO ()
cronwrap (Cronwrap c) = do
  ini <- fromRight <$> readIniFile configFile
  let readText section key = fromRight $ lookupValue section key ini

  h <- getHostName
  let host = readText "xmpp" "host"
      user = readText "xmpp" "user"
      pass = readText "xmpp" "pass"
      messageTo = readText "xmpp" "messageTo"
      localHost = T.pack h
      command = T.pack c

  runJob host user pass messageTo localHost command

main :: IO ()
main = cronwrap =<< execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc
      <> progDesc "execute a command and send its error output over xmpp"
      <> header "Cronwrap" )
