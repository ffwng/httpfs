module CommandLine where

import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Control.Applicative
import Options.Applicative

type BasicAuth = (B.ByteString, B.ByteString)

data CmdArgs = CmdArgs {
  disableCertificateValidation :: Bool,
  basicAuth :: Maybe BasicAuth,
  linkXPath :: String,
  logRequests :: Bool,
  baseUrl :: String,
  mountPoint :: String,
  otherArgs :: [String]
  }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
          <$> switch ( long "insecure"
                       <> short 'k'
                       <> help "Disable certificate validation" )
          <*> optional ( option (eitherReader readAuth)
                         ( long "auth"
                           <> short 'u'
                           <> metavar "USER:PASS"
                           <> help "Basic authentication (username:password)" ) )
          <*> strOption (long "xpath" <> help "XPath for <a> elements (default //a)" <> value "//a")
          <*> switch (long "log-requests" <> help "Logs HTTP requests (should be used with -- -f)")
          <*> strArgument ( metavar "URL"
                            <> help "URL of root directory" )
          <*> strArgument ( metavar "MOUNT-POINT"
                          <> help "Mount point for FUSE" )
          <*> many (strArgument $ metavar "ARGS..."
                    <> help "Additional FUSE arguments like -f -d (use --)")

readAuth :: String -> Either String (B.ByteString, B.ByteString)
readAuth s = case B.split ':' $ B.pack s of
  [u, p] -> Right (u, p)
  _ -> Left "Wrong format for basic authentication. Use username:password"
