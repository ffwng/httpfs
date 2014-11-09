module CommandLine where

import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Control.Applicative
import Options.Applicative

data CmdArgs = CmdArgs {
  disableCertificateValidation :: Bool,
  basicAuth :: Maybe (B.ByteString, B.ByteString),
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
