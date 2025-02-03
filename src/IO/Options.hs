module IO.Options
(
  withStdinInputOption
, withStdoutOutputOption
) where

import Data.Maybe ( fromMaybe ) 
import IO.Types ( Output(..), Input(..) )
import Options.Applicative ( optional, Parser )

withStdinInputOption :: Parser Input -> Parser Input
withStdinInputOption p = fromMaybe Stdin <$> optional p

withStdoutOutputOption :: Parser Output -> Parser Output
withStdoutOutputOption p = fromMaybe Stdout <$> optional p
