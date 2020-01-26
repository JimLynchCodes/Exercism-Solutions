module HelloWorld where

import Prelude
import Data.Maybe (Maybe(Just, Nothing))

helloWorld :: Maybe String -> String
helloWorld (Just x) = "Hello, " <> x <> "!"
helloWorld (Nothing) = "Hello, World!"