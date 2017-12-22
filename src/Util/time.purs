module Util.Time where

import Prelude

-- | Nullary class used to raise a custom warning for the debug functions.
class DebugWarning

instance warn :: Warn "Util.Time usage" => DebugWarning

-- | Log any PureScript value to the console for debugging purposes and then
-- | return a value. This will log the value's underlying representation for
-- | low-level debugging.
foreign import time :: forall b. DebugWarning => String -> (Unit -> b) -> b
