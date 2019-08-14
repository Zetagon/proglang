-- |

module Level1.Errors where
import Control.Exception



-- | The compiler did something wrong and our assumptions have been violated
data CompilerError = AccessUnavailableFieldError -- ^ An access to an unavailable field in a record happend
                   | WordIsNotDefinedError -- ^ The word that was called is not defined
  deriving (Show, Eq)

instance Exception CompilerError
