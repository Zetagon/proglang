-- |

module Level1.Errors where
import Control.Exception



-- | The compiler did something wrong and our assumptions have been violated
data CompilerError = AccessUnavailableFieldError
                   | WordIsNotDefinedError
  deriving (Show, Eq)

instance Exception CompilerError
