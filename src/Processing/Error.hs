
module Processing.Error where

-- import Control.Error
import Graphics.Rendering.OpenGL.Raw.Core32
import Control.Monad
import Control.Applicative

data Error = InvalidEnum 
            | InvalidValue
            | InvalidOperation
            | InvalidFrameBufferOperation
            | OutOfMemory
            | Unknown
            deriving (Show, Eq)



    -- merci DMcGill_
checkError :: IO [Error]
checkError = (liftM . map) glenum2Error errs
    where errs = do  
              e <- glGetError
              if e == gl_NO_ERROR
                then return []
                else (e:) <$> errs
          glenum2Error e 
            | e == gl_INVALID_ENUM                      = InvalidEnum
            | e == gl_INVALID_VALUE                     = InvalidValue
            | e == gl_INVALID_OPERATION                 = InvalidOperation
            | e == gl_INVALID_FRAMEBUFFER_OPERATION     = InvalidFrameBufferOperation
            | e == gl_OUT_OF_MEMORY                     = OutOfMemory
            | otherwise                                 = Unknown
    
