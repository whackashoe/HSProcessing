
module Processing.Shader where

import Graphics.Rendering.OpenGL.Raw.Core32
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Unsafe.Coerce
import qualified Data.Map as Map

newtype Shader = Shader GLuint 
    deriving (Eq, Show)

    -- merci DMcGill_
checkError :: IO [GLenum]
checkError = do
  e <- glGetError
  if e == gl_NO_ERROR
    then return []
    else (e:) <$> checkError  
      

errorStrings e 
    | e == gl_NO_ERROR                          = "No error"
    | e == gl_INVALID_ENUM                      = "Invalid enum"
    | e == gl_INVALID_VALUE                     = "Invalid value"
    | e == gl_INVALID_OPERATION                 = "Invalid Operation"
    | e == gl_INVALID_FRAMEBUFFER_OPERATION     = "Invalid Framebuffer Operation"
    | e == gl_OUT_OF_MEMORY                     = "Out of Memory. Fatal."
    | otherwise                                 = "Unknown error."


formattedErrorCodes :: [GLenum] -> String
formattedErrorCodes = unlines . map errorStrings

reportErrors :: [GLenum] -> IO ()
reportErrors = putStrLn . formattedErrorCodes



makeShader :: T.Text -> T.Text -> IO Shader
makeShader vertexsrc fragmentsrc = do
    vertex <- glCreateShader gl_VERTEX_SHADER
    fragment <- glCreateShader gl_FRAGMENT_SHADER
    shaderSource vertex vertexsrc
    shaderSource fragment fragmentsrc
    program <- glCreateProgram 
    glAttachShader program vertex
    glAttachShader program fragment
    glLinkProgram program
    pure $ Shader program
        
        
shaderSource :: GLuint -> T.Text -> IO ()
shaderSource sh text = do
    BSU.unsafeUseAsCString bytestring shader
    e <- checkError
    case e of
        [] -> pure ()
        _ -> fail $ formattedErrorCodes e
    where
        bytestring = TE.encodeUtf8 text
        shader cstr = 
            withArray [cstr] (\s -> glShaderSource sh 1 ((castPtr s)::(Ptr (Ptr GLchar))) nullPtr)

-- mm_freak suggested:
{-

checkError :: EitherT Error IO ()


createShaders :: EitherT Error IO (Shader, Shader)
createShaders = do
    vertex <- liftIO (glCreateShader gl_VERTEX_SHADER) <* checkError
    fragment <- liftIO (glCreateShader gl_FRAGMENT_SHADER) <* checkError
    pure (Shader vertex, Shader fragment)
    
    -}
    
    -- or maybe something better with the free monad
createShaders :: IO (Shader, Shader)
createShaders = do
    vertex <- glCreateShader gl_VERTEX_SHADER
    e <- checkError
    case e of
        [] -> do
            fragment <- glCreateShader gl_FRAGMENT_SHADER
            e <- checkError 
            case e of
                [] -> pure (Shader vertex, Shader fragment)
                _  -> fail $ formattedErrorCodes e
        _  -> fail $ formattedErrorCodes e