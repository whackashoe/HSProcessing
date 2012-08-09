
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

newtype Shader = Shader GLuint 
    deriving (Eq, Show)


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
shaderSource sh text = BSU.unsafeUseAsCString bytestring shader
    where
        bytestring = TE.encodeUtf8 text
        shader cstr = 
            withArray [cstr] (\s -> glShaderSource sh 1 ((castPtr s)::(Ptr (Ptr GLchar))) nullPtr)



            
            