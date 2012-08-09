
module Processing.Shader where

import Graphics.Rendering.OpenGL.Raw.Core32
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
{-

makeShader :: T.Text -> T.Text -> IO Shader
makeShader vertex fragment = do
    vertex <- glCreateShader gl_VERTEX_SHADER
    fragment <- glCreate
    where compileShader typ text = do
        shader <- gl_CreateShader typ
        glShaderSource shader 1 
        
export_to_C :: T.Text -> IO (Ptr CString)
export_to_C input = do
    -}
runAddSource :: Shader -> T.Text -> IO ()
runAddSource (Shader sh) text = BSU.unsafeUseAsCString bytestring shader
    where
        bytestring = TE.encodeUtf8 text
        shader cstr = 
            withArray [cstr] (\s -> glShaderSource 1 1 ((castPtr s)::(Ptr (Ptr GLchar))) nullPtr)



            
            