{-# LANGUAGE OverloadedStrings #-}

module Processing.Shader where

import Graphics.Rendering.OpenGL.Raw.Core32
import Graphics.Rendering.OpenGL.Raw.Core31.TypesInternal
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Unsafe.Coerce
import Control.Error
import Control.Monad.IO.Class
import Foreign.Storable
import Data.Bits
import Data.Int
import Data.Word

import Processing.Error

newtype ShaderProgram = Shader GLuint 
    deriving (Eq, Show)

            
tryGL :: IO r -> EitherT [Error] IO r
tryGL m = liftIO $ m <* checkError

--formattedErrorCodes :: [GLenum] -> String
--formattedErrorCodes = unlines . map errorStrings

--reportErrors :: [GLenum] -> IO ()
--reportErrors = putStrLn . formattedErrorCodes


{-
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
        
  -}      
 
 {- 
makeShader :: T.Text -> T.Text -> EitherT [Error] (IO Shader)
makeShader vsrc fsrc = do
    (vertex, fragment) <- createShaders
    shaderSource vertex vsrc
    shaderSource fragment fsrc
    
-}


 -- from tekmo        
createShaders :: EitherT [Error] IO (GLuint, GLuint)
createShaders = do
    vertex <- tryGL $ glCreateShader gl_VERTEX_SHADER
    fragment <- tryGL $ glCreateShader gl_FRAGMENT_SHADER
    pure (vertex, fragment)

    
shaderSource :: GLuint -> T.Text -> EitherT [Error] IO ()
shaderSource sh text = do
    tryGL $ BSU.unsafeUseAsCString bytestring shader
    where
        bytestring = TE.encodeUtf8 text
        shader cstr = 
            withArray [cstr] (\s -> glShaderSource sh 1 ((castPtr s)::(Ptr (Ptr GLchar))) nullPtr)

compileShader :: GLuint -> EitherT [Error] IO T.Text
compileShader shader = do
    tryGL $ glCompileShader shader
    s <- tryGL $ compileStatus shader
    if s then tryGL $ infoLog shader
     else pure "" 

compileStatus :: GLuint -> IO Bool
compileStatus shader = do
    s <- alloca (\p -> glGetShaderiv shader gl_COMPILE_STATUS p >> peek p)
    pure $ (glTruth . glIntToEnum) s
    
infoLog :: GLuint -> IO T.Text
infoLog shader = do
    (GLint logsize) <- infoLogLength shader
    log' <- allocaArray (fromIntegral logsize) 
            (\p -> glGetShaderInfoLog shader (GLsizei logsize) nullPtr p >> peekCString (unsafeCoerce p))
    pure $ T.pack log'
    
infoLogLength :: (Integral a) => GLuint -> IO a
infoLogLength shader = do
    (GLint s) <- alloca (\p -> glGetShaderiv shader gl_INFO_LOG_LENGTH p >> peek p)
    pure $ (fromIntegral s)
    


glTruth :: GLenum -> Bool
glTruth x = if x == gl_TRUE then True
            else False
            
zzEncode32 :: Int32 -> Word32            
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))

glIntToEnum :: GLint -> GLenum
glIntToEnum (GLint (CInt x)) = GLenum (CUInt (zzEncode32 x))

-- fromGLint :: (Integral a, ) => GLint -> a
--fromGLint (GLint x) = x