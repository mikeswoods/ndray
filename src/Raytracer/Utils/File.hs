{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module Raytracer.Utils.File (realPath, resolvePath) 
where
import System.Directory (canonicalizePath, getCurrentDirectory)
import Control.Exception (catch, SomeException)

--------------------------------------------------------------------------------

-- | Given a path, relative or absolute, this function tries to return
-- the actual absolute path. 
realPath ::    FilePath            -- ^ The path to convert
            -> IO (Maybe FilePath)
realPath path = do
  getPath <- catch (canonicalizePath path) handler
  case getPath of 
    "" -> return Nothing
    p  -> return $ Just p
  where
    handler :: SomeException -> IO FilePath
    handler _ = return ""


-- | This function attempts to resolve a file relative to a given source
-- filepath.
resolvePath ::    Maybe FilePath      -- ^ The source file path
               -> Maybe FilePath      -- ^ The relative file path
               -> IO (Maybe FilePath)
resolvePath Nothing Nothing = do
  pwd <- getCurrentDirectory
  return $ Just pwd
resolvePath Nothing (Just relativeTo) = do
  pwd <- getCurrentDirectory  
  realPath $ pwd ++ "/" ++ relativeTo
resolvePath (Just path) _ = realPath path
