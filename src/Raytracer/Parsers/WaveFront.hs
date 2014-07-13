{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ===========================================================================
-- |
-- | WaveFront *.obj model parser
-- |
-- | ===========================================================================

module Raytracer.Parsers.WaveFront (Mesh, v, vn, f, extent
  ,vertices, normals, scaleFactor, parseFile, triangles)
where
import Prelude hiding (foldr)
import Data.Functor.Identity
import qualified Data.DList as DList
import Data.Foldable (foldr)
import Control.Lens ((^.), makeLenses)
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.ST
import Control.Monad.State (MonadState, runStateT, modify)
import Text.Printf (printf)
import Text.Parsec (ParsecT, runPT, getInput, setInput
  ,anyChar, manyTill, char, digit, many, many1, newline, try, space, spaces, string)
import Numeric (readSigned, readFloat)
import Data.Array.IArray (Array, listArray, (!), bounds)
import Raytracer.Space.Tensor as T
import Control.Applicative (empty, (<|>))

--------------------------------------------------------------------------------

-- | Data type to hold a vertex ("v") defintion
type V = PointR3


-- | Data type to hold a vertex normal ("vn") definition 
type VN = VectorR3


-- | Data type to hold a triangle face
data Face = 
    FaceV  (Int,Int,Int) 
  | FaceVN (Int,Int,Int) (Int,Int,Int)
  deriving (Show,Eq)


-- | Intermediate parser state ParseState for (v, vn, f, and normals)
type ParseState = (DList.DList V      -- ^ "v" defs
                  ,DList.DList VN     -- ^ "vn" defs
                  ,DList.DList Face   -- ^ Faces
                  ,Double             -- Global minimum component value
                  ,Double)            -- Global maximum component value


-- |
data Mesh = 
  Mesh 
  { _v           :: Array Int V 
  , _vn          :: Array Int VN
  , _f           :: Array Int Face
  , _extent      :: (Double,Double)
  , _scaleFactor :: Maybe Double
  } deriving (Eq)

makeLenses ''Mesh


-- | Triangle vertices + normals data type
data MeshTriangle = 
  Triangle 
  { _vertices :: (V,V,V)
  , _normals  :: (VN,VN,VN) 
  } deriving (Eq,Show)


makeLenses ''MeshTriangle

--------------------------------------------------------------------------------

instance Show Mesh where

  show mesh = printf 
    ("Mesh {\n"                      ++ 
     "  v           = %d\n"          ++ 
     "  vn          = %d\n"          ++
     "  f           = %d\n"          ++ 
     "  extent      = (%.2f,%.2f)\n" ++ 
     "  scaleFactor = %s\n"          ++ 
     "}") 
     v' 
     vn'
     f'
     gMin
     gMax
     scaleFact
    where
      (_,v')       = bounds $ mesh^.v
      (_,vn')      = bounds $ mesh^.vn
      (_,f')       = bounds $ mesh^.f
      (gMin,gMax)  = mesh^.extent
      scaleFact    =
        case (mesh^.scaleFactor) of
          Just factor -> show factor
          Nothing     -> "--"

--------------------------------------------------------------------------------

-- | Empty (initial) parser state
emptyState :: ParseState 
emptyState = (DList.empty, DList.empty, DList.empty, 0.0, 0.0)


-- | Returns a list of Triamnl, where each tuple contains a unique integer index
-- associated with each triangle as well as a triple corresponding to the
-- vertices of the resulting triangle, e.g. (PointR3,PointR3,PointR3)
triangles ::    Mesh           -- ^ A WaveFront *.obj triangle mesh
             -> [MeshTriangle]
triangles m = fmap mkMeshTriangle facesAndTris
  where
    build face vertices = DList.cons (face,toTriangle m face) vertices
    facesAndTris        = DList.toList $ foldr build DList.empty $ m^.f
    limit@(start,end)   = bounds $ m^.v
    epsilonV            = vec3 0 0 0
    findNormal (v0,v1,v2) = e1 `T.cross` e2
      where
        (e1,e2) = (v1 T./=> v0,v2 T./=> v0)

    avgNormals :: Array Int VectorR3
    avgNormals = runSTArray $ do
      normals <- newArray limit epsilonV :: ST s (STArray s Int VectorR3)
      forM_ facesAndTris $ \(FaceV (i,j,k),tri) -> do
        let n = findNormal tri
        ni <- readArray normals i
        nj <- readArray normals j
        nk <- readArray normals k
        writeArray normals i $ unit $ n + ni
        writeArray normals j $ unit $ n + nj 
        writeArray normals k $ unit $ n + nk
      return normals

    -- Calculate face normals and average over vertices case:
    mkMeshTriangle (FaceV (i,j,k),tri)    = Triangle tri (avgNormals ! i,avgNormals ! j,avgNormals ! k)
    -- Normals read from file case:
    mkMeshTriangle (FaceVN _ (i,j,k),tri) = Triangle tri ((m^.vn) ! i,(m^.vn) ! j,(m^.vn) ! k)
 

-- | Given a mesh definition and a triangle face index triple (i,j,k), this
-- function will return a triple of vertices specifying the given triangle's
-- face 
toTriangle ::    Mesh    -- ^ The mesh definition to read from
              -> Face    -- ^ The face index triple to convert to a triangle
              -> (V,V,V)
toTriangle m (FaceV (i,j,k))    = ((m^.v) ! i, (m^.v) ! j, (m^.v) ! k)
toTriangle m (FaceVN (i,j,k) _) = ((m^.v) ! i, (m^.v) ! j, (m^.v) ! k)

--------------------------------------------------------------------------------

-- | Parse the specified mesh *.obj file
parseFile ::    FilePath -- ^ The file to parse
             -> Bool     -- ^ Auto-scale. If True, all vertex components will be
                         -- scaled to the range [-1.0,1.0]
             -> IO Mesh
parseFile filePath autoScale = do 
  (v,vn,f,gMin,gMax) <- parseFile' filePath
  let scaleFact = abs gMin + abs gMax
  let scaler    = if autoScale then (\v -> v /. scaleFact) else id
  let v'        = fmap scaler $ DList.toList v
  let vn'       = DList.toList vn
  let f'        = DList.toList f
  return $ Mesh 
    { _v           = listArray (1, length v') v'
    , _vn          = listArray (1, length vn') vn'
    , _f           = listArray (1, length f') f' 
    , _extent      = (gMin,gMax)
    , _scaleFactor = if autoScale then Just scaleFact else Nothing }


-- | Internal function
parseFile' ::   FilePath       -- ^ The file to parse
             -> IO ParseState
parseFile' filePath = do 
  contents <- readFile filePath
  let (err,accum) = runIdentity $ (runStateT $ parseText contents) emptyState
  case err of
    Left err' -> error $ show err'
    Right _   -> return accum
  where
    parseText text = runPT (many line) () "WaveFront object file" text


-- | This function parses the given *.obj file based on the format described at
-- http://paulbourke.net/dataformats/obj/minobj.html
--
line :: (MonadState ParseState m) => ParsecT String st m ()
line = do
      try vertex
  <|> try normal
  <|> try vertexFace
  <|> vertexNormalFace
  <|> comment
  <|> smoothingOff
  <|> smoothingOn
  <|> leadingWS
  <|> trailingWS
  where
    vertex = do
      vNew <- vertexDef
      let (x',y',z') = (getX vNew,getY vNew,getZ vNew)
      modify $ \(v,vn,f,gMin,gMax) ->
                  (DList.snoc v vNew
                  ,vn
                  ,f
                  ,minimum [gMin,x',y',z']
                  ,maximum [gMax,x',y',z'])
    normal = do
      vnNew <- normalDef
      modify $ \(v,vn,f,gMin,gMax) -> 
                  (v,DList.snoc vn vnNew,f,gMin,gMax)
    vertexFace = do
      face <- faceWithVertex
      modify $ \(v,vn,f,gMin,gMax) -> 
                  (v,vn,DList.snoc f face,gMin,gMax)
    vertexNormalFace = do
      face <- faceWithVertexAndNormal
      modify $ \(v,vn,f,gMin,gMax) -> 
                  (v,vn,DList.snoc f face,gMin,gMax)


-- | Consumes any trailing whitespace, discarding the result
--
leadingWS :: (Monad m) => ParsecT String st m ()
leadingWS = do 
  _ <- manyTill space $ try anyChar
  return ()


-- | Consumes any trailing whitespace, discarding the result
--
trailingWS :: (Monad m) => ParsecT String st m ()
trailingWS = do 
  _ <- manyTill space $ try newline
  return ()


ignoreTilEnd :: (Monad m) => String -> ParsecT String st m ()
ignoreTilEnd ignore = do 
  _ <- string ignore
  _ <- manyTill anyChar $ try newline
  return ()


-- | Consumes any characters from the comment delimiter '#' to the EOL/EOF,
-- discarding the result
--
comment :: (Monad m) => ParsecT String st m ()
comment = ignoreTilEnd "#"


smoothingOff :: (Monad m) => ParsecT String st m ()
smoothingOff = ignoreTilEnd "s off"


smoothingOn :: (Monad m) => ParsecT String st m ()
smoothingOn = ignoreTilEnd "s on"


-- | Parses a floating point value
-- Adapted from ParsecTools-0.0.2.0
-- http://hackage.haskell.org/package/ParsecTools-0.0.2.0/docs/src/Text-Parsec-Numbers.html#parseFloat
--
floatValue :: (Monad m) => ParsecT String st m Double
floatValue = do
  s <- getInput
  case readSigned readFloat s of
    [(n,s')]  -> n <$ setInput s'
    _         -> empty


-- | Parse an integral value.
-- Adapted from ParsecTools-0.0.2.0
-- http://hackage.haskell.org/package/ParsecTools-0.0.2.0/docs/src/Text-Parsec-Numbers.html#parseFloat
intValue :: (Integral a, Read a, Monad m) => ParsecT String st m a
intValue = read <$> ((:) <$> signedValue <*> many1 digit)


-- | Parse the potential +/- before a number, returning ' ' for a '+'
-- Adapted from ParsecTools-0.0.2.0
-- http://hackage.haskell.org/package/ParsecTools-0.0.2.0/docs/src/Text-Parsec-Numbers.html#parseFloat
--
signedValue :: (Monad m) => ParsecT String st m Char
signedValue = (' ' <$ char '+') <|> char '-' <|> pure ' '


vertexDef :: (Monad m) => ParsecT String st m V
vertexDef = do
  _  <- string "v"
  _  <- spaces
  vx <- floatValue
  _  <- spaces
  vy <- floatValue
  _  <- spaces
  vz <- floatValue
  return $ point3 vx vy vz


normalDef :: (Monad m) => ParsecT String st m VN
normalDef = do 
  _  <- string "vn" 
  _  <- spaces
  nx <- floatValue
  _  <- spaces
  ny <- floatValue
  _  <- spaces
  nz <- floatValue
  return $ vec3 nx ny nz


vertexIndex :: (Monad m) => ParsecT String st m Int
vertexIndex = do 
  vi <- intValue
  return vi


vertexTextureIndex :: (Monad m) => ParsecT String st m (Int,Int)
vertexTextureIndex = do
  vi <- intValue
  _  <- spaces
  _  <- string "/"
  _  <- spaces
  ti <- intValue
  return (vi,ti)


vertexTextureNormalIndex :: (Monad m) => ParsecT String st m (Int,Int,Int)
vertexTextureNormalIndex = do 
  vi <- intValue
  _  <- spaces
  _  <- string "/"
  _  <- spaces
  ti <- intValue
  _  <- spaces
  _  <- string "/"
  _  <- spaces
  ni <- intValue
  return (vi,ti,ni)


vertexNormalIndex :: (Monad m) => ParsecT String st m (Int,Int)
vertexNormalIndex = do 
  vi <- intValue
  _  <- spaces
  _  <- string "//"
  _  <- spaces
  ni <- intValue
  return (vi,ni)


faceWithVertex :: (MonadState ParseState m) => ParsecT String st m Face
faceWithVertex = do
  _  <- string "f"
  _  <- spaces
  vx <- vertexIndex
  _  <- spaces
  vy <- vertexIndex
  _  <- spaces
  vz <- vertexIndex
  return $ FaceV (vx,vy,vz)


faceWithVertexAndNormal :: (MonadState ParseState m) => ParsecT String st m Face
faceWithVertexAndNormal = do
  _  <- string "f"
  _  <- spaces
  (vx,nx) <- vertexNormalIndex
  _       <- spaces
  (vy,ny) <- vertexNormalIndex
  _       <- spaces
  (vz,nz) <- vertexNormalIndex
  return (FaceVN (vx,vy,vz) (nx,ny,nz))
