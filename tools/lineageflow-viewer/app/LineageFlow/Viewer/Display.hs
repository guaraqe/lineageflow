{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Viewer.Display
  ( ImageChoice (..)
  , getImageChoice
  , displayScalar
  , displayVector
  , displayTensor
  , displayTri
  , Selection (..)
  , displaySelection
  , ColorMaker
  , linearColor
  ) where

import LineageFlow.Prelude
import LineageFlow.Viewer.Input
import LineageFlow.Viewer.Tensor

import qualified LineageFlow.ArrayU as ArrayU

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU.Quadrics

import Foreign.Storable
import Data.Text (Text)

--------------------------------------------------------------------------------

data ImageChoice =
  ImageScalar |
  ImageVector |
  ImageTensor |
  ImageSelection |
  ImageTri
  deriving Eq

getImageChoice :: Text -> ImageChoice
getImageChoice = \case
  "Scalar" -> ImageScalar
  "Vector" -> ImageVector
  "Tensor" -> ImageTensor
  "Selection" -> ImageSelection
  "Triangulation" -> ImageTri
  _ -> error "Invalid image, should not happen."

--------------------------------------------------------------------------------

nan :: Double
nan = 0/0

linearColor :: Double -> Double -> ColorMaker
linearColor c w s
  | isNaN s' = GL.Color3 0.5 0.5 0.5
  | s'  <= 0 = GL.Color3 0 0 1
  | s' <= 0.33 = GL.Color3 0 (3*s') (1-3*s')
  | s' <= 0.66 = GL.Color3 (3*(s'-0.33)) 1 0
  | s' <= 1.00 = GL.Color3 1 (1-3*(s'-0.66)) 0
  | otherwise = GL.Color3 1 0 0
  where s' = (s - c) / w + 0.5

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(a,b,c) -> f a b c

isNaNVector :: Vector -> Bool
isNaNVector = getAny . foldMap (Any . isNaN)

isNaNTensor :: Tensor -> Bool
isNaNTensor = getAny . foldMap (foldMap (Any . isNaN))

type ColorMaker = Scalar -> GL.Color3 GL.GLdouble

--------------------------------------------------------------------------------

displayVertex :: ColorMaker -> Time -> TCMapA Vector -> IO ()
displayVertex maker time (Compose vertex) =
  ArrayU.mapM_ (\v -> renderSphere maker v nan) $
    (unIx (vertex ! time))


{-
  GL.renderPrimitive GL.Points $
    ArrayU.mapM_ (renderPoint maker nan) $
      (unIx (vertex ! time))
-}

renderPoint :: ColorMaker -> Scalar -> Vector -> IO ()
renderPoint maker scalar (V3 x y z) = do
  GL.color (maker scalar)
  GL.vertex (GL.Vertex3 x y z)

idTensor :: Tensor
idTensor = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

--------------------------------------------------------------------------------

displayScalar :: ColorMaker -> Time -> TCMapA Vector -> TCMapA Scalar -> IO ()
displayScalar maker time (Compose vertex) (Compose scalar) = do
    ArrayU.mapM_ (\(s,v) -> renderSphere maker v s) $
      ArrayU.zipWith (,)
        (unIx (scalar ! time))
        (unIx (vertex ! time))

--------------------------------------------------------------------------------

displayGroup :: ColorMaker -> Time -> TCMapA Vector -> TCMapA Int -> IO ()
displayGroup maker time (Compose vertex) (Compose scalar) = do
    ArrayU.mapM_ (\(s,v) -> renderSphere maker v s) $
      ArrayU.zipWith (,)
        (_fmap fromIntegral $ unIx (scalar ! time))
        (unIx (vertex ! time))

--------------------------------------------------------------------------------

displayVector :: Double -> ColorMaker -> Time -> TCMapA Vector -> TCMapA Vector -> IO ()
displayVector scale maker time (Compose vertex) (Compose vector) =
  GL.renderPrimitive GL.Lines $
    ArrayU.mapM_ (uncurry (renderVector scale maker)) $
      ArrayU.zipWith (,)
        (unIx (vertex ! time))
        (unIx (vector ! time))

renderVector :: Double -> ColorMaker -> Vector -> Vector -> IO ()
renderVector scale maker (V3 px py pz) vector =
  if isNaNVector vector
    then do
      -- Should change, very ugly
      renderPoint maker nan (V3 px py pz)
      renderPoint maker nan (V3 px py pz)
    else do
      GL.color (maker scalar)
      GL.vertex (GL.Vertex3 px py pz)
      GL.vertex (GL.Vertex3 (px + scale * x) (py + scale * y) (pz + scale * z))
      where (scalar ,(V3 x y z)) = normalVector vector

normalVector :: Vector -> (Double, Vector)
normalVector v =
  let n = norm v in
    if n == 0 then (0, pure 0) else (n, fmap (/n) v)

--------------------------------------------------------------------------------

displayTensor :: Double -> ColorMaker -> Time -> TCMapA Vector -> TCMapA Tensor -> IO ()
displayTensor scale maker time (Compose vertex) (Compose tensor) =
    ArrayU.mapM_ (uncurry (renderTensor scale maker)) $
      ArrayU.zipWith (,)
        (unIx (vertex ! time))
        (unIx (tensor ! time))

signedNorm :: Vector -> Double
signedNorm v@(V3 x y z) = signum (x * y * z)

renderTensor :: Double -> ColorMaker -> Vector -> Tensor -> IO ()
renderTensor scale maker vertex tensor =
  if isNaNTensor tensor || tensor == pure (pure 0)
    then
      renderPoint maker nan vertex
    else do
      let
        (sca,rot) = eigen3D tensor
        scalar = norm sca
      GL.color (maker scalar)
      GL.preservingMatrix $ do
        GL.matrixMode GL.$= GL.Modelview 0
        withTranslation vertex $
          withRotation rot $
            withScale (fmap (/scalar) sca) $
              renderQuadric
                (QuadricStyle
                  (Just GL.Smooth)
                  GenerateTextureCoordinates
                  Outside
                  FillStyle)
                (Sphere scale 8 4)

renderSphere :: ColorMaker -> Vector -> Scalar -> IO ()
renderSphere maker vertex scalar = do
  GL.color (maker scalar)
  GL.preservingMatrix $ do
    GL.matrixMode GL.$= GL.Modelview 0
    withTranslation vertex $
      renderQuadric
        (QuadricStyle
          (Just GL.Smooth)
          GenerateTextureCoordinates
          Outside
          FillStyle)
        (Sphere 2 8 4)


withTranslation :: Vector -> IO () -> IO ()
withTranslation (V3 x y z) action = do
  GL.translate (GL.Vector3 x y z)
  action
  GL.translate (GL.Vector3 (-x) (-y) (-z))
{-# INLINE withTranslation #-}

withRotation :: Tensor -> IO () -> IO ()
withRotation t action = do
  rot <- rotationMatrix t GL.RowMajor
  rotT <- rotationMatrix t GL.ColumnMajor
  GL.multMatrix rot
  action
  GL.multMatrix rotT
{-# INLINE withRotation #-}

withScale :: Vector -> IO () -> IO ()
withScale (V3 l1 l2 l3) action = do
  GL.scale (abs l1) (abs l2) (abs l3)
  action
  GL.scale (1/abs l1) (1/abs l2) (1/abs l3)
{-# INLINE withScale #-}

rotationMatrix :: Tensor -> GL.MatrixOrder -> IO (GL.GLmatrix Double)
rotationMatrix (V3 (V3 xx xy xz) (V3 yx yy yz) (V3 zx zy zz)) order = let
  putAt ptr = do
    pokeElemOff ptr 0 xx
    pokeElemOff ptr 1 xy
    pokeElemOff ptr 2 xz
    pokeElemOff ptr 3 0

    pokeElemOff ptr 4 yx
    pokeElemOff ptr 5 yy
    pokeElemOff ptr 6 yz
    pokeElemOff ptr 7 0

    pokeElemOff ptr 8 zx
    pokeElemOff ptr 9 zy
    pokeElemOff ptr 10 zz
    pokeElemOff ptr 11 0

    pokeElemOff ptr 12 0
    pokeElemOff ptr 13 0
    pokeElemOff ptr 14 0
    pokeElemOff ptr 15 1
  in
    GL.withNewMatrix order putAt

--------------------------------------------------------------------------------

displayTri ::
 ColorMaker ->
 Time ->
 TCMapA Vector ->
 Maybe (TSMapA Scalar) ->
 TSMapA (S2 (Dep Time Cell)) ->
 IO ()
displayTri maker time (Compose vertex) (Just (Compose scalar)) (Compose tri)  =
  GL.renderPrimitive GL.Lines $
    ArrayU.mapM_ (uncurry3 (renderLine maker)) $
      ArrayU.zipWith3 (,,)
        (unIx (scalar ! time))
        (unIx vertex1)
        (unIx vertex2)
  where
    vertexTime = vertex ! time
    triTime = tri ! time
    vertex1 = _fmap (\(S2 i j) -> vertexTime ! i) triTime
    vertex2 = _fmap (\(S2 i j) -> vertexTime ! j) triTime

displayTri maker time (Compose vertex) Nothing (Compose tri) =
  GL.renderPrimitive GL.Lines $
    ArrayU.mapM_ (uncurry (renderLine maker nan)) $
      ArrayU.zipWith (,)
        (unIx vertex1)
        (unIx vertex2)
  where
    vertexTime = vertex ! time
    triTime = tri ! time
    vertex1 = _fmap (\(S2 i j) -> vertexTime ! i) triTime
    vertex2 = _fmap (\(S2 i j) -> vertexTime ! j) triTime

renderLine :: ColorMaker -> Scalar -> Vector -> Vector -> IO ()
renderLine maker scalar (V3 px py pz) (V3 x y z) = do
  GL.color (maker scalar)
  GL.vertex (GL.Vertex3 px py pz)
  GL.vertex (GL.Vertex3 x y z)

--------------------------------------------------------------------------------

data Selection = Selection
  { selection_vertex :: TCMapA Vector
  , selection_scalar ::  Maybe (TCMapA Scalar)
  , selection_vector :: Maybe (TCMapA Vector)
  , selection_tensor :: Maybe (TCMapA Tensor)
  , selection_group :: Maybe (TCMapA Int)
  , selection_tri :: Maybe (TSMapA (S2 (Dep Time Cell)))
  , selection_triScalar :: Maybe (TSMapA Scalar)
  }

displaySelection :: ImageChoice -> Double -> ColorMaker -> Time -> Selection -> IO ()
displaySelection
  image scale maker time (Selection vertex scalar vector tensor group tri triScalar) =
  case image of
    ImageScalar -> maybe dispVertex (displayScalar maker time vertex) scalar
    ImageVector -> maybe dispVertex (displayVector scale maker time vertex) vector
    ImageTensor -> maybe dispVertex (displayTensor scale maker time vertex) tensor
    ImageSelection -> maybe dispVertex (displayGroup maker time vertex) group
    ImageTri -> maybe dispVertex (displayTri maker time vertex triScalar) tri
  where dispVertex = displayVertex maker time vertex
