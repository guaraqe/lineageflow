module LineageFlow.Viewer.Utils
  ( eigenValuesSym3
  ) where

import LineageFlow.Prelude

eigenValuesSym3 :: Tensor -> (Vector,Tensor)
eigenValuesSym3 (V3 (V3 xx xy xz) (V3 yx yy yz) (V3 zx zy zz)) =
  let
    a11 = xx
    a12 = (xy + yx) / 2
    a13 = (xz + zx) / 2
    a22 = yy
    a23 = (yz + zy) / 2
    a33 = zz

    c2 = - a11 - a22 - a33
    c1 = a11*a22 + a11 * a33 + a22 * a33 - a12 ** 2 - a13 ** 3 - a23 ** 2
    c0 = a11 * a23 ** 2 + a22 * a13 ** 2 + a33 * a12 ** 2
       - a11 * a22 * a33 - 2 * a13 * a12 * a23

    p = c2 ** 2 - 3 * c1
    q = - 27 * c0 / 2 - c2 ** 3 + 9 * c2 * c1 / 2
    t = 2 * p ** (-3/2) * q

    theta = (/3) . atan . (/q) . sqrt . (*27) $
      c1 ** 2 * (p - c1) / 4 + c0 * (q + 27 * c0 / 4)

    x1 = 2 * cos theta
    x2 = - cos theta - (sqrt 3) * sin theta
    x3 = - cos theta + (sqrt 3) * sin theta

    lambda x = sqrt p * x / 3 - c2 / 3
    l1 = lambda x1
    l2 = lambda x2
    l3 = lambda x3

    crosser l =
      let
        y1 = V3 (a11 - l) a12 a13
        y2 = V3 a12 (a22 - l) a23
      in
        if colinear y1 y2
          then
            let
              mu = if y1 == pure 0 then 0 else (y1 !.! y2) / (y2 !.! y2)
            in V3 (1 / sqrt (1 + mu ** 2)) (- mu / sqrt (1 + mu ** 2)) 0
          else
            normalize $ cross y1 y2

    crosserD v l = normalize $ cross v (V3 a12 (a22 - l) a23)
    v1 = crosser l1
    v2 = if l1 == l2
           then crosserD v1 l2
           else crosser l2
    v3 = normalize $ cross v1 v2
    in
      (V3 l1 l2 l3, V3 v1 v2 v3)

colinear :: Vector -> Vector -> Bool
colinear v1 v2 =
  let (_,p) = project v1 v2 in norm p == 0

test :: Tensor
test =
  V3
    (V3 1 0 0)
    (V3 0 1 0)
    (V3 0 0 3)
