module Quat (
    Quat, showAsSum, showAsTup,
    qUnitR, qUnitI, qUnitJ, qUnitK, qZero, qNegOne,
    qGetR, qGetI, qGetJ, qGetK,
    qconj, qimag, qmodulus2, qmodulus, qnormalize, qcoords,
    qscale, qinv, qadd, qsub, qmult,
    qexp, qlog, qpow, qslerp
) where

import Debug.Trace
import Text.Printf (printf)

-- ========================================
-- Compare with mrd's implementation
--   https://github.com/mrd/haskell-quaternion/blob/master/Data/Quaternion.hs
data Quat = Quat Double Double Double Double
    deriving Eq

-- String functions
showAsSum :: Quat -> String
showAsSum (Quat r i j k) = f_r <> post_r <> f_i <> post_i <> f_j <> post_j <> f_k <> post_k
  where
    eps = 10e-6
    f_r = if (abs(r - fromIntegral(round r)) < eps) then (printf "%.0f" r) else (printf "%.6f" r)
    f_i = if (abs(i - fromIntegral(round i)) < eps) then (printf "%.0f" i) else (printf "%.6f" i)
    f_j = if (abs(j - fromIntegral(round j)) < eps) then (printf "%.0f" j) else (printf "%.6f" j)
    f_k = if (abs(k - fromIntegral(round k)) < eps) then (printf "%.0f" k) else (printf "%.6f" k)
    -- TODO (possible): Move sign in front of i/j/k values to preceding delimiting sign character.
    post_r = " + "
    post_i = "i + "
    post_j = "j + "
    post_k = "k"

showAsTup :: Quat -> String
showAsTup (Quat r i j k) = lp <> f_r <> c <> f_i <> c <> f_j <> c <> f_k <> rp
  where
    eps = 10e-6
    f_r = if (abs(r - fromIntegral(round r)) < eps) then (printf "%.0f" r) else (printf "%.6f" r)
    f_i = if (abs(i - fromIntegral(round i)) < eps) then (printf "%.0f" i) else (printf "%.6f" i)
    f_j = if (abs(j - fromIntegral(round j)) < eps) then (printf "%.0f" j) else (printf "%.6f" j)
    f_k = if (abs(k - fromIntegral(round k)) < eps) then (printf "%.0f" k) else (printf "%.6f" k)
    lp = "("
    c = ", "
    rp = ")"

instance Show Quat where
    show q = showAsSum q

qUnitR, qUnitI, qUnitJ, qUnitK :: Quat
qUnitR = Quat 1.0 0.0 0.0 0.0
qUnitI = Quat 0.0 1.0 0.0 0.0
qUnitJ = Quat 0.0 0.0 1.0 0.0
qUnitK = Quat 0.0 0.0 0.0 1.0

qZero, qNegOne :: Quat
qZero  = Quat   0.0  0.0 0.0 0.0
qNegOne= Quat (-1.0) 0.0 0.0 0.0

-- Unary operators
qGetR, qGetI, qGetJ, qGetK :: Quat -> Double
qGetR (Quat r _ _ _) = r
qGetI (Quat _ i _ _) = i
qGetJ (Quat _ _ j _) = j
qGetK (Quat _ _ _ k) = k

qconj, qimag :: Quat -> Quat
qconj (Quat r i j k) = Quat r (-i) (-j) (-k)
qimag (Quat _ i j k) = Quat 0 i j k

qmodulus2, qmodulus :: Quat -> Double
qmodulus2 (Quat r i j k) = r**2 + i**2 + j**2 + k**2
qmodulus q = sqrt $ qmodulus2 q

qnormalize :: Quat -> Quat
qnormalize q = (1/(qmodulus q)) `qscale` q

qcoords :: Quat -> (Double, Double, Double, Double)
qcoords (Quat r i j k) = (r, i, j, k)

-- Binary operators
qscale :: Double -> Quat -> Quat
qscale s (Quat r i j k) = Quat (s*r) (s*i) (s*j) (s*k)

qinv :: Quat -> Quat
qinv q = (1.0 / (qmodulus2 q)) `qscale` (qconj q)

qadd :: Quat -> Quat -> Quat
qadd (Quat r1 i1 j1 k1) (Quat r2 i2 j2 k2) = Quat (r1+r2) (i1+i2) (j1+j2) (k1+k2)

qsub :: Quat -> Quat -> Quat
qsub (Quat r1 i1 j1 k1) (Quat r2 i2 j2 k2) = Quat (r1-r2) (i1-i2) (j1-j2) (k1-k2)

qmult :: Quat -> Quat -> Quat
qmult (Quat r1 i1 j1 k1) (Quat r2 i2 j2 k2) = Quat res_a res_b res_c res_d
  where
    res_a = r1 * r2 - i1 * i2 - j1 * j2 - k1 * k2
    res_b = r1 * i2 + i1 * r2 + j1 * k2 - k1 * j2
    res_c = r1 * j2 - i1 * k2 + j1 * r2 + k1 * i2
    res_d = r1 * k2 + i1 * j2 - j1 * i2 + k1 * r2

-- Exponentiation, Log, and Slerp functions

-- How to find the exp of a quaternion:
-- Given an arbitrary quaternion q, q = r + v, for some r in R, and some purely imaginary quaternion, v.
-- Then exp(v) = cos theta + sin theta * vhat, where theta = |v| and vhat = v / |v|
-- So exp(q) = exp(r) * (cos theta + sin theta * vhat)
qexp :: Quat -> Quat
qexp q@(Quat r i j k) = exp(r) `qscale` direction
  where
    epsilon = 10e-6
    qijk = Quat 0 i j k  -- Called v, above
    theta = qmodulus qijk
    direction = if theta < epsilon then qUnitR else (cos_theta `qadd` sin_theta_vhat)
    qijkhat = qnormalize qijk  -- (1/theta) `scale` qijk
    cos_theta = (cos theta) `qscale` qUnitR
    sin_theta_vhat = (sin theta) `qscale` qijkhat

-- How to find the log of a quaternion:
-- Given a non-zero quaternion q, q = |q| * qhat, where qhat is a unit quaternion.
-- qhat = (cos theta) + (sin theta) * qijkhat = exp(qijkhat * theta),
--     where qijkhat is a pure imaginary unit quaternion.
-- log(qhat) = qijkhat * theta
-- So log(q) = log(|q|) + qijkhat * theta
qlog :: Quat -> Quat  -- TODO: Ensure that we're not taking the log of the cut point (-1,0,0,0).
qlog q@(Quat r i j k) = if qnorm < epsilon then error "Error: Cannot take log of zero" else result
  where
    epsilon = 10e-6
    qnorm = qmodulus q
    qhat = (1/qnorm) `qscale` q
    theta = acos $ qGetR qhat
    qijk = qimag qhat
    qijknorm = qmodulus qijk
    sign_r = signum r
    qijkhat = if qijknorm < epsilon then (sign_r `qscale` qUnitR) else qnormalize $ qimag qhat  -- q is near real axis
    (_, res_i, res_j, res_k) = qcoords $ theta `qscale` qijkhat
    result = Quat (log qnorm) res_i res_j res_k

qpow :: Quat -> Double -> Quat
qpow q x = if (x == 0) then qUnitR else qexp(x `qscale` qlog(q))

qslerp :: Double -> Quat -> Quat -> Quat
qslerp t q0 q1 = q0 `qmult` (qpow toq1 t)
  where toq1 = (qinv q0) `qmult` q1
