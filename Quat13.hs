module Quat13 (
    QVec,
    qvadd, qvsub, qvdot, qvcross, qvscale, qvneg,
    qvZero,
    qvmodulus2, qvmodulus,

    Quat13, showAsSum, showAsTup,
    qUnitR, qUnitI, qUnitJ, qUnitK, qZero, qNegOne,
    qGetR, qGetI, qGetJ, qGetK,
    qconj, qmodulus2, qmodulus, qnormalize, qcoords,
    qscale, qinv, qadd, qsub, qmult,
    qexp, qlog, qpow, qslerp
) where

import Debug.Trace
import Text.Printf (printf)

-- ========================================
-- Compare with mrd's implementation
-- ========================================
type QVec = (Double, Double, Double)

qvadd :: QVec -> QVec -> QVec
qvadd (i1, j1, k1) (i2, j2, k2) = (i1 + i2, j1 + j2, k1 + k2)

qvsub :: QVec -> QVec -> QVec
qvsub (i1, j1, k1) (i2, j2, k2) = (i1 - i2, j1 - j2, k1 - k2)

qvdot :: QVec -> QVec -> Double
qvdot (a, b, c) (x, y, z) = a*x + b*y + c*z

qvcross :: QVec -> QVec -> QVec
qvcross (a, b, c) (x, y, z) = (b*z - c*y, c*x - a*z, a*y - b*x)

qvscale :: Double -> QVec -> QVec
qvscale s (i, j, k) = (s*i, s*j, s*k)

qvneg :: QVec -> QVec
qvneg (i, j, k) = (-i, -j, -k)

qvZero :: QVec
qvZero = (0.0, 0.0, 0.0)

qvmodulus2, qvmodulus :: QVec -> Double
qvmodulus2 (a, b, c) = a**2 + b**2 + c**2
qvmodulus qv = sqrt $ qvmodulus2 qv

qvnormalize :: QVec -> QVec
qvnormalize qv = (1.0 / (qvmodulus qv)) `qvscale` qv

-- ========================================
--   https://github.com/mrd/haskell-quaternion/blob/master/Data/Quaternion.hs

data Quat13 = Quat13 { real::Double, imag::(Double, Double, Double) }
    deriving Eq

-- String functions
showAsSum :: Quat13 -> String
showAsSum (Quat13 {real=r, imag=(i, j, k)}) = f_r <> post_r <> f_i <> post_i <> f_j <> post_j <> f_k <> post_k
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

showAsTup :: Quat13 -> String
showAsTup (Quat13 {real=r, imag=(i, j, k)}) = lp <> f_r <> c <> f_i <> c <> f_j <> c <> f_k <> rp
  where
    eps = 10e-6
    f_r = if (abs(r - fromIntegral(round r)) < eps) then (printf "%.0f" r) else (printf "%.6f" r)
    f_i = if (abs(i - fromIntegral(round i)) < eps) then (printf "%.0f" i) else (printf "%.6f" i)
    f_j = if (abs(j - fromIntegral(round j)) < eps) then (printf "%.0f" j) else (printf "%.6f" j)
    f_k = if (abs(k - fromIntegral(round k)) < eps) then (printf "%.0f" k) else (printf "%.6f" k)
    lp = "("
    c = ", "
    rp = ")"

instance Show Quat13 where
    show q = showAsSum q

qUnitR, qUnitI, qUnitJ, qUnitK :: Quat13
qUnitR = Quat13 { real=1.0, imag=(0.0, 0.0, 0.0) }
qUnitI = Quat13 { real=0.0, imag=(1.0, 0.0, 0.0) }
qUnitJ = Quat13 { real=0.0, imag=(0.0, 1.0, 0.0) }
qUnitK = Quat13 { real=0.0, imag=(0.0, 0.0, 1.0) }

qZero, qNegOne :: Quat13
qZero  = Quat13 { real=  0.0,  imag=(0.0, 0.0, 0.0) }
qNegOne= Quat13 { real=(-1.0), imag=(0.0, 0.0, 0.0) }

-- Unary operators
qGetR, qGetI, qGetJ, qGetK :: Quat13 -> Double
qGetR (Quat13 { real=r, imag=(_, _, _) }) = r
qGetI (Quat13 { real=_, imag=(i, _, _) }) = i
qGetJ (Quat13 { real=_, imag=(_, j, _) }) = j
qGetK (Quat13 { real=_, imag=(_, _, k) }) = k

qconj :: Quat13 -> Quat13
qconj (Quat13 { real=r, imag=qv }) = Quat13 { real=r, imag=qvneg qv }

-- ========================================

qmodulus2, qmodulus :: Quat13 -> Double
qmodulus2 (Quat13 { real=r, imag=(i, j, k) }) = r**2 + i**2 + j**2 + k**2
qmodulus q = sqrt $ qmodulus2 q

qnormalize :: Quat13 -> Quat13
qnormalize q = (1.0 / (qmodulus q)) `qscale` q

qcoords :: Quat13 -> (Double, Double, Double, Double)
qcoords (Quat13 { real=r, imag=(i, j, k) }) = (r, i, j, k)

-- Binary operators
qscale :: Double -> Quat13 -> Quat13
qscale s (Quat13 { real=r, imag=im }) = Quat13 { real=(s*r), imag=s `qvscale` im }

qinv :: Quat13 -> Quat13
qinv q = (1.0 / (qmodulus2 q)) `qscale` (qconj q)

qadd :: Quat13 -> Quat13 -> Quat13
qadd (Quat13 { real=r1, imag=qv1 }) (Quat13 { real=r2, imag=qv2 }) = result
  where
    result = Quat13 { real=r1+r2, imag=qv1 `qvadd` qv2 }

qsub :: Quat13 -> Quat13 -> Quat13
qsub (Quat13 { real=r1, imag=qv1 }) (Quat13 { real=r2, imag=qv2 }) = result
  where
    result = Quat13 { real=r1-r2, imag=qv1 `qvsub` qv2 }

qmult :: Quat13 -> Quat13 -> Quat13
qmult (Quat13 { real=r1, imag=qv1 }) (Quat13 {real=r2, imag=qv2 }) = result
  where
    res_real = r1*r2 - qv1 `qvdot` qv2
    res_imag= (r1 `qvscale` qv2) `qvadd` (r2 `qvscale` qv1) `qvadd` (qv1 `qvcross` qv2)
    result = Quat13 { real=res_real, imag=res_imag }

-- Exponentiation, Log, and Slerp functions

-- How to find the exp of a quaternion:
-- Given an arbitrary quaternion q, q = r + v, for some r in R, and some purely imaginary quaternion, v.
-- Then exp(v) = cos theta + sin theta * vhat, where theta = |v| and vhat = v / |v|
-- So exp(q) = exp(r) * (cos theta + sin theta * vhat)
qexp :: Quat13 -> Quat13
qexp q@(Quat13 { real=r, imag=(i, j, k) }) = exp(r) `qscale` direction
  where
    epsilon = 10e-6
    qijk = Quat13 { real=0, imag=(i, j, k) }  -- Called v, above
    theta = qmodulus qijk
    direction = if theta < epsilon then qUnitR else (cos_theta `qadd` sin_theta_vhat)
    qijkhat = qnormalize qijk  -- (1.0 / theta) `scale` qijk
    cos_theta = (cos theta) `qscale` qUnitR
    sin_theta_vhat = (sin theta) `qscale` qijkhat

-- How to find the log of a quaternion:
-- Given a non-zero quaternion q, q = |q| * qhat, where qhat is a unit quaternion.
-- qhat = (cos theta) + (sin theta) * qijkhat = exp(qijkhat * theta),
--     where qijkhat is a pure imaginary unit quaternion.
-- log(qhat) = qijkhat * theta
-- So log(q) = log(|q|) + qijkhat * theta
qlog :: Quat13 -> Quat13  -- TODO: Ensure that we're not taking the log of the cut point (-1,0,0,0).
qlog q@(Quat13 {real=r, imag=qv }) = if qnorm < epsilon then error "Error: Cannot take log of zero" else result
  where
    epsilon = 10e-6
    qnorm = qmodulus q
    qhat = (1.0 / qnorm) `qscale` q
    theta = acos $ qGetR qhat
    qvnorm = qvmodulus qv
    sign_r = signum r
    qvhat = if qvnorm < epsilon then qvZero else qvnormalize qv  -- q is near real axis NNNNN
    result = Quat13 { real=(log qnorm), imag=theta `qvscale` qvhat }

qpow :: Quat13 -> Double -> Quat13
qpow q x = if (x == 0) then qUnitR else qexp(x `qscale` qlog(q))

qslerp :: Double -> Quat13 -> Quat13 -> Quat13
qslerp t q0 q1 = q0 `qmult` (qpow toq1 t)
  where toq1 = (qinv q0) `qmult` q1
