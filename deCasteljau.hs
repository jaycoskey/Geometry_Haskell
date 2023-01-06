import Quat
import QuatTest

-- ========================================
-- de Casteljau for quaternion arguments
qdeCasteljau :: Double -> [Quat] -> Quat
qdeCasteljau t [q] = q  -- Termination condition
qdeCasteljau t (q0:qs) = qdeCasteljau t reduced
  where reduced = zipWith (qslerp t) (q0:qs) qs

test_quat_bezier = do
    putStrLn "Entering test_quat_bezier"
    putStrLn $ "\tResult @ 0.0: " ++ (show $ qdeCasteljau 0.0 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.1: " ++ (show $ qdeCasteljau 0.1 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.2: " ++ (show $ qdeCasteljau 0.2 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.3: " ++ (show $ qdeCasteljau 0.3 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.4: " ++ (show $ qdeCasteljau 0.4 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.5: " ++ (show $ qdeCasteljau 0.5 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.6: " ++ (show $ qdeCasteljau 0.6 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.7: " ++ (show $ qdeCasteljau 0.7 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.8: " ++ (show $ qdeCasteljau 0.8 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 0.9: " ++ (show $ qdeCasteljau 0.9 [qUnitR, qUnitI, qUnitJ, qUnitK])
    putStrLn $ "\tResult @ 1.0: " ++ (show $ qdeCasteljau 1.0 [qUnitR, qUnitI, qUnitJ, qUnitK])

-- ========================================

main :: IO()
main = do
    test_quat
    test_quat_bezier
    putStrLn $ "log 1 = " <> (show $ qlog qUnitR)
    putStrLn $ "log I = " <> (show $ qlog qUnitI)
    putStrLn $ "log J = " <> (show $ qlog qUnitJ)
    putStrLn $ "log K = " <> (show $ qlog qUnitK)
    putStrLn ""
    putStrLn $ "exp(i*pi) = " <> (show $ qexp $ pi `qscale` qUnitI)
    putStrLn $ "exp(j*pi) = " <> (show $ qexp $ pi `qscale` qUnitJ)
    putStrLn $ "exp(k*pi) = " <> (show $ qexp $ pi `qscale` qUnitK)
    putStrLn ""
    let l_neg_half = qlog $ (-0.5) `qscale` qUnitR
    let l_neg_2 = qlog $ (-2.0) `qscale` qUnitR
    putStrLn $ "log -0.5 = " <> show l_neg_half
    putStrLn $ "log -2 = " <> show l_neg_2
