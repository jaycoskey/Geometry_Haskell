-- TODO: Switch to using Test.HUnit

module QuatTest (
    test_quat,
    test_quat_identities,
    test_quat_scaling,
    test_quat_exponentiation,
    test_quat_slerp,
    test_quat_visual
) where

import Quat13

-- ========================================
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _ x = x

-- ========================================
test_quat = do
    test_quat_identities
    test_quat_scaling
    test_quat_exponentiation
    test_quat_slerp
    test_quat_visual

test_quat_exponentiation = do
    putStrLn "Entering test_quat_exponentiation"
    putStrLn $ assert (qpow qUnitR 2.0 == qUnitR) "\t1.0**2 == 1.0"
    let eps = 10e-6
    let e_pi_i = qexp $ pi `qscale` qUnitI
    let e_pi_j = qexp $ pi `qscale` qUnitJ
    let e_pi_k = qexp $ pi `qscale` qUnitK
    let e_pi_i_gap = qmodulus $ e_pi_i `qsub` qNegOne
    let e_pi_j_gap = qmodulus $ e_pi_j `qsub` qNegOne
    let e_pi_k_gap = qmodulus $ e_pi_k `qsub` qNegOne

    putStrLn $ assert (e_pi_i_gap < eps) "\texp(i*pi) == -1"
    putStrLn $ assert (e_pi_j_gap < eps) "\texp(j*pi) == -1"
    putStrLn $ assert (e_pi_k_gap < eps) "\texp(k*pi) == -1"

test_quat_identities = do
    putStrLn "Entering test_quat_identities"
    putStrLn $ assert (qUnitR `qmult` qUnitR == qUnitR) "\t1 * 1 == 1"
    putStrLn $ assert (qUnitR `qmult` qUnitI == qUnitI) "\t1 * i == i"
    putStrLn $ assert (qUnitR `qmult` qUnitJ == qUnitJ) "\t1 * j == j"
    putStrLn $ assert (qUnitR `qmult` qUnitK == qUnitK) "\t1 * k == k"
    putStrLn $ assert (qUnitI `qmult` qUnitJ == qUnitK) "\t1 * i == i"

    putStrLn $ assert (qUnitI `qmult` qUnitI == (-1.0) `qscale` qUnitR) "\ti * i == -1"
    putStrLn $ assert (qUnitJ `qmult` qUnitJ == (-1.0) `qscale` qUnitR) "\tj * j == -1"
    putStrLn $ assert (qUnitK `qmult` qUnitK == (-1.0) `qscale` qUnitR) "\tk * k == -1"

    putStrLn $ assert (qUnitI `qmult` qUnitJ == qUnitK) "\ti * j == k"
    putStrLn $ assert (qUnitJ `qmult` qUnitK == qUnitI) "\tj * k == i"
    putStrLn $ assert (qUnitK `qmult` qUnitI == qUnitJ) "\tk * i == j"

    putStrLn $ assert (qUnitJ `qmult` qUnitI == (-1.0) `qscale` qUnitK) "\tj * i == -k"
    putStrLn $ assert (qUnitK `qmult` qUnitJ == (-1.0) `qscale` qUnitI) "\tk * j == -i"
    putStrLn $ assert (qUnitI `qmult` qUnitK == (-1.0) `qscale` qUnitJ) "\ti * k == -j"

test_quat_scaling = do
    let qUnitR_1_4 = (1/4.0) `qscale` qUnitR
    let qUnitR_4 = 4.0 `qscale` qUnitR
    let qUnitI_2 = 2.0 `qscale` qUnitR
    let qUnitJ_3 = 3.0 `qscale` qUnitR
    let qUnitK_6 = 6.0 `qscale` qUnitR

    putStrLn "Entering test_quat_scaling"
    putStrLn $ assert (qUnitR_1_4 `qmult` qUnitR_4 == qUnitR) "\tPassed: Quaternion scaling, part 1"
    putStrLn $ assert (qUnitI_2 `qmult` qUnitJ_3 == qUnitK_6) "\tPassed: Quaternion scaling, part 2"
    putStrLn $ assert (show qUnitR_4 == "4 + 0i + 0j + 0k") "\tPassed: Quaternion scaling, part 3"

    -- Note: No easy way to specify maximum precision without specifying minimum precision (e.g., 0.25).
    putStrLn $ assert (show qUnitR_1_4 == "0.250000 + 0i + 0j + 0k") "\tPassed: Quaternion scaling, part 4"

test_quat_slerp = do
    putStrLn "Entering test_quat_slerp"
    putStrLn $ "\tResult: " ++ (show $ qslerp 0.000000 qUnitI qUnitJ)
    putStrLn $ "\tResult: " ++ (show $ qslerp 0.333333 qUnitI qUnitJ)
    putStrLn $ "\tResult: " ++ (show $ qslerp 0.500000 qUnitI qUnitJ)
    putStrLn $ "\tResult: " ++ (show $ qslerp 0.666666 qUnitI qUnitJ)
    putStrLn $ "\tResult: " ++ (show $ qslerp 1.000000 qUnitI qUnitJ)

test_quat_visual = do
    let log_qUnitR = qlog(qUnitR)
    let exp_qUnitR = qexp(qUnitR)

    let el_qUnitR = qexp(qlog(qUnitR))
    let el_qUnitI = qexp(qlog(qUnitI))
    let el_qUnitJ = qexp(qlog(qUnitJ))
    let el_qUnitK = qexp(qlog(qUnitK))

    let le_qUnitR = qlog(qexp(qUnitR))
    let le_qUnitI = qlog(qexp(qUnitI))
    let le_qUnitJ = qlog(qexp(qUnitJ))
    let le_qUnitK = qlog(qexp(qUnitK))

    putStrLn "Entering test_quat_visual"
    putStrLn $ "\tqUnitR=" ++ (show qUnitR)
    putStrLn $ "\texp(qUnitR)=" ++ (show exp_qUnitR)
    putStrLn $ "\tlog(qUnitR)=" ++ (show log_qUnitR)
    putStrLn ""
    putStrLn $ "\texp(log(qUnitR))=" ++ (show el_qUnitR)
    putStrLn $ "\texp(log(qUnitI))=" ++ (show el_qUnitI)
    putStrLn $ "\texp(log(qUnitJ))=" ++ (show el_qUnitJ)
    putStrLn $ "\texp(log(qUnitK))=" ++ (show el_qUnitK)
    putStrLn ""
    putStrLn $ "\tlog(exp(qUnitR))=" ++ (show le_qUnitR)
    putStrLn $ "\tlog(exp(qUnitI))=" ++ (show le_qUnitI)
    putStrLn $ "\tlog(exp(qUnitJ))=" ++ (show le_qUnitJ)
    putStrLn $ "\tlog(exp(qUnitK))=" ++ (show le_qUnitK)
