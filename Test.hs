import Quat13
import QuatTest


-- TODO: Switch to using Test.HUnit
main = do
    test_quat
    test_quat_identities
    test_quat_scaling
    test_quat_exponentiation
    test_quat_slerp
    test_quat_visual
