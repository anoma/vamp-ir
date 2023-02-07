// SPDX-License-Identifier: MIT OR Apache-2.0
pragma solidity >=0.5.0 <0.8.0;
pragma experimental ABIEncoderV2;

import {PairingsBn254} from './pairings.sol';

library ZkGarage {
    using PairingsBn254 for PairingsBn254.G1Point;
    using PairingsBn254 for PairingsBn254.G2Point;
    using PairingsBn254 for PairingsBn254.Fr;

    struct VerificationKey {
        uint256 domain_size;
        uint256 num_inputs;
        PairingsBn254.Fr omega;
        ArithmeticVerifierKey arith;
        PermutationVerifierKey perm;
        PairingsBn254.Fr k1;
        PairingsBn254.Fr k2;
        PairingsBn254.Fr k3;
        PairingsBn254.G2Point g2_x;
    }

    struct ArithmeticVerifierKey {
        PairingsBn254.G1Point q_m_comm;
        PairingsBn254.G1Point q_l_comm;
        PairingsBn254.G1Point q_r_comm;
        PairingsBn254.G1Point q_o_comm;
        PairingsBn254.G1Point q_4_comm;        
        PairingsBn254.G1Point left_sigma_comm;
        PairingsBn254.G1Point right_sigma_comm;
        PairingsBn254.G1Point out_sigma_comm;
        PairingsBn254.G1Point fourth_sigma_comm;
        PairingsBn254.G1Point q_c_comm;
    }

    struct PermutationVerifierKey {
        PairingsBn254.G1Point left_sigma_comm;
        PairingsBn254.G1Point right_sigma_comm;
        PairingsBn254.G1Point out_sigma_comm;
        PairingsBn254.G1Point fourth_sigma_comm;
    }

    struct Proof {
        PairingsBn254.Fr[] input_values;
        PairingsBn254.G1Point a_comm;
        PairingsBn254.G1Point b_comm;
        PairingsBn254.G1Point c_comm;
        PairingsBn254.G1Point d_comm;
        PairingsBn254.G1Point z_comm;
        PairingsBn254.G1Point f_comm;
        PairingsBn254.G1Point h_1_comm;
        PairingsBn254.G1Point h_2_comm;
        PairingsBn254.G1Point z_2_comm;
        PairingsBn254.G1Point t_1_comm;
        PairingsBn254.G1Point t_2_comm;
        PairingsBn254.G1Point t_3_comm;
        PairingsBn254.G1Point t_4_comm;
        PairingsBn254.G1Point aw_commit;
        PairingsBn254.G1Point saw_commit;
        ProofEvaluations evaluations;
    }

    struct ProofEvaluations {
        WireEvaluations wire_evals;
        PermutationEvaluations perm_evals;
        LookupEvaluations lookup_evals;
        CustomEvaluations custom_evals;
    }

    struct WireEvaluations {
        PairingsBn254.Fr a_eval;
        PairingsBn254.Fr b_eval;
        PairingsBn254.Fr c_eval;
        PairingsBn254.Fr d_eval;
    }

    struct PermutationEvaluations {
        PairingsBn254.Fr left_sigma_eval;
        PairingsBn254.Fr right_sigma_eval;
        PairingsBn254.Fr out_sigma_eval;
        PairingsBn254.Fr permutation_eval;
    }

    struct LookupEvaluations {
        PairingsBn254.Fr q_lookup_eval;
        PairingsBn254.Fr z2_next_eval;
        PairingsBn254.Fr h1_eval;
        PairingsBn254.Fr h1_next_eval;
        PairingsBn254.Fr h2_eval;
        PairingsBn254.Fr f_eval;
        PairingsBn254.Fr table_eval;
        PairingsBn254.Fr table_next_eval;
    }

    struct CustomEvaluations {
        PairingsBn254.Fr[] vals;
    }
}

