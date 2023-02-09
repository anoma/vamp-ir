// SPDX-License-Identifier: MIT OR Apache-2.0
pragma solidity >=0.5.0;
pragma experimental ABIEncoderV2;

import {PairingsBn254} from './pairings.sol';

library ZkGarage {
    using PairingsBn254 for PairingsBn254.G1Point;
    using PairingsBn254 for PairingsBn254.G2Point;
    using PairingsBn254 for PairingsBn254.Fr;

    struct VerifierKey {
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

    struct VerifierState {
        PairingsBn254.Fr alpha;
        PairingsBn254.Fr beta;
        PairingsBn254.Fr gamma;
        PairingsBn254.Fr delta;
        PairingsBn254.Fr epsilon;
        PairingsBn254.Fr zeta;
        PairingsBn254.Fr range_sep_challenge;
        PairingsBn254.Fr logic_sep_challenge;
        PairingsBn254.Fr fixed_base_sep_challenge;
        PairingsBn254.Fr var_base_sep_challenge;
        PairingsBn254.Fr lookup_sep_challenge;
        PairingsBn254.Fr z_challenge;
        PairingsBn254.Fr z_h_eval;
        PairingsBn254.Fr l1_eval;
        PairingsBn254.Fr pi_eval;
        PairingsBn254.Fr r0;
        PairingsBn254.Fr vanishing_poly_eval;
        PairingsBn254.Fr z_challenge_to_n;
        PairingsBn254.Fr aw_challenge;
        PairingsBn254.Fr saw_challenge;
        PairingsBn254.G1Point[] aw_commits;
        PairingsBn254.Fr[] aw_evals;
        PairingsBn254.Fr[] aw_separators;
        PairingsBn254.Fr[] saw_evals;
        PairingsBn254.Fr[] saw_separators;
    }
}

