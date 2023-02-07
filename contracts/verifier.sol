// Adapted from Matter Labs Plonk verifier at https://github.com/matter-labs/zksync/blob/master/contracts/contracts/PlonkCore.sol
// SPDX-License-Identifier: MIT OR Apache-2.0

pragma solidity >=0.5.0 <0.8.0;
pragma experimental ABIEncoderV2;

import {ZkGarage} from './data_structures.sol';
import {PairingsBn254} from './pairings.sol';
import {TranscriptLibrary} from './transcript.sol';

contract Plonk4ArithVerifier {
    uint256 constant r_mod = 21888242871839275222246405745257275088548364400416034343698204186575808495617;

    using PairingsBn254 for PairingsBn254.G1Point;
    using PairingsBn254 for PairingsBn254.G2Point;
    using PairingsBn254 for PairingsBn254.Fr;

    using ZkGarage for ZkGarage.Proof;
    using ZkGarage for ZkGarage.VerificationKey;

    using TranscriptLibrary for TranscriptLibrary.Transcript;

    function evaluate_lagrange_poly_out_of_domain(
        uint256 poly_num,
        uint256 domain_size,
        PairingsBn254.Fr memory omega,
        PairingsBn254.Fr memory at
    ) internal view returns (PairingsBn254.Fr memory res) {
        require(poly_num < domain_size);
        PairingsBn254.Fr memory one = PairingsBn254.new_fr(1);
        PairingsBn254.Fr memory omega_power = omega.pow(poly_num);
        res = at.pow(domain_size);
        res.sub_assign(one);
        require(res.value != 0); // Vanishing polynomial can not be zero at point `at`
        res.mul_assign(omega_power);

        PairingsBn254.Fr memory den = PairingsBn254.copy(at);
        den.sub_assign(omega_power);
        den.mul_assign(PairingsBn254.new_fr(domain_size));

        den = den.inverse();

        res.mul_assign(den);
    }

    function batch_evaluate_lagrange_poly_out_of_domain(
        uint256[] memory poly_nums,
        uint256 domain_size,
        PairingsBn254.Fr memory omega,
        PairingsBn254.Fr memory at
    ) internal view returns (PairingsBn254.Fr[] memory res) {
        PairingsBn254.Fr memory one = PairingsBn254.new_fr(1);
        PairingsBn254.Fr memory tmp_1 = PairingsBn254.new_fr(0);
        PairingsBn254.Fr memory tmp_2 = PairingsBn254.new_fr(domain_size);
        PairingsBn254.Fr memory vanishing_at_z = at.pow(domain_size);
        vanishing_at_z.sub_assign(one);
        // we can not have random point z be in domain
        require(vanishing_at_z.value != 0);
        PairingsBn254.Fr[] memory nums = new PairingsBn254.Fr[](poly_nums.length);
        PairingsBn254.Fr[] memory dens = new PairingsBn254.Fr[](poly_nums.length);
        // numerators in a form omega^i * (z^n - 1)
        // denoms in a form (z - omega^i) * N
        for (uint256 i = 0; i < poly_nums.length; i++) {
            tmp_1 = omega.pow(poly_nums[i]); // power of omega
            nums[i].assign(vanishing_at_z);
            nums[i].mul_assign(tmp_1);

            dens[i].assign(at); // (X - omega^i) * N
            dens[i].sub_assign(tmp_1);
            dens[i].mul_assign(tmp_2); // mul by domain size
        }

        PairingsBn254.Fr[] memory partial_products = new PairingsBn254.Fr[](poly_nums.length);
        partial_products[0].assign(PairingsBn254.new_fr(1));
        for (uint256 i = 1; i < dens.length - 1; i++) {
            partial_products[i].assign(dens[i - 1]);
            partial_products[i].mul_assign(dens[i]);
        }

        tmp_2.assign(partial_products[partial_products.length - 1]);
        tmp_2.mul_assign(dens[dens.length - 1]);
        tmp_2 = tmp_2.inverse(); // tmp_2 contains a^-1 * b^-1 (with! the last one)

        for (uint256 i = dens.length - 1; i < dens.length; i--) {
            dens[i].assign(tmp_2); // all inversed
            dens[i].mul_assign(partial_products[i]); // clear lowest terms
            tmp_2.mul_assign(dens[i]);
        }

        for (uint256 i = 0; i < nums.length; i++) {
            nums[i].mul_assign(dens[i]);
        }

        return nums;
    }

    function evaluate_vanishing(uint256 domain_size, PairingsBn254.Fr memory at)
        internal
        view
        returns (PairingsBn254.Fr memory res)
    {
        res = at.pow(domain_size);
        res.sub_assign(PairingsBn254.new_fr(1));
    }

    function multiscalar_mul(PairingsBn254.Fr[] memory scalars, PairingsBn254.G1Point[] memory points)
        internal
        view
        returns (PairingsBn254.G1Point memory res)
    {
        res = PairingsBn254.P1();
        for (uint256 i = 0; i < scalars.length; i++) {
            PairingsBn254.G1Point memory tmp = points[i].point_mul(scalars[i]);
            res.point_add_assign(tmp);
        }
    }

    function barycentric_eval(
        PairingsBn254.Fr[] memory evaluations,
        PairingsBn254.Fr memory point,
        uint256 domain_size,
        PairingsBn254.Fr memory omega
    )
        internal
        view
        returns (PairingsBn254.Fr memory res)
    {
        PairingsBn254.Fr memory domain_size_as_fr = PairingsBn254.new_fr(domain_size);
        PairingsBn254.Fr memory group_gen_inv = omega.inverse();
        PairingsBn254.Fr memory numerator = evaluate_vanishing(domain_size, point);
        numerator.mul_assign(domain_size_as_fr.inverse());

        PairingsBn254.Fr[] memory denominators = new PairingsBn254.Fr[](evaluations.length);

        for (uint256 i = 0; i < evaluations.length; i++) {
            PairingsBn254.Fr memory den = group_gen_inv.pow(i);
            den.mul_assign(point);
            den.sub_assign(PairingsBn254.new_fr(1));
            denominators[i] = den.inverse();
        }

        for (uint256 i = 0; i < denominators.length; i++) {
            PairingsBn254.Fr memory tmp = denominators[i];
            if (evaluations[i].value != 0) {
                tmp.mul_assign(evaluations[i]);
                res.add_assign(tmp);
            }
        }

        res.mul_assign(numerator);

        return res;
    }

    function verify(
        ZkGarage.Proof memory proof,
        ZkGarage.VerificationKey memory vk
    ) internal view returns (bool) {

        PairingsBn254.Fr memory domain_size_as_fr = PairingsBn254.new_fr(vk.domain_size);

        require(proof.input_values.length == vk.num_inputs);
        require(vk.num_inputs >= 1);
        TranscriptLibrary.Transcript memory transcript = TranscriptLibrary.new_transcript();

        // Append Public Inputs to the transcript
        for (uint256 i = 0; i < vk.num_inputs; i++) {
            transcript.update_with_fr(proof.input_values[i]);
        }

        // Add commitment to witness polynomials to transcript
        transcript.update_with_g1(proof.a_comm);
        transcript.update_with_g1(proof.b_comm);
        transcript.update_with_g1(proof.c_comm);
        transcript.update_with_g1(proof.d_comm);

        // Compute table compression challenge `zeta`.
        PairingsBn254.Fr memory zeta = transcript.get_challenge();
        transcript.update_with_fr(zeta);     

        // Add commitmtent to f_poly to transcript
        transcript.update_with_g1(proof.f_comm);

        // Add commitment to h polynomials to transcript
        transcript.update_with_g1(proof.h_1_comm);
        transcript.update_with_g1(proof.h_2_comm);

        // Compute permutation challenge `beta`.
        PairingsBn254.Fr memory beta = transcript.get_challenge();
        transcript.update_with_fr(beta); 
        
        // Compute permutation challenge `gamma`.
        PairingsBn254.Fr memory gamma = transcript.get_challenge();
        transcript.update_with_fr(gamma); 

        // Compute permutation challenge `delta`.
        PairingsBn254.Fr memory delta = transcript.get_challenge();
        transcript.update_with_fr(delta); 
        
        // Compute permutation challenge `epsilon`.
        PairingsBn254.Fr memory epsilon = transcript.get_challenge();
        transcript.update_with_fr(epsilon); 

        // Challenges must be different
        require(beta.value != gamma.value);
        require(beta.value != delta.value);
        require(beta.value != epsilon.value); 
        require(gamma.value != delta.value); 
        require(gamma.value != epsilon.value); 
        require(delta.value != epsilon.value);

        // Add commitment to permutation polynomial to transcript
        transcript.update_with_g1(proof.z_comm);

        // Compute quotient challenge `alpha`
        PairingsBn254.Fr memory alpha = transcript.get_challenge();
        transcript.update_with_fr(alpha);

        // Compute custom gate separator challenges
        PairingsBn254.Fr memory range_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(range_sep_challenge);

        PairingsBn254.Fr memory logic_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(logic_sep_challenge);

        PairingsBn254.Fr memory fixed_base_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(fixed_base_sep_challenge);

        PairingsBn254.Fr memory var_base_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(var_base_sep_challenge);

        PairingsBn254.Fr memory lookup_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(lookup_sep_challenge);

        // Add commitment to quotient polynomial to the transcript
        transcript.update_with_g1(proof.t_1_comm);
        transcript.update_with_g1(proof.t_2_comm);
        transcript.update_with_g1(proof.t_3_comm);
        transcript.update_with_g1(proof.t_4_comm);

        // Compute evaluation point challenge `z_challenge`
        PairingsBn254.Fr memory z_challenge = transcript.get_challenge();
        transcript.update_with_fr(z_challenge);

        // Compute zero polynomial evaluated at `z_challenge`
        PairingsBn254.Fr memory z_h_eval = z_challenge.pow(vk.domain_size);
        z_h_eval.sub_assign(PairingsBn254.new_fr(1));

        // Compute first lagrange polynomial evaluated at `z_challenge`
        PairingsBn254.Fr memory den = z_challenge;

        den.sub_assign(PairingsBn254.new_fr(1));
        den.mul_assign(domain_size_as_fr);

        PairingsBn254.Fr memory l1_eval = z_h_eval;
        l1_eval.mul_assign(den.inverse());

        // Compute `r0`, the constant term of the linearization polynomial
        PairingsBn254.Fr memory pi_eval = barycentric_eval(proof.input_values, z_challenge, vk.domain_size, vk.omega);
        PairingsBn254.Fr memory alpha_sq = alpha;
        alpha_sq.mul_assign(alpha);
        PairingsBn254.Fr memory lookup_sep_challenge_sq = lookup_sep_challenge;
        lookup_sep_challenge_sq.mul_assign(lookup_sep_challenge);

        PairingsBn254.Fr memory lookup_sep_challenge_cu = lookup_sep_challenge_sq;
        lookup_sep_challenge_cu.mul_assign(lookup_sep_challenge);

        // a + beta * sigma_1 + gamma
        PairingsBn254.Fr memory beta_sig1 = proof.evaluations.perm_evals.left_sigma_eval;
        beta_sig1.mul_assign(beta);

        PairingsBn254.Fr memory b_0 = proof.evaluations.wire_evals.a_eval;
        b_0.add_assign(beta_sig1);
        b_0.add_assign(gamma);

        // a + beta * sigma_2 + gamma
        PairingsBn254.Fr memory beta_sig2 = proof.evaluations.perm_evals.right_sigma_eval;
        beta_sig2.mul_assign(beta);

        PairingsBn254.Fr memory b_1 = proof.evaluations.wire_evals.b_eval;
        b_1.add_assign(beta_sig2);
        b_1.add_assign(gamma);

        // a + beta * sigma_3 + gamma
        PairingsBn254.Fr memory beta_sig3 = proof.evaluations.perm_evals.out_sigma_eval;
        beta_sig3.mul_assign(beta);

        PairingsBn254.Fr memory b_2 = proof.evaluations.wire_evals.c_eval;
        b_2.add_assign(beta_sig3);
        b_2.add_assign(gamma);

        // ((d + gamma) * z_hat) * alpha
        PairingsBn254.Fr memory b_3 = proof.evaluations.wire_evals.d_eval;
        b_3.add_assign(gamma);
        b_3.mul_assign(proof.evaluations.perm_evals.permutation_eval);
        b_3.mul_assign(alpha);

        PairingsBn254.Fr memory tmp_b = b_0;
        tmp_b.mul_assign(b_1);
        tmp_b.mul_assign(b_2);
        tmp_b.mul_assign(b_3);

        // l_1(z) * alpha^2
        PairingsBn254.Fr memory tmp_c = l1_eval;
        tmp_c.mul_assign(alpha_sq);

        PairingsBn254.Fr memory epsilon_one_plus_delta = PairingsBn254.new_fr(1);
        epsilon_one_plus_delta.add_assign(delta);
        epsilon_one_plus_delta.mul_assign(epsilon);

        PairingsBn254.Fr memory tmp_d_0 = lookup_sep_challenge_sq;
        tmp_d_0.mul_assign(proof.evaluations.lookup_evals.z2_next_eval);

        PairingsBn254.Fr memory tmp_d_1 = epsilon_one_plus_delta;
        tmp_d_1.add_assign(delta);
        tmp_d_1.add_assign(proof.evaluations.lookup_evals.h2_eval);

        PairingsBn254.Fr memory tmp_d_2 = delta;
        tmp_d_2.mul_assign(proof.evaluations.lookup_evals.h1_next_eval);
        tmp_d_2.add_assign(epsilon_one_plus_delta);
        tmp_d_2.add_assign(proof.evaluations.lookup_evals.h2_eval);
        
        PairingsBn254.Fr memory tmp_d = tmp_d_0;
        tmp_d.mul_assign(tmp_d_1);
        tmp_d.mul_assign(tmp_d_2);

        PairingsBn254.Fr memory tmp_e = lookup_sep_challenge_cu;
        tmp_e.mul_assign(l1_eval);

        // Return r_0
        PairingsBn254.Fr memory r0 = pi_eval;
        r0.sub_assign(tmp_b);
        r0.sub_assign(tmp_c);
        r0.sub_assign(tmp_d);
        r0.sub_assign(tmp_e);

        // Add evaluations to transcript
        transcript.update_with_fr(proof.evaluations.wire_evals.a_eval);
        transcript.update_with_fr(proof.evaluations.wire_evals.b_eval);
        transcript.update_with_fr(proof.evaluations.wire_evals.c_eval);
        transcript.update_with_fr(proof.evaluations.wire_evals.d_eval);

        transcript.update_with_fr(proof.evaluations.perm_evals.left_sigma_eval);
        transcript.update_with_fr(proof.evaluations.perm_evals.right_sigma_eval);
        transcript.update_with_fr(proof.evaluations.perm_evals.out_sigma_eval);
        transcript.update_with_fr(proof.evaluations.perm_evals.permutation_eval);

        transcript.update_with_fr(proof.evaluations.lookup_evals.f_eval);
        transcript.update_with_fr(proof.evaluations.lookup_evals.q_lookup_eval);
        transcript.update_with_fr(proof.evaluations.lookup_evals.z2_next_eval);
        transcript.update_with_fr(proof.evaluations.lookup_evals.h1_eval);
        transcript.update_with_fr(proof.evaluations.lookup_evals.h1_next_eval);
        transcript.update_with_fr(proof.evaluations.lookup_evals.h2_eval);

        for (uint256 i = 0; i < vk.num_inputs; i++) {
            transcript.update_with_fr(proof.evaluations.custom_evals.vals[i]);
        }

        // Compute commitment to linearization polynomial
        PairingsBn254.Fr[] memory scalars = new PairingsBn254.Fr[](12);
        PairingsBn254.G1Point[] memory points = new PairingsBn254.G1Point[](12);

        // Arithmetic part of linearization polynomial
        PairingsBn254.Fr memory q_m_eval = proof.evaluations.wire_evals.a_eval;
        q_m_eval.mul_assign(proof.evaluations.wire_evals.b_eval);

        scalars[0] = q_m_eval;
        points[0] = vk.arith.q_m_comm;

        scalars[1] = proof.evaluations.wire_evals.a_eval;
        points[1] = vk.arith.q_l_comm;

        scalars[2] = proof.evaluations.wire_evals.b_eval;
        points[2] = vk.arith.q_r_comm;

        scalars[3] = proof.evaluations.wire_evals.c_eval;
        points[3] = vk.arith.q_o_comm;

        scalars[4] = proof.evaluations.wire_evals.d_eval;
        points[4] = vk.arith.q_4_comm;

        scalars[5] = PairingsBn254.new_fr(1);
        points[5] = vk.arith.q_c_comm;

        // Permutation part of linearization polynomial

        // (a_eval + beta * z + gamma)(b_eval + beta * z * k1 +
        // gamma)(c_eval + beta * k2 * z + gamma)(d_eval + beta
        // * k3 * z + gamma) * alpha

        PairingsBn254.Fr memory beta_z = beta;
        beta_z.mul_assign(z_challenge);

        PairingsBn254.Fr memory q_0 = proof.evaluations.wire_evals.a_eval;
        q_0.add_assign(beta_z);
        q_0.add_assign(gamma);

        PairingsBn254.Fr memory beta_k1_z = beta;
        beta_k1_z.mul_assign(vk.k1);
        beta_k1_z.mul_assign(z_challenge);

        PairingsBn254.Fr memory q_1 = proof.evaluations.wire_evals.b_eval;
        q_1.add_assign(beta_k1_z);
        q_1.add_assign(gamma);

        PairingsBn254.Fr memory beta_k2_z = beta;
        beta_k2_z.mul_assign(vk.k2);
        beta_k2_z.mul_assign(z_challenge);

        PairingsBn254.Fr memory q_2 = proof.evaluations.wire_evals.c_eval;
        q_2.add_assign(beta_k2_z);
        q_2.add_assign(gamma);

        PairingsBn254.Fr memory beta_k3_z = beta;
        beta_k3_z.mul_assign(vk.k3);
        beta_k3_z.mul_assign(z_challenge);

        PairingsBn254.Fr memory q_3 = proof.evaluations.wire_evals.d_eval;
        q_3.add_assign(beta_k3_z);
        q_3.add_assign(gamma);

        PairingsBn254.Fr memory x = q_0;
        x.mul_assign(q_1);
        x.mul_assign(q_2);
        x.mul_assign(q_3);

        // l1(z) * alpha^2
        PairingsBn254.Fr memory r = l1_eval;
        r.mul_assign(alpha_sq);

        x.add_assign(r);

        scalars[6] = x;
        points[6] = proof.z_comm;

        // -(a_eval + beta * sigma_1_eval + gamma)(b_eval + beta *
        // sigma_2_eval + gamma)(c_eval + beta * sigma_3_eval +
        // gamma) * alpha^2

        PairingsBn254.Fr memory beta_sigma_1 = beta;
        beta_sigma_1.mul_assign(proof.evaluations.perm_evals.left_sigma_eval);

        PairingsBn254.Fr memory q2_0 = proof.evaluations.wire_evals.a_eval;
        q2_0.add_assign(beta_sigma_1);
        q2_0.add_assign(gamma);

        PairingsBn254.Fr memory beta_sigma_2 = beta;
        beta_sigma_2.mul_assign(proof.evaluations.perm_evals.right_sigma_eval);

        PairingsBn254.Fr memory q2_1 = proof.evaluations.wire_evals.b_eval;
        q2_1.add_assign(beta_sigma_2);
        q2_1.add_assign(gamma);

        PairingsBn254.Fr memory beta_sigma_3 = beta;
        beta_sigma_3.mul_assign(proof.evaluations.perm_evals.out_sigma_eval);

        PairingsBn254.Fr memory q2_2 = proof.evaluations.wire_evals.c_eval;
        q2_2.add_assign(beta_sigma_3);
        q2_2.add_assign(gamma);

        PairingsBn254.Fr memory q2_3 = beta;
        q2_3.mul_assign(proof.evaluations.perm_evals.permutation_eval);
        q2_3.mul_assign(alpha);

        PairingsBn254.Fr memory y = PairingsBn254.new_fr(0);
        y.sub_assign(q2_0);
        y.mul_assign(q2_1);
        y.mul_assign(q2_2);
        y.mul_assign(q2_3);

        scalars[7] = y;
        points[7] = vk.perm.fourth_sigma_comm;

        // Second part
        PairingsBn254.Fr memory vanishing_poly_eval = evaluate_vanishing(vk.domain_size, z_challenge);
        // z_challenge ^ n
        PairingsBn254.Fr memory z_challenge_to_n = vanishing_poly_eval;
        z_challenge_to_n.add_assign(PairingsBn254.new_fr(1));

        PairingsBn254.Fr memory t_1_scalar = PairingsBn254.new_fr(0);
        t_1_scalar.sub_assign(vanishing_poly_eval);

        PairingsBn254.Fr memory t_2_scalar = t_1_scalar;
        t_2_scalar.mul_assign(z_challenge_to_n);

        PairingsBn254.Fr memory t_3_scalar = t_2_scalar;
        t_3_scalar.mul_assign(z_challenge_to_n);

        PairingsBn254.Fr memory t_4_scalar = t_3_scalar;
        t_4_scalar.mul_assign(z_challenge_to_n);

        scalars[8] = t_1_scalar;
        scalars[9] = t_2_scalar;
        scalars[10] = t_3_scalar;
        scalars[11] = t_4_scalar;

        points[8] = proof.t_1_comm;
        points[9] = proof.t_2_comm;
        points[10] = proof.t_3_comm;
        points[11] = proof.t_4_comm;

        require(scalars.length == points.length);

        PairingsBn254.G1Point memory lin_comm = multiscalar_mul(scalars, points);

        PairingsBn254.Fr memory aw_challenge = transcript.get_challenge();

        PairingsBn254.G1Point[] memory aw_commits = new PairingsBn254.G1Point[](8);
        aw_commits[0] = lin_comm;
        aw_commits[1] = vk.perm.left_sigma_comm;
        aw_commits[2] = vk.perm.right_sigma_comm;
        aw_commits[3] = vk.perm.out_sigma_comm;
        aw_commits[4] = proof.a_comm;
        aw_commits[5] = proof.b_comm;
        aw_commits[6] = proof.c_comm;
        aw_commits[7] = proof.d_comm;

        PairingsBn254.Fr memory neg_r0 = PairingsBn254.new_fr(0);
        neg_r0.sub_assign(r0);

        PairingsBn254.Fr[] memory aw_evals = new PairingsBn254.Fr[](8);
        aw_evals[0] = neg_r0;
        aw_evals[1] = proof.evaluations.perm_evals.left_sigma_eval;
        aw_evals[2] = proof.evaluations.perm_evals.right_sigma_eval;
        aw_evals[3] = proof.evaluations.perm_evals.out_sigma_eval;
        aw_evals[4] = proof.evaluations.wire_evals.a_eval;
        aw_evals[5] = proof.evaluations.wire_evals.b_eval;
        aw_evals[6] = proof.evaluations.wire_evals.c_eval;
        aw_evals[7] = proof.evaluations.wire_evals.d_eval;

        PairingsBn254.Fr memory saw_challenge = transcript.get_challenge();

        PairingsBn254.G1Point[] memory saw_commits = new PairingsBn254.G1Point[](1);
        saw_commits[0] = proof.z_comm;

        PairingsBn254.Fr[] memory saw_evals = new PairingsBn254.Fr[](1);
        saw_evals[0] = proof.evaluations.perm_evals.permutation_eval;

        PairingsBn254.Fr[] memory aw_separators = new PairingsBn254.Fr[](aw_evals.length);
        
        aw_separators[0] = PairingsBn254.new_fr(1);

        for (uint256 i = 1; i < aw_evals.length-1; i++) {
            aw_separators[i] = aw_separators[i-1];
            aw_separators[i].mul_assign(aw_challenge);
        }

        PairingsBn254.Fr[] memory saw_separators = new PairingsBn254.Fr[](saw_evals.length);
        
        saw_separators[0] = saw_challenge;

        for (uint256 i = 1; i < saw_evals.length-1; i++) {
            saw_separators[i] = saw_separators[i-1];
            saw_separators[i].mul_assign(saw_challenge);
        }

        PairingsBn254.G1Point memory f = multiscalar_mul(aw_separators, aw_commits);
        PairingsBn254.G1Point memory e = PairingsBn254.P1();
        PairingsBn254.Fr memory e_scalar = PairingsBn254.new_fr(1);

        for (uint256 i = 0; i < aw_evals.length; i++) {
            PairingsBn254.Fr memory tmp = aw_evals[i];
            tmp.mul_assign(aw_separators[i]);
            e_scalar.add_assign(tmp);
        }

        for (uint256 i = 0; i < saw_evals.length; i++) {
            PairingsBn254.Fr memory tmp = saw_evals[i];
            tmp.mul_assign(saw_separators[i]);
            e_scalar.add_assign(tmp);
        }

        e.point_mul_assign(e_scalar);

        PairingsBn254.G1Point[] memory a_commits = new PairingsBn254.G1Point[](2);
        a_commits[0] = proof.aw_commit;
        a_commits[1] = proof.saw_commit;

        PairingsBn254.Fr[] memory a_seps = new PairingsBn254.Fr[](2);
        a_seps[0] = PairingsBn254.new_fr(1);
        a_seps[1] = saw_challenge;

        PairingsBn254.G1Point memory a1 = multiscalar_mul(
            a_seps,
            a_commits
        );

        PairingsBn254.G1Point[] memory b_commits = new PairingsBn254.G1Point[](4);
        b_commits[0] = proof.aw_commit;
        b_commits[1] = proof.saw_commit;
        b_commits[2] = f;
        b_commits[3] = e;

        PairingsBn254.Fr[] memory b_seps = new PairingsBn254.Fr[](4);
        b_seps[0] = z_challenge;
        b_seps[1] = saw_challenge;
        b_seps[1].mul_assign(z_challenge);
        b_seps[1].mul_assign(vk.omega);
        b_seps[2] = PairingsBn254.new_fr(1);
        b_seps[3] = PairingsBn254.new_fr(0);
        b_seps[3].sub_assign(PairingsBn254.new_fr(1));

        PairingsBn254.G1Point memory b1 = multiscalar_mul(
            b_seps,
            b_commits
        );

        return PairingsBn254.pairingProd2(
            a1,
            vk.g2_x,
            b1,
            PairingsBn254.P2()
        );
    }
}