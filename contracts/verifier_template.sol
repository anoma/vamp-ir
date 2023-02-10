// Adapted from Matter Labs Plonk verifier at https://github.com/matter-labs/zksync/blob/master/contracts/contracts/PlonkCore.sol
// SPDX-License-Identifier: MIT OR Apache-2.0

pragma solidity >=0.5.0;
pragma experimental ABIEncoderV2;

library TranscriptLibrary {
    // flip                    0xe000000000000000000000000000000000000000000000000000000000000000;
    uint256 constant FR_MASK = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;

    uint32 constant DST_0 = 0;
    uint32 constant DST_1 = 1;
    uint32 constant DST_CHALLENGE = 2;

    struct Transcript {
        bytes32 state_0;
        bytes32 state_1;
        uint32 challenge_counter;
    }

    function new_transcript() internal pure returns (Transcript memory t) {
        t.state_0 = bytes32(0);
        t.state_1 = bytes32(0);
        t.challenge_counter = 0;
    }

    function update_with_u256(Transcript memory self, uint256 value) internal pure {
        bytes32 old_state_0 = self.state_0;
        self.state_0 = keccak256(abi.encodePacked(DST_0, old_state_0, self.state_1, value));
        self.state_1 = keccak256(abi.encodePacked(DST_1, old_state_0, self.state_1, value));
    }

    function update_with_fr(Transcript memory self, PairingsBn254.Fr memory value) internal pure {
        update_with_u256(self, value.value);
    }

    function update_with_g1(Transcript memory self, PairingsBn254.G1Point memory p) internal pure {
        update_with_u256(self, p.X);
        update_with_u256(self, p.Y);
    }

    function get_challenge(Transcript memory self) internal pure returns (PairingsBn254.Fr memory challenge) {
        bytes32 query = keccak256(abi.encodePacked(DST_CHALLENGE, self.state_0, self.state_1, self.challenge_counter));
        self.challenge_counter += 1;
        challenge = PairingsBn254.Fr({value: uint256(query) & FR_MASK});
    }
}

library PairingsBn254 {
    uint256 constant q_mod = 21888242871839275222246405745257275088696311157297823662689037894645226208583;
    uint256 constant r_mod = 21888242871839275222246405745257275088548364400416034343698204186575808495617;
    uint256 constant bn254_b_coeff = 3;

    struct G1Point {
        uint256 X;
        uint256 Y;
    }

    struct Fr {
        uint256 value;
    }

    function new_fr(uint256 fr) internal pure returns (Fr memory) {
        require(fr < r_mod);
        return Fr({value: fr});
    }

    function copy(Fr memory self) internal pure returns (Fr memory n) {
        n.value = self.value;
    }

    function assign(Fr memory self, Fr memory other) internal pure {
        self.value = other.value;
    }

    function inverse(Fr memory fr) internal view returns (Fr memory) {
        require(fr.value != 0);
        return pow(fr, r_mod - 2);
    }

    function add_assign(Fr memory self, Fr memory other) internal pure {
        self.value = addmod(self.value, other.value, r_mod);
    }

    function sub_assign(Fr memory self, Fr memory other) internal pure {
        self.value = addmod(self.value, r_mod - other.value, r_mod);
    }

    function mul_assign(Fr memory self, Fr memory other) internal pure {
        self.value = mulmod(self.value, other.value, r_mod);
    }

    function pow(Fr memory self, uint256 power) internal view returns (Fr memory) {
        uint256[6] memory input = [32, 32, 32, self.value, power, r_mod];
        uint256[1] memory result;
        bool success;
        assembly {
            success := staticcall(gas(), 0x05, input, 0xc0, result, 0x20)
        }
        require(success);
        return Fr({value: result[0]});
    }

    // Encoding of field elements is: X[0] * z + X[1]
    struct G2Point {
        uint256[2] X;
        uint256[2] Y;
    }

    function P1() internal pure returns (G1Point memory) {
        return G1Point(1, 2);
    }

    function new_g1(uint256 x, uint256 y) internal pure returns (G1Point memory) {
        return G1Point(x, y);
    }

    function new_g1_checked(uint256 x, uint256 y) internal pure returns (G1Point memory) {
        if (x == 0 && y == 0) {
            // point of infinity is (0,0)
            return G1Point(x, y);
        }

        // check encoding
        require(x < q_mod);
        require(y < q_mod);
        // check on curve
        uint256 lhs = mulmod(y, y, q_mod); // y^2
        uint256 rhs = mulmod(x, x, q_mod); // x^2
        rhs = mulmod(rhs, x, q_mod); // x^3
        rhs = addmod(rhs, bn254_b_coeff, q_mod); // x^3 + b
        require(lhs == rhs);

        return G1Point(x, y);
    }

    function new_g2(uint256[2] memory x, uint256[2] memory y) internal pure returns (G2Point memory) {
        return G2Point(x, y);
    }

    function copy_g1(G1Point memory self) internal pure returns (G1Point memory result) {
        result.X = self.X;
        result.Y = self.Y;
    }

    function P2() internal pure returns (G2Point memory) {
        // for some reason ethereum expects to have c1*v + c0 form

        return
            G2Point(
                [
                    0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2,
                    0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
                ],
                [
                    0x090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b,
                    0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
                ]
            );
    }

    function negate(G1Point memory self) internal pure {
        // The prime q in the base field F_q for G1
        if (self.Y == 0) {
            require(self.X == 0);
            return;
        }

        self.Y = q_mod - self.Y;
    }

    function point_add(G1Point memory p1, G1Point memory p2) internal view returns (G1Point memory r) {
        point_add_into_dest(p1, p2, r);
        return r;
    }

    function point_add_assign(G1Point memory p1, G1Point memory p2) internal view {
        point_add_into_dest(p1, p2, p1);
    }

    function point_add_into_dest(
        G1Point memory p1,
        G1Point memory p2,
        G1Point memory dest
    ) internal view {
        if (p2.X == 0 && p2.Y == 0) {
            // we add zero, nothing happens
            dest.X = p1.X;
            dest.Y = p1.Y;
            return;
        } else if (p1.X == 0 && p1.Y == 0) {
            // we add into zero, and we add non-zero point
            dest.X = p2.X;
            dest.Y = p2.Y;
            return;
        } else {
            uint256[4] memory input;

            input[0] = p1.X;
            input[1] = p1.Y;
            input[2] = p2.X;
            input[3] = p2.Y;

            bool success = false;
            assembly {
                success := staticcall(gas(), 6, input, 0x80, dest, 0x40)
            }
            require(success);
        }
    }

    function point_sub_assign(G1Point memory p1, G1Point memory p2) internal view {
        point_sub_into_dest(p1, p2, p1);
    }

    function point_sub_into_dest(
        G1Point memory p1,
        G1Point memory p2,
        G1Point memory dest
    ) internal view {
        if (p2.X == 0 && p2.Y == 0) {
            // we subtracted zero, nothing happens
            dest.X = p1.X;
            dest.Y = p1.Y;
            return;
        } else if (p1.X == 0 && p1.Y == 0) {
            // we subtract from zero, and we subtract non-zero point
            dest.X = p2.X;
            dest.Y = q_mod - p2.Y;
            return;
        } else {
            uint256[4] memory input;

            input[0] = p1.X;
            input[1] = p1.Y;
            input[2] = p2.X;
            input[3] = q_mod - p2.Y;

            bool success = false;
            assembly {
                success := staticcall(gas(), 6, input, 0x80, dest, 0x40)
            }
            require(success);
        }
    }

    function point_mul(G1Point memory p, Fr memory s) internal view returns (G1Point memory r) {
        point_mul_into_dest(p, s, r);
        return r;
    }

    function point_mul_assign(G1Point memory p, Fr memory s) internal view {
        point_mul_into_dest(p, s, p);
    }

    function point_mul_into_dest(
        G1Point memory p,
        Fr memory s,
        G1Point memory dest
    ) internal view {
        uint256[3] memory input;
        input[0] = p.X;
        input[1] = p.Y;
        input[2] = s.value;
        bool success;
        assembly {
            success := staticcall(gas(), 7, input, 0x60, dest, 0x40)
        }
        require(success);
    }

    function pairing(G1Point[] memory p1, G2Point[] memory p2) internal view returns (bool) {
        require(p1.length == p2.length);
        uint256 elements = p1.length;
        uint256 inputSize = elements * 6;
        uint256[] memory input = new uint256[](inputSize);
        for (uint256 i = 0; i < elements; i++) {
            input[i * 6 + 0] = p1[i].X;
            input[i * 6 + 1] = p1[i].Y;
            input[i * 6 + 2] = p2[i].X[0];
            input[i * 6 + 3] = p2[i].X[1];
            input[i * 6 + 4] = p2[i].Y[0];
            input[i * 6 + 5] = p2[i].Y[1];
        }
        uint256[1] memory out;
        bool success;
        assembly {
            success := staticcall(gas(), 8, add(input, 0x20), mul(inputSize, 0x20), out, 0x20)
        }
        require(success);
        return out[0] != 0;
    }

    /// Convenience method for a pairing check for two pairs.
    function pairingProd2(
        G1Point memory a1,
        G2Point memory a2,
        G1Point memory b1,
        G2Point memory b2
    ) internal view returns (bool) {
        G1Point[] memory p1 = new G1Point[](2);
        G2Point[] memory p2 = new G2Point[](2);
        p1[0] = a1;
        p1[1] = b1;
        p2[0] = a2;
        p2[1] = b2;
        return pairing(p1, p2);
    }
}

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
    
    function next_uint(
        bytes memory bs, uint start
    ) internal pure returns (uint) {
        require(bs.length >= start + 32, "slicing out of range");
        uint x;
        assembly {
            x := mload(add(bs, add(0x20, start)))
        }
        start += 32;
        return x;
    }

    function deserialize_proof(
        bytes calldata input,
        VerifierKey memory vk
    ) internal pure returns (Proof memory proof) {

        uint256 idx;

        for(uint i=0; i < vk.num_inputs; i++){
            proof.input_values[i] = PairingsBn254.new_fr(next_uint(input, idx));
        }

        proof.a_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.b_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.c_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.d_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.z_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.f_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.h_1_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.h_2_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.z_2_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.t_1_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.t_2_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.t_3_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );
        proof.t_4_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.aw_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.saw_comm = PairingsBn254.G1Point(
            next_uint(input, idx),
            next_uint(input, idx)
        );

        proof.evaluations = deserialize_evaluations(input, idx);    

        return proof;                                                                             
    }

    function deserialize_evaluations(
        bytes calldata input,
        uint idx
    ) internal pure returns (ProofEvaluations memory evaluations) {
        // WireEvaluations
        evaluations.wire_evals.a_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.wire_evals.b_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.wire_evals.c_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.wire_evals.d_eval = PairingsBn254.new_fr(next_uint(input, idx));

        // PermutationEvaluations
        evaluations.perm_evals.left_sigma_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.perm_evals.right_sigma_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.perm_evals.out_sigma_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.perm_evals.permutation_eval = PairingsBn254.new_fr(next_uint(input, idx));

        // LookupEvaluations
        evaluations.lookup_evals.q_lookup_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.z2_next_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.h1_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.h1_next_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.h2_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.f_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.table_eval = PairingsBn254.new_fr(next_uint(input, idx));
        evaluations.lookup_evals.table_next_eval = PairingsBn254.new_fr(next_uint(input, idx));

        // CustomEvaluations
        for(uint i=0; i < input.length-idx; i++){
            evaluations.custom_evals.vals[i] = PairingsBn254.new_fr(next_uint(input, idx));
        }
    }

    struct ArithmeticVerifierKey {
        PairingsBn254.G1Point q_m_comm;
        PairingsBn254.G1Point q_l_comm;
        PairingsBn254.G1Point q_r_comm;
        PairingsBn254.G1Point q_o_comm;
        PairingsBn254.G1Point q_4_comm;        
        PairingsBn254.G1Point q_c_comm;
        PairingsBn254.G1Point q_arith_comm;
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
        PairingsBn254.G1Point aw_comm;
        PairingsBn254.G1Point saw_comm;

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

contract Plonk4ArithVerifier {
    uint256 constant r_mod = 21888242871839275222246405745257275088548364400416034343698204186575808495617;

    using PairingsBn254 for PairingsBn254.G1Point;
    using PairingsBn254 for PairingsBn254.G2Point;
    using PairingsBn254 for PairingsBn254.Fr;

    using ZkGarage for ZkGarage.Proof;
    using ZkGarage for ZkGarage.VerifierKey;
    using ZkGarage for ZkGarage.VerifierState;

    using TranscriptLibrary for TranscriptLibrary.Transcript;

    // this line replaced by hard-coded verifier key
    ZkGarage.VerifierKey vk;

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

    function compute_r0_factor (
        PairingsBn254.Fr memory sigma_eval,
        PairingsBn254.Fr memory wire_eval,
        ZkGarage.VerifierState memory verifier
    ) internal pure returns(PairingsBn254.Fr memory tmp) {

        // (wire_eval) + beta * (sigma_eval) + gamma
        PairingsBn254.Fr memory beta_sig1 = sigma_eval;
        beta_sig1.mul_assign(verifier.beta);

        tmp = wire_eval;
        tmp.add_assign(beta_sig1);
        tmp.add_assign(verifier.gamma);

        return tmp;
    }

    function compute_r0 (
        ZkGarage.Proof memory proof,
        ZkGarage.VerifierState memory verifier
    ) internal view {
        verifier.pi_eval = barycentric_eval(proof.input_values, verifier.z_challenge, vk.domain_size, vk.omega);
        
        PairingsBn254.Fr memory tmp_b;

        // a + beta * sigma_1 + gamma
        tmp_b.mul_assign(
            compute_r0_factor (
                proof.evaluations.perm_evals.left_sigma_eval,
                proof.evaluations.wire_evals.a_eval,
                verifier
            )
        );

        // b + beta * sigma_2 + gamma
        tmp_b.mul_assign(
            compute_r0_factor (
                proof.evaluations.perm_evals.right_sigma_eval,
                proof.evaluations.wire_evals.b_eval,
                verifier
            )
        );

        // c + beta * sigma_3 + gamma
        tmp_b.mul_assign(
            compute_r0_factor (
                proof.evaluations.perm_evals.out_sigma_eval,
                proof.evaluations.wire_evals.c_eval,
                verifier
            )
        );

        // ((d + gamma) * z_hat) * alpha
        PairingsBn254.Fr memory b_3 = proof.evaluations.wire_evals.d_eval;
        b_3.add_assign(verifier.gamma);
        b_3.mul_assign(proof.evaluations.perm_evals.permutation_eval);
        b_3.mul_assign(verifier.alpha);

        tmp_b.mul_assign(b_3);

        // l_1(z) * alpha^2
        PairingsBn254.Fr memory tmp_c = verifier.l1_eval;
        tmp_c.mul_assign(verifier.alpha);
        tmp_c.mul_assign(verifier.alpha);

        PairingsBn254.Fr memory epsilon_one_plus_delta = PairingsBn254.new_fr(1);
        epsilon_one_plus_delta.add_assign(verifier.delta);
        epsilon_one_plus_delta.mul_assign(verifier.epsilon);

        PairingsBn254.Fr memory tmp_d = verifier.lookup_sep_challenge;
        tmp_d.mul_assign(verifier.lookup_sep_challenge);
        tmp_d.mul_assign(proof.evaluations.lookup_evals.z2_next_eval);

        PairingsBn254.Fr memory tmp_e = verifier.lookup_sep_challenge;
        tmp_e.mul_assign(verifier.lookup_sep_challenge);
        tmp_e.mul_assign(verifier.lookup_sep_challenge);
        tmp_e.mul_assign(verifier.l1_eval);

        PairingsBn254.Fr memory tmp_d_1;
        tmp_d_1 = epsilon_one_plus_delta;
        tmp_d_1.add_assign(verifier.delta);
        tmp_d_1.add_assign(proof.evaluations.lookup_evals.h2_eval);
        
        PairingsBn254.Fr memory tmp_d_2;
        tmp_d_2 = verifier.delta;
        tmp_d_2.mul_assign(proof.evaluations.lookup_evals.h1_next_eval);
        tmp_d_2.add_assign(epsilon_one_plus_delta);
        tmp_d_2.add_assign(proof.evaluations.lookup_evals.h2_eval);
        
        tmp_d.mul_assign(tmp_d_1);
        tmp_d.mul_assign(tmp_d_2);

        // Return r_0
        verifier.r0 = verifier.pi_eval;
        verifier.r0.sub_assign(tmp_b);
        verifier.r0.sub_assign(tmp_c);
        verifier.r0.sub_assign(tmp_d);
        verifier.r0.sub_assign(tmp_e);
    }

    function compute_permutation_portion_1 (
        ZkGarage.Proof memory proof,
        ZkGarage.VerifierState memory verifier
    ) internal view returns (PairingsBn254.Fr memory x) {
        
        // Permutation part of linearization polynomial

        // (a_eval + beta * z + gamma)(b_eval + beta * z * k1 +
        // gamma)(c_eval + beta * k2 * z + gamma)(d_eval + beta
        // * k3 * z + gamma) * alpha

        PairingsBn254.Fr memory beta_z = verifier.beta;
        beta_z.mul_assign(verifier.z_challenge);

        PairingsBn254.Fr memory q_0 = proof.evaluations.wire_evals.a_eval;
        q_0.add_assign(beta_z);
        q_0.add_assign(verifier.gamma);

        PairingsBn254.Fr memory beta_k1_z = verifier.beta;
        beta_k1_z.mul_assign(vk.k1);
        beta_k1_z.mul_assign(verifier.z_challenge);

        PairingsBn254.Fr memory q_1 = proof.evaluations.wire_evals.b_eval;
        q_1.add_assign(beta_k1_z);
        q_1.add_assign(verifier.gamma);

        PairingsBn254.Fr memory beta_k2_z = verifier.beta;
        beta_k2_z.mul_assign(vk.k2);
        beta_k2_z.mul_assign(verifier.z_challenge);

        PairingsBn254.Fr memory q_2 = proof.evaluations.wire_evals.c_eval;
        q_2.add_assign(beta_k2_z);
        q_2.add_assign(verifier.gamma);

        PairingsBn254.Fr memory beta_k3_z = verifier.beta;
        beta_k3_z.mul_assign(vk.k3);
        beta_k3_z.mul_assign(verifier.z_challenge);

        PairingsBn254.Fr memory q_3 = proof.evaluations.wire_evals.d_eval;
        q_3.add_assign(beta_k3_z);
        q_3.add_assign(verifier.gamma);

        x = q_0;
        x.mul_assign(q_1);
        x.mul_assign(q_2);
        x.mul_assign(q_3);

        // l1(z) * alpha^2
        PairingsBn254.Fr memory r = verifier.l1_eval;
        r.mul_assign(verifier.alpha);
        r.mul_assign(verifier.alpha);

        x.add_assign(r);

        return x;
    }

    function compute_permutation_portion_2 (
        ZkGarage.Proof memory proof,
        ZkGarage.VerifierState memory verifier
    ) internal pure returns (PairingsBn254.Fr memory y) {
        // -(a_eval + beta * sigma_1_eval + gamma)(b_eval + beta *
        // sigma_2_eval + gamma)(c_eval + beta * sigma_3_eval +
        // gamma) * alpha^2

        PairingsBn254.Fr memory beta_sigma_1 = verifier.beta;
        beta_sigma_1.mul_assign(proof.evaluations.perm_evals.left_sigma_eval);

        PairingsBn254.Fr memory q2_0 = proof.evaluations.wire_evals.a_eval;
        q2_0.add_assign(beta_sigma_1);
        q2_0.add_assign(verifier.gamma);

        PairingsBn254.Fr memory beta_sigma_2 = verifier.beta;
        beta_sigma_2.mul_assign(proof.evaluations.perm_evals.right_sigma_eval);

        PairingsBn254.Fr memory q2_1 = proof.evaluations.wire_evals.b_eval;
        q2_1.add_assign(beta_sigma_2);
        q2_1.add_assign(verifier.gamma);

        PairingsBn254.Fr memory beta_sigma_3 = verifier.beta;
        beta_sigma_3.mul_assign(proof.evaluations.perm_evals.out_sigma_eval);

        PairingsBn254.Fr memory q2_2 = proof.evaluations.wire_evals.c_eval;
        q2_2.add_assign(beta_sigma_3);
        q2_2.add_assign(verifier.gamma);

        PairingsBn254.Fr memory q2_3 = verifier.beta;
        q2_3.mul_assign(proof.evaluations.perm_evals.permutation_eval);
        q2_3.mul_assign(verifier.alpha);

        y.sub_assign(q2_0);
        y.mul_assign(q2_1);
        y.mul_assign(q2_2);
        y.mul_assign(q2_3);

        return y;
    }

    function compute_point_1 (
        ZkGarage.Proof memory proof,
        ZkGarage.VerifierState memory verifier
    ) internal view returns (PairingsBn254.G1Point memory a1) {

        PairingsBn254.G1Point[] memory a_commits = new PairingsBn254.G1Point[](2);
        a_commits[0] = proof.aw_comm;
        a_commits[1] = proof.saw_comm;

        PairingsBn254.Fr[] memory a_seps = new PairingsBn254.Fr[](2);
        a_seps[0] = PairingsBn254.new_fr(1);
        a_seps[1] = verifier.saw_challenge;

        a1 = multiscalar_mul(
            a_seps,
            a_commits
        );

        return a1;
    }

    function compute_point_2 (
        ZkGarage.Proof memory proof,
        ZkGarage.VerifierState memory verifier
    ) internal view returns (PairingsBn254.G1Point memory b1) {

        PairingsBn254.G1Point memory f = multiscalar_mul(verifier.aw_separators, verifier.aw_commits);
        PairingsBn254.G1Point memory e = PairingsBn254.P1();
        PairingsBn254.Fr memory e_scalar = PairingsBn254.new_fr(1);

        for (uint256 i = 0; i < verifier.aw_evals.length; i++) {
            PairingsBn254.Fr memory tmp = verifier.aw_evals[i];
            tmp.mul_assign(verifier.aw_separators[i]);
            e_scalar.add_assign(tmp);
        }

        for (uint256 i = 0; i < verifier.saw_evals.length; i++) {
            PairingsBn254.Fr memory tmp = verifier.saw_evals[i];
            tmp.mul_assign(verifier.saw_separators[i]);
            e_scalar.add_assign(tmp);
        }

        e.point_mul_assign(e_scalar);

        PairingsBn254.G1Point[] memory b_commits = new PairingsBn254.G1Point[](4);
        b_commits[0] = proof.aw_comm;
        b_commits[1] = proof.saw_comm;
        b_commits[2] = f;
        b_commits[3] = e;

        PairingsBn254.Fr[] memory b_seps = new PairingsBn254.Fr[](4);
        b_seps[0] = verifier.z_challenge;
        b_seps[1] = verifier.saw_challenge;
        b_seps[1].mul_assign(verifier.z_challenge);
        b_seps[1].mul_assign(vk.omega);
        b_seps[2] = PairingsBn254.new_fr(1);
        b_seps[3] = PairingsBn254.new_fr(0);
        b_seps[3].sub_assign(PairingsBn254.new_fr(1));

        b1 = multiscalar_mul(
            b_seps,
            b_commits
        );

        return b1;
    }

    function verify(
        ZkGarage.Proof memory proof
    ) public view returns (bool) {

        ZkGarage.VerifierState memory verifier;

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
        verifier.zeta = transcript.get_challenge();
        transcript.update_with_fr(verifier.zeta);     

        // Add commitmtent to f_poly to transcript
        transcript.update_with_g1(proof.f_comm);

        // Add commitment to h polynomials to transcript
        transcript.update_with_g1(proof.h_1_comm);
        transcript.update_with_g1(proof.h_2_comm);

        // Compute permutation challenge `beta`.
        verifier.beta = transcript.get_challenge();
        transcript.update_with_fr(verifier.beta); 
        
        // Compute permutation challenge `gamma`.
        verifier.gamma = transcript.get_challenge();
        transcript.update_with_fr(verifier.gamma); 

        // Compute permutation challenge `delta`.
        verifier.delta = transcript.get_challenge();
        transcript.update_with_fr(verifier.delta); 
        
        // Compute permutation challenge `epsilon`.
        verifier.epsilon = transcript.get_challenge();
        transcript.update_with_fr(verifier.epsilon); 

        // Challenges must be different
        require(verifier.beta.value != verifier.gamma.value);
        require(verifier.beta.value != verifier.delta.value);
        require(verifier.beta.value != verifier.epsilon.value); 
        require(verifier.gamma.value != verifier.delta.value); 
        require(verifier.gamma.value != verifier.epsilon.value); 
        require(verifier.delta.value != verifier.epsilon.value);

        // Add commitment to permutation polynomial to transcript
        transcript.update_with_g1(proof.z_comm);

        // Compute quotient challenge `alpha`
        verifier.alpha = transcript.get_challenge();
        transcript.update_with_fr(verifier.alpha);

        // Compute custom gate separator challenges
        verifier.range_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(verifier.range_sep_challenge);

        verifier.logic_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(verifier.logic_sep_challenge);

        verifier.fixed_base_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(verifier.fixed_base_sep_challenge);

        verifier.var_base_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(verifier.var_base_sep_challenge);

        verifier.lookup_sep_challenge = transcript.get_challenge();
        transcript.update_with_fr(verifier.lookup_sep_challenge);

        // Add commitment to quotient polynomial to the transcript
        transcript.update_with_g1(proof.t_1_comm);
        transcript.update_with_g1(proof.t_2_comm);
        transcript.update_with_g1(proof.t_3_comm);
        transcript.update_with_g1(proof.t_4_comm);
        

        // Compute evaluation point challenge `z_challenge`
        verifier.z_challenge = transcript.get_challenge();
        transcript.update_with_fr(verifier.z_challenge);

        // Compute zero polynomial evaluated at `z_challenge`
        verifier.z_h_eval = verifier.z_challenge.pow(vk.domain_size);
        verifier.z_h_eval.sub_assign(PairingsBn254.new_fr(1));

        // Compute first lagrange polynomial evaluated at `z_challenge`
        PairingsBn254.Fr memory den = verifier.z_challenge;

        den.sub_assign(PairingsBn254.new_fr(1));
        den.mul_assign(domain_size_as_fr);

        verifier.l1_eval = verifier.z_h_eval;
        verifier.l1_eval.mul_assign(den.inverse());
        
        // Compute `r0`, the constant term of the linearization polynomial
        compute_r0(proof, verifier);        

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

        scalars[6] = compute_permutation_portion_1(proof, verifier);
        points[6] = proof.z_comm;

        scalars[7] = compute_permutation_portion_2(proof, verifier);
        points[7] = vk.perm.fourth_sigma_comm;

        // Second part
        verifier.vanishing_poly_eval = evaluate_vanishing(vk.domain_size, verifier.z_challenge);
    
        // z_challenge ^ n
        verifier.z_challenge_to_n = verifier.vanishing_poly_eval;
        verifier.z_challenge_to_n.add_assign(PairingsBn254.new_fr(1));

        scalars[8] = PairingsBn254.new_fr(0);
        scalars[8].sub_assign(verifier.vanishing_poly_eval);

        scalars[9] = scalars[8];
        scalars[9].mul_assign(verifier.z_challenge_to_n);

        scalars[10] = scalars[9];
        scalars[10].mul_assign(verifier.z_challenge_to_n);

        scalars[11] = scalars[10];
        scalars[11].mul_assign(verifier.z_challenge_to_n);

        points[8] = proof.t_1_comm;
        points[9] = proof.t_2_comm;
        points[10] = proof.t_3_comm;
        points[11] = proof.t_4_comm;

        require(scalars.length == points.length);

        verifier.aw_commits = new PairingsBn254.G1Point[](8);

        verifier.aw_commits[0] = multiscalar_mul(scalars, points);

        verifier.aw_challenge = transcript.get_challenge();

        verifier.aw_commits[1] = vk.perm.left_sigma_comm;
        verifier.aw_commits[2] = vk.perm.right_sigma_comm;
        verifier.aw_commits[3] = vk.perm.out_sigma_comm;
        verifier.aw_commits[4] = proof.a_comm;
        verifier.aw_commits[5] = proof.b_comm;
        verifier.aw_commits[6] = proof.c_comm;
        verifier.aw_commits[7] = proof.d_comm;


        verifier.aw_evals = new PairingsBn254.Fr[](8);
        verifier.aw_evals[0] = PairingsBn254.new_fr(0);
        verifier.aw_evals[0].sub_assign(verifier.r0);

        verifier.aw_evals[1] = proof.evaluations.perm_evals.left_sigma_eval;
        verifier.aw_evals[2] = proof.evaluations.perm_evals.right_sigma_eval;
        verifier.aw_evals[3] = proof.evaluations.perm_evals.out_sigma_eval;
        verifier.aw_evals[4] = proof.evaluations.wire_evals.a_eval;
        verifier.aw_evals[5] = proof.evaluations.wire_evals.b_eval;
        verifier.aw_evals[6] = proof.evaluations.wire_evals.c_eval;
        verifier.aw_evals[7] = proof.evaluations.wire_evals.d_eval;

        verifier.saw_challenge = transcript.get_challenge();

        verifier.saw_evals = new PairingsBn254.Fr[](1);
        verifier.saw_evals[0] = proof.evaluations.perm_evals.permutation_eval;

        verifier.aw_separators = new PairingsBn254.Fr[](verifier.aw_evals.length);
        
        verifier.aw_separators[0] = PairingsBn254.new_fr(1);

        for (uint256 i = 1; i < verifier.aw_evals.length-1; i++) {
            verifier.aw_separators[i] = verifier.aw_separators[i-1];
            verifier.aw_separators[i].mul_assign(verifier.aw_challenge);
        }

        verifier.saw_separators = new PairingsBn254.Fr[](verifier.saw_evals.length);
        
        verifier.saw_separators[0] = verifier.saw_challenge;

        for (uint256 i = 1; i < verifier.saw_evals.length-1; i++) {
            verifier.saw_separators[i] = verifier.saw_separators[i-1];
            verifier.saw_separators[i].mul_assign(verifier.saw_challenge);
        }

        PairingsBn254.G1Point memory a1 = compute_point_1(proof, verifier);
        PairingsBn254.G1Point memory b1 = compute_point_2(proof, verifier);

        return PairingsBn254.pairingProd2(
            a1,
            vk.g2_x,
            b1,
            PairingsBn254.P2()
        );
    }
}