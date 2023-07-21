#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::string::*;
    use num_bigint::BigInt;
    use std::collections::HashSet;

    #[test]
    fn test_translate_3ac_to_diagram() {
        let test_expr = vec![
            TExpr {
                v: Expr::Infix(
                    InfixOp::Equal,
                    Box::new(TExpr {
                        v: Expr::Variable(Variable { name: None, id: 56 }),
                        t: None,
                    }),
                    Box::new(TExpr {
                        v: Expr::Infix(
                            InfixOp::Add,
                            Box::new(TExpr {
                                v: Expr::Variable(Variable {
                                    name: Some("x".to_string()),
                                    id: 11,
                                }),
                                t: None,
                            }),
                            Box::new(TExpr {
                                v: Expr::Constant(BigInt::from(1)),
                                t: None,
                            }),
                        ),
                        t: None,
                    }),
                ),
                t: None,
            },
            TExpr {
                v: Expr::Infix(
                    InfixOp::Equal,
                    Box::new(TExpr {
                        v: Expr::Variable(Variable { name: None, id: 56 }),
                        t: None,
                    }),
                    Box::new(TExpr {
                        v: Expr::Constant(BigInt::from(3345387)),
                        t: None,
                    }),
                ),
                t: None,
            },
            TExpr {
                v: Expr::Infix(
                    InfixOp::Equal,
                    Box::new(TExpr {
                        v: Expr::Variable(Variable {
                            name: Some("c".to_string()),
                            id: 12,
                        }),
                        t: None,
                    }),
                    Box::new(TExpr {
                        v: Expr::Constant(BigInt::from(-32)),
                        t: None,
                    }),
                ),
                t: None,
            },
            TExpr {
                v: Expr::Infix(
                    InfixOp::Equal,
                    Box::new(TExpr {
                        v: Expr::Variable(Variable {
                            name: Some("f".to_string()),
                            id: 13,
                        }),
                        t: None,
                    }),
                    Box::new(TExpr {
                        v: Expr::Constant(BigInt::from(-68)),
                        t: None,
                    }),
                ),
                t: None,
            },
            TExpr {
                v: Expr::Infix(
                    InfixOp::Equal,
                    Box::new(TExpr {
                        v: Expr::Variable(Variable {
                            name: Some("r".to_string()),
                            id: 14,
                        }),
                        t: None,
                    }),
                    Box::new(TExpr {
                        v: Expr::Variable(Variable { name: None, id: 58 }),
                        t: None,
                    }),
                ),
                t: None,
            },
        ];

        let mut input_ids = HashSet::new();
        input_ids.insert(11);
        input_ids.insert(12);
        input_ids.insert(13);
        input_ids.insert(14);

        let str_diag =
            build_string_diagram(test_expr, &input_ids, &mut DefinitionRegistry::new(vec![]));

        assert!(
            str_diag.is_well_formed(),
            "Test that the translated diagram is well formed"
        );
    }

    #[test]
    fn test_fuse_equality_nodes_basic() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(5, 0)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(5, 2)));
        let addr1 = diagram.add_node(Node::Equality(
            vec![Variable::new(1)],
            vec![Port(i1, 0), Port(5, 1), Port(i2, 0)],
        ));
        let addr2 = diagram.add_node(Node::Equality(
            vec![Variable::new(2)],
            vec![Port(i3, 0), Port(4, 1), Port(i4, 0)],
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseEquality(addr1, 1, vec![], addr2, vec![]),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&addr2).is_none(),
            "Check that the second node is gone"
        );
        assert_eq!(
            node_count,
            diagram.nodes.len() + 1,
            "There should be one less node after fusion"
        );

        if let Some(Node::Equality(vars, ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(
                vars.len(),
                2,
                "Check that the first node has updated variables and ports"
            );

            assert_eq!(ports.len(), 4, "The total ports in the remaining node should be two less than the sum of the original");
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_fuse_addition_nodes() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(5, 1)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(5, 2)));
        let addr1 = diagram.add_node(Node::Addition(vec![Port(i1, 0), Port(5, 0), Port(i2, 0)]));
        let addr2 = diagram.add_node(Node::Addition(vec![
            Port(addr1, 1),
            Port(i4, 0),
            Port(i5, 0),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseAddition(addr1, 1, vec![], addr2, vec![]),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&addr2).is_none(),
            "Check that the second node is gone"
        );
        assert_eq!(
            node_count,
            diagram.nodes.len() + 1,
            "There should be one less node after fusion"
        );

        if let Some(Node::Addition(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 4, "The total ports in the remaining node should be two less than the sum of the original");
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_fuse_multiplication_nodes() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(5, 1)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(5, 2)));
        let addr1 = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0),
            Port(5, 0),
            Port(i2, 0),
        ]));
        let addr2 = diagram.add_node(Node::Multiplication(vec![
            Port(addr1, 1),
            Port(i4, 0),
            Port(i5, 0),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseMultiplication(addr1, 1, vec![], addr2, vec![]),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&addr2).is_none(),
            "Check that the second node is gone"
        );
        assert_eq!(
            node_count,
            diagram.nodes.len() + 1,
            "There should be one less node after fusion"
        );

        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 4, "The total ports in the remaining node should be two less than the sum of the original");
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_split_addition_nodes_basic() {
        // Create a simple diagram with a single addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(4, 3)));
        let addr1 = diagram.add_node(Node::Addition(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(i4, 0),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        apply_rewrite_step(&mut diagram, &(), RewriteRule::SplitAddition(addr1));

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        // Assert that the original node is modified
        let new_address = addr1 + 1;
        if let Some(Node::Addition(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 3, "Modified node should only have three ports");
            assert_eq!(
                ports[0],
                Port(new_address, 2),
                "Modified node head should point to the new node"
            );
        } else {
            panic!("Node not properly modified");
        }

        // Assert that a new node has been added
        if let Some(Node::Addition(ports)) = diagram.nodes.get(&new_address) {
            assert_eq!(ports.len(), 3, "New node should only have three ports");
            assert_eq!(
                ports[0],
                Port(i1, 0),
                "New node should have same head as original node"
            );
        } else {
            panic!("New node not properly formed");
        }

        assert_eq!(
            node_count + 1,
            diagram.nodes.len(),
            "There should be one more node after splitting"
        );
    }

    #[test]
    fn test_split_multiplication_nodes_basic() {
        // Create a simple diagram with a single multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(4, 3)));
        let addr1 = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(i4, 0),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        apply_rewrite_step(&mut diagram, &(), RewriteRule::SplitMultiplication(addr1));

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        // Assert that the original node is modified
        let new_address = addr1 + 1;
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 3, "Modified node should only have three ports");
            assert_eq!(
                ports[0],
                Port(new_address, 2),
                "Modified node head should point to the new node"
            );
        } else {
            panic!("Node not properly modified");
        }

        // Assert that a new node has been added
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&new_address) {
            assert_eq!(ports.len(), 3, "New node should only have three ports");
            assert_eq!(
                ports[0],
                Port(i1, 0),
                "New node should have same head as original node"
            );
        } else {
            panic!("New node not properly formed");
        }

        assert_eq!(
            node_count + 1,
            diagram.nodes.len(),
            "There should be one more node after splitting"
        );
    }

    #[test]
    fn test_constant_constant_eq() {
        // Create a simple diagram with a single pair of interacting constants and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0)));
        let i3 = diagram.add_node(Node::Constant(BigInt::from(55), Port(3, 0)));
        let i4 = diagram.add_node(Node::Constant(BigInt::from(55), Port(i3, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::ConstantConstantRemoval(i3, i4),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i3).is_none(),
            "Check that the first node is gone"
        );
        assert!(
            diagram.nodes.get(&i4).is_none(),
            "Check that the second node is gone"
        );

        assert_eq!(
            diagram.nodes.len() + 2,
            node_count,
            "Nodecount should have decreased by 2"
        );
    }

    #[test]
    fn test_unrestricted_constant() {
        // Create a simple diagram with a single interacting constant and unrestricted and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        let i4 = diagram.add_node(Node::Constant(BigInt::from(55), Port(i3, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::UnrestrictedUnaryRemoval(i3, i4),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i3).is_none(),
            "Check that the first node is gone"
        );
        assert!(
            diagram.nodes.get(&i4).is_none(),
            "Check that the second node is gone"
        );

        assert_eq!(
            diagram.nodes.len() + 2,
            node_count,
            "Node count should have decreased by 2"
        );
    }

    #[test]
    fn test_unrestricted_unrestricted() {
        // Create a simple diagram with interacting unrestricted nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(i3, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::UnrestrictedUnaryRemoval(i3, i4),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i3).is_none(),
            "Check that the first node is gone"
        );
        assert!(
            diagram.nodes.get(&i4).is_none(),
            "Check that the second node is gone"
        );

        assert_eq!(
            diagram.nodes.len() + 2,
            node_count,
            "Node count should have decreased by 2"
        );
    }

    #[test]
    fn test_add_constant_constant_head() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Constant(BigInt::from(16), Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 1)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(32),
            Port(i1, 0),
            Port(i2, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::AddConstantConstantHead(i3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the updated diagram
        // First, check that the AddConstant node at address i3 has been replaced with a Constant node of value -16.
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Constant(value, port) => {
                    assert_eq!(value, &BigInt::from(-16));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        // Next, check that the original Constant node connected to the first port has been removed.
        assert!(diagram.nodes.get(&i1).is_none());

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_add_constant_constant_tail() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Constant(BigInt::from(16), Port(2, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(32),
            Port(i2, 0),
            Port(i1, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::AddConstantConstantTail(i3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the updated diagram
        // First, check that the AddConstant node at address i3 has been replaced with a Constant node of value 48.
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Constant(value, port) => {
                    assert_eq!(value, &BigInt::from(48));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        // Next, check that the original Constant node connected to the first port has been removed.
        assert!(diagram.nodes.get(&i1).is_none());

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_mul_constant_constant_head() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Constant(BigInt::from(15), Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(3),
            Port(i1, 0),
            Port(i2, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::MulConstantConstantHead(i3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the updated diagram
        // First, check that the MultiplyConstant node at address i3 has been replaced with a Constant node of value -16.
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Constant(value, port) => {
                    assert_eq!(value, &BigInt::from(5));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        // Next, check that the original Constant node connected to the first port has been removed.
        assert!(diagram.nodes.get(&i1).is_none());

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_mul_constant_constant_tail() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Constant(BigInt::from(16), Port(2, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(32),
            Port(i2, 0),
            Port(i1, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::MulConstantConstantTail(i3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the updated diagram
        // First, check that the MultiplyConstant node at address i3 has been replaced with a Constant node of value 48.
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Constant(value, port) => {
                    assert_eq!(value, &BigInt::from(512));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        // Next, check that the original Constant node connected to the first port has been removed.
        assert!(diagram.nodes.get(&i1).is_none());

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_exp_constant_constant_tail() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Constant(BigInt::from(6), Port(2, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i3 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(3),
            Port(i2, 0),
            Port(i1, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::ExpConstantConstantTail(i3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the updated diagram
        // First, check that the ExponentiateConstant node at address i3 has been replaced with a Constant node of value 48.
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Constant(value, port) => {
                    assert_eq!(value, &BigInt::from(216));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(
            diagram.nodes.get(&i1).is_none(),
            "check that the original Constant node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_mul_constant_zero() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(0),
            Port(i2, 0),
            Port(i1, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::MulConstantZero(i3, (Port(i1, 0), Port(i2, 0)), i3 + 1),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the updated diagram
        // First, check that the MultiplyConstant node at address i3 has been replaced with a Constant node of value 0.
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Constant(value, port) => {
                    assert_eq!(value, &BigInt::from(0));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&(i3 + 1)) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i1, 0));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 + 1 does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count + 1,
            "Node count should have increased by 1"
        );
    }

    #[test]
    fn test_add_constant_zero() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i3 = diagram.add_node(Node::AddConstant(BigInt::from(0), Port(i2, 0), Port(i1, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::AddConstantZero(i3, (Port(i1, 0), Port(i2, 0))),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i3).is_none(),
            "check that the original AddConstant node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_mul_constant_one() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(1),
            Port(i2, 0),
            Port(i1, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::MulConstantOne(i3, (Port(i1, 0), Port(i2, 0))),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(diagram.nodes.get(&i3).is_none(), "check that the original MultiplyConstant node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_exp_constant_one() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i3 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(1),
            Port(i2, 0),
            Port(i1, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::ExpConstantOne(i3, (Port(i1, 0), Port(i2, 0))),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(diagram.nodes.get(&i3).is_none(), "check that the original ExponentiateConstant node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_remove_binary_addition_node() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i3 = diagram.add_node(Node::Addition(vec![Port(i2, 0), Port(i1, 0)]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::RemoveBinaryAddition(i3, (Port(i1, 0), Port(i2, 0))),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i3).is_none(),
            "check that the original Addition node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_remove_binary_multiplication_node() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i3 = diagram.add_node(Node::Multiplication(vec![Port(i2, 0), Port(i1, 0)]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::RemoveBinaryMultiplication(i3, (Port(i1, 0), Port(i2, 0))),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(diagram.nodes.get(&i3).is_none(), "check that the original Multiplication node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_addition_by_constant_hd_tl() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        let i3 = diagram.add_node(Node::AddConstant(BigInt::from(15), Port(i1, 0), Port(3, 0)));
        let i4 = diagram.add_node(Node::AddConstant(BigInt::from(12), Port(2, 1), Port(i2, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseAdditionByConstantHdTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::AddConstant(value, _, port) => {
                    assert_eq!(value, &BigInt::from(27));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an AddConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(
            diagram.nodes.get(&i4).is_none(),
            "check that the second AddConstant node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_addition_by_constant_hd_hd() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        let i3 = diagram.add_node(Node::AddConstant(BigInt::from(15), Port(3, 0), Port(i1, 0)));
        let i4 = diagram.add_node(Node::AddConstant(BigInt::from(12), Port(2, 0), Port(i2, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseAdditionByConstantHdHd(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::AddConstant(value, port, _) => {
                    assert_eq!(value, &BigInt::from(3));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an AddConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(
            diagram.nodes.get(&i4).is_none(),
            "check that the second AddConstant node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_addition_by_constant_tl_tl() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        let i3 = diagram.add_node(Node::AddConstant(BigInt::from(15), Port(i1, 0), Port(3, 1)));
        let i4 = diagram.add_node(Node::AddConstant(BigInt::from(12), Port(i2, 0), Port(2, 1)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseAdditionByConstantTlTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::AddConstant(value, _, port) => {
                    assert_eq!(value, &BigInt::from(3));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an AddConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(
            diagram.nodes.get(&i4).is_none(),
            "check that the second AddConstant node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_multiplication_by_constant_hd_tl() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(10),
            Port(i1, 0),
            Port(3, 0),
        ));
        let i4 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(12),
            Port(2, 1),
            Port(i2, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseMultiplicationByConstantHdTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::MultiplyConstant(value, _, port) => {
                    assert_eq!(value, &BigInt::from(120));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an MultiplyConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(diagram.nodes.get(&i4).is_none(), "check that the second MultiplyConstant node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_multiplication_by_constant_hd_hd() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(3),
            Port(3, 0),
            Port(i1, 0),
        ));
        let i4 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(2, 0),
            Port(i2, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseMultiplicationByConstantHdHd(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::MultiplyConstant(value, _, port) => {
                    assert_eq!(value, &BigInt::from(5));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an MultiplyConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(diagram.nodes.get(&i4).is_none(), "check that the second MultiplyConstant node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_multiplication_by_constant_tl_tl() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(3),
            Port(i1, 0),
            Port(3, 1),
        ));
        let i4 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(i2, 0),
            Port(2, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseMultiplicationByConstantTlTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::MultiplyConstant(value, port, _) => {
                    assert_eq!(value, &BigInt::from(5));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an MultiplyConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(diagram.nodes.get(&i4).is_none(), "check that the second MultiplyConstant node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_fuse_exponentiation_by_constant_hd_tl() {
        // Create a simple diagram with a single constant exponentiation node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        let i3 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(10),
            Port(i1, 0),
            Port(3, 0),
        ));
        let i4 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(12),
            Port(2, 1),
            Port(i2, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::FuseExponentiationByConstantHdTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::ExponentiateConstant(value, _, port) => {
                    assert_eq!(value, &BigInt::from(120));
                    assert_eq!(port, &Port(i2, 0));
                }
                _ => panic!("Node is not an ExponentiateConstant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        assert!(diagram.nodes.get(&i4).is_none(), "check that the second ExponentiateConstant node connected to the first port has been removed");

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_equality_unrestricted() {
        // Create a simple diagram with one equality node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(3, 2)));
        let addr1 = diagram.add_node(Node::Equality(
            vec![Variable::new(1)],
            vec![Port(i1, 0), Port(i2, 0), Port(i3, 0)],
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::EqualityUnrestricted(addr1, 1),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the modified diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i2).is_none(),
            "Check that the second node is gone"
        );
        assert_eq!(
            node_count,
            diagram.nodes.len() + 1,
            "There should be one less node after fusion"
        );

        if let Some(Node::Equality(_, ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(
                ports.len(),
                2,
                "The total ports in the remaining node should be one less than the original"
            );
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_binary_equation_node() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i3 = diagram.add_node(Node::Equality(vec![], vec![Port(i2, 0), Port(i1, 0)]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::RemoveBinaryEquality(i3, (Port(i1, 0), Port(i2, 0))),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&i3).is_none(),
            "check that the original Equality node connected to the first port has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have decreased by 1"
        );
    }

    #[test]
    fn test_addition_const_addition_hd() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 1)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(8, 0),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(ia, 3),
            Port(i4, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::AdditionConstAddition(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Addition(ports) => {
                    assert_eq!(ports[0], Port(ic, 1));
                    assert_eq!(ports[3], Port(i4, 0));
                }
                _ => panic!("Node is not an Addition node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::AddConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0));
                    assert_eq!(port2, &Port(ia, 0));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not an AddConstant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_addition_const_addition_tl() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(8, 1),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(i4, 0),
            Port(ia, 3),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::AdditionConstAddition(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Addition(ports) => {
                    assert_eq!(ports[0], Port(ic, 1));
                    assert_eq!(ports[3], Port(i4, 0));
                }
                _ => panic!("Node is not an Addition node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::AddConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0));
                    assert_eq!(port2, &Port(ia, 0));
                    assert_eq!(val, &BigInt::from(-15));
                }
                _ => panic!("Node is not an AddConstant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_multiplication_const_multiplication_hd() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 1)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4)));
        let ia = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(8, 0),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(ia, 3),
            Port(i4, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::MultiplicationConstMultiplication(ia, 3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Multiplication(ports) => {
                    assert_eq!(ports[0], Port(ic, 1));
                    assert_eq!(ports[3], Port(i4, 0));
                }
                _ => panic!("Node is not an Multiplication node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::MultiplyConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0));
                    assert_eq!(port2, &Port(ia, 0));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not an MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_multiplication_const_multiplication_tl() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4)));
        let ia = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(8, 1),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(i4, 0),
            Port(ia, 3),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::MultiplicationConstMultiplication(ia, 3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Multiplication(ports) => {
                    assert_eq!(ports[0], Port(ic, 1));
                    assert_eq!(ports[3], Port(i4, 0));
                }
                _ => panic!("Node is not an Multiplication node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::MultiplyConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0));
                    assert_eq!(port2, &Port(ia, 0));
                    assert_eq!(val, &BigInt::from(1 / 15));
                }
                _ => panic!("Node is not an MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_addition_const() {
        // Create a simple diagram with a single constant addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(7, 0),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::AdditionConst(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Addition(ports) => {
                    assert_eq!(ports[0], Port(ic, 1));
                    assert_eq!(ports.len(), 4);
                }
                _ => panic!("Node is not an Addition node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::AddConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0));
                    assert_eq!(port2, &Port(ia, 0));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not an AddConstant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_multiplication_const() {
        // Create a simple diagram with a single constant multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4)));
        let ia = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(7, 0),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::MultiplicationConst(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Multiplication(ports) => {
                    assert_eq!(ports[0], Port(ic, 1));
                    assert_eq!(ports.len(), 4);
                }
                _ => panic!("Node is not a Multiplication node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::MultiplyConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0));
                    assert_eq!(port2, &Port(ia, 0));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not an MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_equality_const_nvar() {
        // Create a simple diagram with a single constant Equality interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4)));
        let ia = diagram.add_node(Node::Equality(
            vec![],
            vec![
                Port(i1, 0),
                Port(i2, 0),
                Port(i3, 0),
                Port(7, 0),
                Port(i5, 0),
            ],
        ));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::EqualityConst(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&ia).is_none(),
            "check that the original Equality node has been removed"
        );

        assert!(
            diagram.nodes.get(&ic).is_none(),
            "check that the original constant node has been removed"
        );

        assert_eq!(
            diagram.nodes.len(),
            node_count + 4 - 2,
            "Node count should have changed, 4 new const, 2 deleted nodes"
        );
    }

    #[test]
    fn test_equality_const_var() {
        // Create a simple diagram with a single constant Equality interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4)));
        let ia = diagram.add_node(Node::Equality(
            vec![Variable::new(34)],
            vec![
                Port(i1, 0),
                Port(i2, 0),
                Port(i3, 0),
                Port(7, 0),
                Port(i5, 0),
            ],
        ));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::EqualityConst(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Equality(_, ports) => {
                    assert_eq!(ports.len(), 1);
                    assert_eq!(ports[0], Port(ic, 0));
                }
                _ => panic!("Node is not a Equality node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count + 4,
            "Node count should have changed, 4 new const, 0 deleted nodes"
        );
    }

    #[test]
    fn test_addmul_unrestricted() {
        // Create a simple diagram with a single unrestricted addition interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0),
            Port(i2, 0),
            Port(i3, 0),
            Port(7, 0),
            Port(i5, 0),
        ]));
        let ic = diagram.add_node(Node::Unrestricted(Port(6, 3)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::AddMulUnrestricted(ia, 3));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&ia).is_none(),
            "check that the original Equality node has been removed"
        );

        assert!(
            diagram.nodes.get(&ic).is_none(),
            "check that the original constant node has been removed"
        );

        assert_eq!(
            diagram.nodes.len(),
            node_count + 4 - 2,
            "Node count should have changed, 4 new const, 2 deleted nodes"
        );
    }

    #[test]
    fn test_addition_const_unrestricted_1() {
        // Create a simple diagram with a single unrestricted addition interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let ia = diagram.add_node(Node::AddConstant(BigInt::from(15), Port(i1, 0), Port(4, 0)));
        diagram.add_node(Node::Unrestricted(Port(3, 1)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::DeleteConstOpUnrestricted(ia, 0),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&ia).is_none(),
            "check that the original AddConstant node has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have one less"
        );
    }

    #[test]
    fn test_addition_const_unrestricted_2() {
        // Create a simple diagram with a single unrestricted addition interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let ia = diagram.add_node(Node::AddConstant(BigInt::from(15), Port(i1, 0), Port(4, 0)));
        diagram.add_node(Node::Unrestricted(Port(3, 1)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(
            &mut diagram,
            &(),
            RewriteRule::DeleteConstOpUnrestricted(ia, 1),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        assert!(
            diagram.nodes.get(&ia).is_none(),
            "check that the original AddConstant node has been removed"
        );

        assert_eq!(
            diagram.nodes.len() + 1,
            node_count,
            "Node count should have one less"
        );
    }

    #[test]
    fn test_swap_add_and_multiply_constants_hd_tl() {
        // Create a simple diagram with a single unrestricted addition interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 1)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let ia = diagram.add_node(Node::AddConstant(BigInt::from(10), Port(5, 1), Port(i1, 0)));
        diagram.add_node(Node::Unrestricted(Port(7, 0)));
        let im = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(6, 0),
            Port(ia, 0),
        ));
        diagram.add_node(Node::Unrestricted(Port(im, 0)));
        diagram.add_node(Node::Unrestricted(Port(4, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::SwapAddMulConstantsHdTl(ia));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(15));
                    assert_eq!(port1, &Port(5, 1));
                    assert_eq!(port2, &Port(i1, 0));
                }
                _ => panic!("Node is not a MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&im) {
            match node {
                Node::AddConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(150));
                    assert_eq!(port1, &Port(6, 0));
                    assert_eq!(port2, &Port(ia, 0));
                }
                _ => panic!("Node is not a MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }

    #[test]
    fn test_swap_add_and_multiply_constants_tl_tl() {
        // Create a simple diagram with a single unrestricted addition interaction and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        diagram.add_node(Node::Unrestricted(Port(2, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0)));
        let ia = diagram.add_node(Node::AddConstant(BigInt::from(10), Port(i1, 0), Port(5, 1)));
        diagram.add_node(Node::Unrestricted(Port(7, 0)));
        let im = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(6, 0),
            Port(ia, 1),
        ));
        diagram.add_node(Node::Unrestricted(Port(im, 0)));
        diagram.add_node(Node::Unrestricted(Port(4, 0)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        apply_rewrite_step(&mut diagram, &(), RewriteRule::SwapAddMulConstantsTlTl(ia));

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(15));
                    assert_eq!(port1, &Port(im, 1));
                    assert_eq!(port2, &Port(i1, 0));
                }
                _ => panic!("Node is not a MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&im) {
            match node {
                Node::AddConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(-150));
                    assert_eq!(port1, &Port(6, 0));
                    assert_eq!(port2, &Port(ia, 0));
                }
                _ => panic!("Node is not a MultiplyConstant node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        assert_eq!(
            diagram.nodes.len(),
            node_count,
            "Node count should not have changed"
        );
    }
}
