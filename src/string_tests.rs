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

        let mut def_vec = vec![];
        def_vec.push(Definition(LetBinding(
            TPat {
                v: Pat::Variable(Variable { id: 11, name: None }),
                t: None,
            },
            Box::new(TExpr {
                v: Expr::Constant(BigInt::from(0)),
                t: None,
            }),
        )));
        def_vec.push(Definition(LetBinding(
            TPat {
                v: Pat::Variable(Variable { id: 12, name: None }),
                t: None,
            },
            Box::new(TExpr {
                v: Expr::Constant(BigInt::from(0)),
                t: None,
            }),
        )));
        def_vec.push(Definition(LetBinding(
            TPat {
                v: Pat::Variable(Variable { id: 13, name: None }),
                t: None,
            },
            Box::new(TExpr {
                v: Expr::Constant(BigInt::from(0)),
                t: None,
            }),
        )));
        def_vec.push(Definition(LetBinding(
            TPat {
                v: Pat::Variable(Variable { id: 14, name: None }),
                t: None,
            },
            Box::new(TExpr {
                v: Expr::Constant(BigInt::from(0)),
                t: None,
            }),
        )));
        def_vec.push(Definition(LetBinding(
            TPat {
                v: Pat::Variable(Variable { id: 56, name: None }),
                t: None,
            },
            Box::new(TExpr {
                v: Expr::Constant(BigInt::from(0)),
                t: None,
            }),
        )));
        def_vec.push(Definition(LetBinding(
            TPat {
                v: Pat::Variable(Variable { id: 58, name: None }),
                t: None,
            },
            Box::new(TExpr {
                v: Expr::Constant(BigInt::from(0)),
                t: None,
            }),
        )));

        let str_diag = build_string_diagram(
            test_expr,
            &input_ids,
            &mut DefinitionRegistry::new(&mut def_vec),
        );

        println!("{str_diag:?}");

        assert!(
            str_diag.is_well_formed(),
            "Test that the translated diagram is well formed"
        );
    }

    #[test]
    fn test_fuse_equality_nodes_basic() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 2, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(5, 0, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(5, 2, 3)));
        let addr1 = diagram.add_node(Node::Equality(
            vec![Variable::new(1)],
            vec![Port(i1, 0, 0), Port(5, 1, 4), Port(i2, 0, 1)],
        ));
        let addr2 = diagram.add_node(Node::Equality(
            vec![Variable::new(2)],
            vec![Port(i3, 0, 2), Port(4, 1, 4), Port(i4, 0, 3)],
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseEquality(addr1, 1),
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 2, 1)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(5, 1, 2)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(5, 2, 3)));
        let addr1 = diagram.add_node(Node::Addition(vec![
            Port(i1, 0, 0),
            Port(5, 0, 4),
            Port(i2, 0, 1),
        ]));
        let addr2 = diagram.add_node(Node::Addition(vec![
            Port(addr1, 1, 4),
            Port(i4, 0, 2),
            Port(i5, 0, 3),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseAddition(addr1, 1),
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 2, 1)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(5, 1, 2)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(5, 2, 3)));
        let addr1 = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0, 0),
            Port(5, 0, 4),
            Port(i2, 0, 1),
        ]));
        let addr2 = diagram.add_node(Node::Multiplication(vec![
            Port(addr1, 1, 4),
            Port(i4, 0, 2),
            Port(i5, 0, 3),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseMultiplication(addr1, 1),
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(4, 2, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(4, 3, 3)));
        let addr1 = diagram.add_node(Node::Addition(vec![
            Port(i1, 0, 0),
            Port(i2, 0, 1),
            Port(i3, 0, 2),
            Port(i4, 0, 3),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Split the node
        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::SplitAddition(
                addr1,
                Port(i1, 0, 0),
                Port(i2, 0, 1),
                vec![Port(i3, 0, 2), Port(i4, 0, 3)],
            ),
        );

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
                Port(i1, 0, 0),
                "Modified node should have same head as original node"
            );
            assert_eq!(ports[1], Port(i2, 0, 1), "Left arg should be unchanged");
            assert_eq!(
                ports[2],
                Port(new_address, 0, 100),
                "Right arg should point at new node"
            );
        } else {
            panic!("Node not properly modified");
        }

        // Assert that a new node has been added
        if let Some(Node::Addition(ports)) = diagram.nodes.get(&new_address) {
            assert_eq!(ports.len(), 3, "New node should only have three ports");
            assert_eq!(
                ports[0],
                Port(addr1, 2, 100),
                "Modified node should have same head as original node"
            );
            assert_eq!(
                ports[1],
                Port(i3, 0, 2),
                "Left arg should be second last orig arg"
            );
            assert_eq!(
                ports[2],
                Port(i4, 0, 3),
                "Right arg should be last orig arg"
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(4, 2, 2)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(4, 3, 3)));
        let addr1 = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0, 0),
            Port(i2, 0, 1),
            Port(i3, 0, 2),
            Port(i4, 0, 3),
        ]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        // Split the node
        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::SplitMultiplication(
                addr1,
                Port(i1, 0, 0),
                Port(i2, 0, 1),
                vec![Port(i3, 0, 2), Port(i4, 0, 3)],
            ),
        );

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
                Port(i1, 0, 0),
                "Modified node should have same head as original node"
            );
            assert_eq!(ports[1], Port(i2, 0, 1), "Left arg should be unchanged");
            assert_eq!(
                ports[2],
                Port(new_address, 0, 100),
                "Right arg should point at new node"
            );
        } else {
            panic!("Node not properly modified");
        }

        // Assert that a new node has been added
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&new_address) {
            assert_eq!(ports.len(), 3, "New node should only have three ports");
            assert_eq!(
                ports[0],
                Port(addr1, 2, 100),
                "Modified node should have same head as original node"
            );
            assert_eq!(
                ports[1],
                Port(i3, 0, 2),
                "Left arg should be second last orig arg"
            );
            assert_eq!(
                ports[2],
                Port(i4, 0, 3),
                "Right arg should be last orig arg"
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0, 0)));
        let i3 = diagram.add_node(Node::Constant(BigInt::from(55), Port(3, 0, 1)));
        let i4 = diagram.add_node(Node::Constant(BigInt::from(55), Port(i3, 0, 1)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0, 0)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        let i4 = diagram.add_node(Node::Constant(BigInt::from(55), Port(i3, 0, 1)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0, 0)));
        let i3: usize = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(i3, 0, 1)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
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
        let i1 = diagram.add_node(Node::Constant(BigInt::from(16), Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 1, 1)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(32),
            Port(i1, 0, 0),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AddConstantConstantHead(i3),
        );

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
                    assert_eq!(port, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Constant(BigInt::from(16), Port(2, 1, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0, 1)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(32),
            Port(i2, 0, 1),
            Port(i1, 0, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AddConstantConstantTail(i3),
        );

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
                    assert_eq!(port, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Constant(BigInt::from(15), Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 1, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(3),
            Port(i1, 0, 0),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MulConstantConstantHead(i3),
        );

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
                    assert_eq!(port, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Constant(BigInt::from(16), Port(2, 1, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(32),
            Port(i2, 0, 1),
            Port(i1, 0, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MulConstantConstantTail(i3),
        );

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
                    assert_eq!(port, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Constant(BigInt::from(6), Port(2, 1, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0, 1)));
        let i3 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(3),
            Port(i2, 0, 1),
            Port(i1, 0, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::ExpConstantConstantTail(i3),
        );

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
                    assert_eq!(port, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 1, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(2, 0, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(0),
            Port(i2, 0, 1),
            Port(i1, 0, 0),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MulConstantZero(i3, (Port(i1, 0, 0), Port(i2, 0, 1)), i3 + 1),
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
                    assert_eq!(port, &Port(i2, 0, 1));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address i3 does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&(i3 + 1)) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i1, 0, 0));
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(0),
            Port(i2, 0, 2),
            Port(i1, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AddConstantZero(i3, (Port(i1, 0, 1), Port(i2, 0, 2))),
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(1),
            Port(i2, 0, 2),
            Port(i1, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MulConstantOne(i3, (Port(i1, 0, 1), Port(i2, 0, 2))),
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));
        let i3 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(1),
            Port(i2, 0, 2),
            Port(i1, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::ExpConstantOne(i3, (Port(i1, 0, 1), Port(i2, 0, 2))),
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));
        let i3 = diagram.add_node(Node::Addition(vec![Port(i2, 0, 2), Port(i1, 0, 1)]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::RemoveBinaryAddition(i3, (Port(i1, 0, 1), Port(i2, 0, 2))),
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));
        let i3 = diagram.add_node(Node::Multiplication(vec![Port(i2, 0, 2), Port(i1, 0, 1)]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::RemoveBinaryMultiplication(i3, (Port(i1, 0, 1), Port(i2, 0, 2))),
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(i1, 0, 0),
            Port(3, 0, 2),
        ));
        let i4 = diagram.add_node(Node::AddConstant(
            BigInt::from(12),
            Port(2, 1, 2),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseAdditionByConstantHdTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::AddConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(27));
                    assert_eq!(port1, &Port(i1, 0, 0));
                    assert_eq!(port2, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 1, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(3, 0, 2),
            Port(i1, 0, 0),
        ));
        let i4 = diagram.add_node(Node::AddConstant(
            BigInt::from(12),
            Port(2, 0, 2),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseAdditionByConstantHdHd(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::AddConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(3));
                    assert_eq!(port1, &Port(i2, 0, 1));
                    assert_eq!(port2, &Port(i1, 0, 0));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        let i3 = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(i1, 0, 0),
            Port(3, 1, 2),
        ));
        let i4 = diagram.add_node(Node::AddConstant(
            BigInt::from(12),
            Port(i2, 0, 1),
            Port(2, 1, 2),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseAdditionByConstantTlTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::AddConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(3));
                    assert_eq!(port1, &Port(i1, 0, 0));
                    assert_eq!(port2, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(10),
            Port(i1, 0, 0),
            Port(3, 0, 2),
        ));
        let i4 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(12),
            Port(2, 1, 2),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseMultiplicationByConstantHdTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(120));
                    assert_eq!(port1, &Port(i1, 0, 0));
                    assert_eq!(port2, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 1, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(3),
            Port(3, 0, 2),
            Port(i1, 0, 0),
        ));
        let i4 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(2, 0, 2),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseMultiplicationByConstantHdHd(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(5));
                    assert_eq!(port1, &Port(i1, 0, 0));
                    assert_eq!(port2, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        let i3 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(3),
            Port(i1, 0, 0),
            Port(3, 1, 2),
        ));
        let i4 = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(i2, 0, 1),
            Port(2, 1, 2),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseMultiplicationByConstantTlTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(5));
                    assert_eq!(port1, &Port(i2, 0, 1));
                    assert_eq!(port2, &Port(i1, 0, 0));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        let i3 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(10),
            Port(i1, 0, 0),
            Port(3, 0, 2),
        ));
        let i4 = diagram.add_node(Node::ExponentiateConstant(
            BigInt::from(12),
            Port(2, 1, 2),
            Port(i2, 0, 1),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::FuseExponentiationByConstantHdTl(i3),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::ExponentiateConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(120));
                    assert_eq!(port1, &Port(i1, 0, 0));
                    assert_eq!(port2, &Port(i2, 0, 1));
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
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0, 0)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(3, 2, 2)));
        let addr1 = diagram.add_node(Node::Equality(
            vec![Variable::new(1)],
            vec![Port(i1, 0, 0), Port(i2, 0, 1), Port(i3, 0, 2)],
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(4, 1, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));
        let i3 = diagram.add_node(Node::Equality(vec![], vec![Port(i2, 0, 2), Port(i1, 0, 1)]));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::RemoveBinaryEquality(i3, (Port(i1, 0, 1), Port(i2, 0, 2))),
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2, 3)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 1, 5)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(8, 0, 6),
            Port(i5, 0, 4),
        ]));
        let ic = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(ia, 3, 6),
            Port(i4, 0, 5),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AdditionConstAddition(ia, 3, 6, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Addition(ports) => {
                    assert_eq!(ports[0], Port(ic, 1, 100));
                    assert_eq!(ports[1], Port(i2, 0, 2));
                    assert_eq!(ports[2], Port(i3, 0, 3));
                    assert_eq!(ports[3], Port(i4, 0, 5));
                    assert_eq!(ports[4], Port(i5, 0, 4));
                }
                _ => panic!("Node is not an Addition node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::AddConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0, 1));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2, 3)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 0, 4)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4, 5)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(8, 1, 6),
            Port(i5, 0, 5),
        ]));
        let ic = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(i4, 0, 4),
            Port(ia, 3, 6),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AdditionConstAddition(ia, 3, 6, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Addition(ports) => {
                    assert_eq!(ports[0], Port(ic, 1, 100));
                    assert_eq!(ports[1], Port(i2, 0, 2));
                    assert_eq!(ports[2], Port(i3, 0, 3));
                    assert_eq!(ports[3], Port(i4, 0, 4));
                    assert_eq!(ports[4], Port(i5, 0, 5));
                }
                _ => panic!("Node is not an Addition node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::AddConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0, 1));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2, 3)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 1, 4)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4, 5)));
        let ia = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(8, 0, 6),
            Port(i5, 0, 5),
        ]));
        let ic = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(ia, 3, 6),
            Port(i4, 0, 4),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MultiplicationConstMultiplication(ia, 3, 6, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Multiplication(ports) => {
                    assert_eq!(ports[0], Port(ic, 1, 100));
                    assert_eq!(ports[1], Port(i2, 0, 2));
                    assert_eq!(ports[2], Port(i3, 0, 3));
                    assert_eq!(ports[3], Port(i4, 0, 4));
                    assert_eq!(ports[4], Port(i5, 0, 5));
                }
                _ => panic!("Node is not an Multiplication node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::MultiplyConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0, 1));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(1, 0, 0)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(7, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(7, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(7, 2, 3)));
        let i4 = diagram.add_node(Node::Unrestricted(Port(8, 0, 4)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(7, 4, 5)));
        let ia = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(8, 1, 6),
            Port(i5, 0, 5),
        ]));
        let ic = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(i4, 0, 4),
            Port(ia, 3, 6),
        ));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MultiplicationConstMultiplication(ia, 3, 6, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Multiplication(ports) => {
                    assert_eq!(ports[0], Port(ic, 1, 100));
                    assert_eq!(ports[1], Port(i2, 0, 2));
                    assert_eq!(ports[2], Port(i3, 0, 3));
                    assert_eq!(ports[3], Port(i4, 0, 4));
                    assert_eq!(ports[4], Port(i5, 0, 5));
                }
                _ => panic!("Node is not an Multiplication node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::MultiplyConstant(val, port1, port2) => {
                    assert_eq!(port1, &Port(i1, 0, 1));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2, 3)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(7, 0, 5),
            Port(i5, 0, 4),
        ]));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3, 5)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AdditionConst(ia, 3, 5, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Addition(ports) => {
                    assert_eq!(ports[0], Port(ic, 1, 100));
                    assert_eq!(ports[1], Port(i2, 0, 2));
                    assert_eq!(ports[2], Port(i3, 0, 3));
                    assert_eq!(ports[3], Port(i5, 0, 4));
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
                    assert_eq!(port1, &Port(i1, 0, 1));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2, 3)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4, 4)));
        let ia = diagram.add_node(Node::Multiplication(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(7, 0, 5),
            Port(i5, 0, 4),
        ]));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3, 5)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::MultiplicationConst(ia, 3, 5, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Multiplication(ports) => {
                    assert_eq!(ports[0], Port(ic, 1, 100));
                    assert_eq!(ports[1], Port(i2, 0, 2));
                    assert_eq!(ports[2], Port(i3, 0, 3));
                    assert_eq!(ports[3], Port(i5, 0, 4));
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
                    assert_eq!(port1, &Port(i1, 0, 1));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2, 3)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4, 4)));
        let ia = diagram.add_node(Node::Equality(
            vec![],
            vec![
                Port(i1, 0, 1),
                Port(i2, 0, 2),
                Port(i3, 0, 3),
                Port(7, 0, 5),
                Port(i5, 0, 4),
            ],
        ));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3, 5)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::EqualityConst(ia, 3, ic, vec![], ic + 1, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the contents of all the constant nodes
        if let Some(node) = diagram.nodes.get(&(ic + 1)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i1, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 2)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i2, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 3)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i3, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 4)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i5, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        // Check the variable ids of the outlying nodes
        if let Some(node) = diagram.nodes.get(&i1) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 1, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i2) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 2, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 3, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i5) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 4, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

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
        diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2, 3)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4, 4)));
        let ia = diagram.add_node(Node::Equality(
            vec![Variable::new(34)],
            vec![
                Port(i1, 0, 1),
                Port(i2, 0, 2),
                Port(i3, 0, 3),
                Port(7, 0, 5),
                Port(i5, 0, 4),
            ],
        ));
        let ic = diagram.add_node(Node::Constant(BigInt::from(15), Port(6, 3, 5)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::EqualityConst(ia, 3, ic, vec![], ic + 1, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check taht the equality node now only has one inhabitant
        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::Equality(_, ports) => {
                    assert_eq!(ports.len(), 1);
                    assert_eq!(ports[0], Port(ic, 0, 5));
                }
                _ => panic!("Node is not a Equality node."),
            }
        } else {
            panic!("Node at address ia does not exist.");
        }

        // Check the contents of all the constant nodes
        if let Some(node) = diagram.nodes.get(&(ic + 1)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i1, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 2)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i2, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 3)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i3, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 4)) {
            match node {
                Node::Constant(val, port) => {
                    assert_eq!(port, &Port(i5, 0, 5));
                    assert_eq!(val, &BigInt::from(15));
                }
                _ => panic!("Node is not a Constant node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        // Check the variable ids of the outlying nodes
        if let Some(node) = diagram.nodes.get(&i1) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 1, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i2) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 2, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 3, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i5) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 4, 0, 5));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
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
        diagram.add_node(Node::Unrestricted(Port(4, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(6, 0, 1)));
        let i2 = diagram.add_node(Node::Unrestricted(Port(6, 1, 2)));
        let i3 = diagram.add_node(Node::Unrestricted(Port(6, 2, 3)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let i5 = diagram.add_node(Node::Unrestricted(Port(6, 4, 4)));
        let ia = diagram.add_node(Node::Addition(vec![
            Port(i1, 0, 1),
            Port(i2, 0, 2),
            Port(i3, 0, 3),
            Port(7, 0, 5),
            Port(i5, 0, 4),
        ]));
        let ic = diagram.add_node(Node::Unrestricted(Port(6, 3, 5)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::AddMulUnrestricted(ia, 3, vec![], ic, ic + 1),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        // Check the contents of all the Unrestricted nodes
        if let Some(node) = diagram.nodes.get(&(ic + 1)) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i1, 0, 1));
                }
                _ => panic!("Node is not a Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 2)) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i2, 0, 2));
                }
                _ => panic!("Node is not a Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 3)) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i3, 0, 3));
                }
                _ => panic!("Node is not a Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&(ic + 4)) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i5, 0, 4));
                }
                _ => panic!("Node is not a Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

        // Check the variable ids of the outlying nodes
        if let Some(node) = diagram.nodes.get(&i1) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 1, 0, 1));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i2) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 2, 0, 2));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i3) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 3, 0, 3));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i5) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic + 4, 0, 4));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

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
        diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let ia = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(i1, 0, 1),
            Port(4, 0, 2),
        ));
        let ic = diagram.add_node(Node::Unrestricted(Port(3, 1, 2)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::DeleteConstOpUnrestricted(ia, 0),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i1, 0, 2));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i1) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic, 0, 2));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

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
        diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let ia = diagram.add_node(Node::AddConstant(
            BigInt::from(15),
            Port(i1, 0, 1),
            Port(4, 0, 2),
        ));
        let ic = diagram.add_node(Node::Unrestricted(Port(3, 1, 2)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::DeleteConstOpUnrestricted(ia, 1),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ic) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(i1, 0, 1));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }
        if let Some(node) = diagram.nodes.get(&i1) {
            match node {
                Node::Unrestricted(port) => {
                    assert_eq!(port, &Port(ic, 0, 1));
                }
                _ => panic!("Node is not an Unrestricted node."),
            }
        } else {
            panic!("Node at address ic does not exist.");
        }

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
        diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 1, 1)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let ia = diagram.add_node(Node::AddConstant(
            BigInt::from(10),
            Port(5, 1, 5),
            Port(i1, 0, 1),
        ));
        diagram.add_node(Node::Unrestricted(Port(7, 0, 2)));
        let im = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(6, 0, 6),
            Port(ia, 0, 5),
        ));
        let i6 = diagram.add_node(Node::Unrestricted(Port(im, 0, 6)));
        diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::SwapAddMulConstantsHdTl(ia, 1, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(15));
                    assert_eq!(port1, &Port(im, 1, 100));
                    assert_eq!(port2, &Port(i1, 0, 1));
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
                    assert_eq!(port1, &Port(i6, 0, 6));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
        diagram.add_node(Node::Unrestricted(Port(2, 0, 0)));
        let i1 = diagram.add_node(Node::Unrestricted(Port(3, 0, 1)));
        diagram.add_node(Node::Unrestricted(Port(0, 0, 0)));
        let ia = diagram.add_node(Node::AddConstant(
            BigInt::from(10),
            Port(i1, 0, 1),
            Port(5, 1, 5),
        ));
        diagram.add_node(Node::Unrestricted(Port(7, 0, 2)));
        let im = diagram.add_node(Node::MultiplyConstant(
            BigInt::from(15),
            Port(6, 0, 6),
            Port(ia, 1, 5),
        ));
        let i6 = diagram.add_node(Node::Unrestricted(Port(im, 0, 6)));
        diagram.add_node(Node::Unrestricted(Port(4, 0, 2)));

        assert!(
            diagram.is_well_formed(),
            "Check that the starting diagram makes sense"
        );

        let node_count = diagram.nodes.len();

        let mut reg = DefinitionRegistry::new(&mut vec![]);
        reg.next_id = 100;
        apply_rewrite_step(
            &mut diagram,
            &mut reg,
            &(),
            RewriteRule::SwapAddMulConstantsTlTl(ia, 1, BigInt::from(15)),
        );

        assert!(
            diagram.is_well_formed(),
            "Check that the diagram still makes sense"
        );

        if let Some(node) = diagram.nodes.get(&ia) {
            match node {
                Node::MultiplyConstant(value, port1, port2) => {
                    assert_eq!(value, &BigInt::from(15));
                    assert_eq!(port1, &Port(im, 1, 100));
                    assert_eq!(port2, &Port(i1, 0, 1));
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
                    assert_eq!(port1, &Port(i6, 0, 6));
                    assert_eq!(port2, &Port(ia, 0, 100));
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
