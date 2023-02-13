use pest::Parser;

use super::*;

fn ident(name: &str) -> Expression {
    Expression::Identifier(name.to_string())
}

fn bident(name: &str) -> Box<Expression> {
    Box::new(ident(name))
}

#[test]
fn parse_identifier() {
    let input = "test_099123__";
    let mut result = BtParser::parse(Rule::Identifier, input).unwrap();

    assert_eq!(result.next().unwrap().as_str(), "test_099123__");
}

#[test]
fn parse_identifier_cant_start_with_number() {
    let input = "0test";
    let result = BtParser::parse(Rule::Identifier, input);

    assert!(result.is_err());
}

#[test]
fn parse_expression_string() {
    let test_data = [
        // Removes quotes
        (r#""test""#, "test"),
        // Escapes quotes and backslashes
        (r#""a\"b""#, "a\"b"),
        (r#""a\\\"b""#, "a\\\"b"),
        (r#"" a \\\" b \" c \\""#, r#" a \" b " c \"#),
        // Hex escapes
        (r#""a\x01c\x02b""#, "a\x01c\x02b"),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, Expression::String(expected.as_bytes().to_vec()))
    }
}

#[test]
fn parse_expression_numbers() {
    let test_data = [
        ("91247", Number::I32(91247)),
        ("91247u", Number::U32(91247)),
        ("7987421L", Number::I64(7987421)),
        ("79132413uL", Number::U64(79132413)),
        ("709251Lu", Number::U64(709251)),
        ("0b0100100101", Number::I32(0b0100100101)),
        ("0x79adfeb", Number::I32(0x79adfeb)),
        ("71afh", Number::I32(0x71af)),
        ("073412234", Number::I32(0o73412234)),
        ("0o0761234", Number::I32(0o0761234)),
        ("1234.12", Number::F64(1234.12)),
        ("973214.4f", Number::F32(973214.4)),
        ("41379.12e-2", Number::F64(41379.12e-2)),
        ("true", Number::Bool(true)),
        ("false", Number::Bool(false)),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, Expression::Number(expected))
    }
}

#[test]
fn parse_expression_call_function() {
    let test_data = [
        (
            "a()",
            Expression::CallFunction {
                target: bident("a"),
                arguments: Vec::new(),
            },
        ),
        (
            "a(b)",
            Expression::CallFunction {
                target: bident("a"),
                arguments: vec![ident("b")],
            },
        ),
        (
            "a(b, c, d, e)",
            Expression::CallFunction {
                target: bident("a"),
                arguments: vec![ident("b"), ident("c"), ident("d"), ident("e")],
            },
        ),
        (
            "a(b,c)(d)",
            Expression::CallFunction {
                target: Box::new(Expression::CallFunction {
                    target: bident("a"),
                    arguments: vec![ident("b"), ident("c")],
                }),
                arguments: vec![ident("d")],
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, expected)
    }
}

#[test]
fn parse_expression_get_array_index() {
    let test_data = [
        (
            "a[b]",
            Expression::GetArrayIndex {
                target: Box::new(ident("a")),
                index: Box::new(ident("b")),
            },
        ),
        (
            "a[b][c]",
            Expression::GetArrayIndex {
                target: Box::new(Expression::GetArrayIndex {
                    target: bident("a"),
                    index: bident("b"),
                }),
                index: bident("c"),
            },
        ),
        (
            "a.b[c]",
            Expression::GetArrayIndex {
                target: Box::new(Expression::GetMember {
                    target: bident("a"),
                    name: "b".to_string(),
                }),
                index: Box::new(ident("c")),
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, expected)
    }
}

#[test]
fn parse_expression_get_member() {
    let test_data = [
        (
            "a.b",
            Expression::GetMember {
                target: bident("a"),
                name: "b".to_string(),
            },
        ),
        (
            "a.b.c",
            Expression::GetMember {
                target: Box::new(Expression::GetMember {
                    target: bident("a"),
                    name: "b".to_string(),
                }),
                name: "c".to_string(),
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, expected)
    }
}

#[test]
fn parse_expression_ternary() {
    let test_data = [
        (
            "a ? b : c",
            Expression::Ternary {
                condition: bident("a"),
                true_expression: bident("b"),
                false_expression: bident("c"),
            },
        ),
        (
            "a ? b : c ? d : e",
            Expression::Ternary {
                condition: bident("a"),
                true_expression: bident("b"),
                false_expression: Box::new(Expression::Ternary {
                    condition: bident("c"),
                    true_expression: bident("d"),
                    false_expression: bident("e"),
                }),
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, expected)
    }
}

#[test]
fn parse_expression_binary() {
    let test_data = [
        (
            "a += b",
            Expression::Binary {
                operator: BinaryOperator::AssignAdd,
                lhs: bident("a"),
                rhs: bident("b"),
            },
        ),
        (
            "a << b",
            Expression::Binary {
                operator: BinaryOperator::LeftShift,
                lhs: bident("a"),
                rhs: bident("b"),
            },
        ),
        (
            "a + b - c",
            Expression::Binary {
                operator: BinaryOperator::Subtract,
                lhs: Box::new(Expression::Binary {
                    operator: BinaryOperator::Add,
                    lhs: bident("a"),
                    rhs: bident("b"),
                }),
                rhs: bident("c"),
            },
        ),
        (
            "a = b = c",
            Expression::Binary {
                operator: BinaryOperator::Assign,
                lhs: bident("a"),
                rhs: Box::new(Expression::Binary {
                    operator: BinaryOperator::Assign,
                    lhs: bident("b"),
                    rhs: bident("c"),
                }),
            },
        ),
        (
            "chunkLen/3",
            Expression::Binary {
                operator: BinaryOperator::Divide,
                lhs: bident("chunkLen"),
                rhs: Box::new(Expression::Number(Number::I32(3))),
            },
        ),
        (
            "a || b",
            Expression::Binary {
                operator: BinaryOperator::LogicalOr,
                lhs: bident("a"),
                rhs: bident("b"),
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_expression(input, expected)
    }
}

#[test]
fn parse_statement_declaration() {
    let test_data = [
        (
            "int a;",
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "a".to_string(),
                args: Vec::new(),
                attributes: Vec::new(),
            },
        ),
        (
            "local int a = 1;",
            Statement::DeclareAndAssign {
                local: true,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "a".to_string(),
                attributes: Vec::new(),
                value: Expression::Number(Number::I32(1)),
            },
        ),
        (
            "int a[1];",
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Array {
                    name: "int".to_string(),
                    size: Expression::Number(Number::I32(1)),
                },
                name: "a".to_string(),
                args: Vec::new(),
                attributes: Vec::new(),
            },
        ),
        (
            "int a[b] = c;",
            Statement::DeclareAndAssign {
                local: false,
                object_type: TypeDeclaration::Array {
                    name: "int".to_string(),
                    size: ident("b"),
                },
                name: "a".to_string(),
                attributes: Vec::new(),
                value: ident("c"),
            },
        ),
        (
            "int a[b] = {c, d};",
            Statement::DeclareAndAssign {
                local: false,
                object_type: TypeDeclaration::Array {
                    name: "int".to_string(),
                    size: ident("b"),
                },
                name: "a".to_string(),
                attributes: Vec::new(),
                value: Expression::DeclareArrayValue(vec![ident("c"), ident("d")]),
            },
        ),
        (
            "int a = 1, b[2] = {1, 2}, c;",
            Statement::DeclareMultiple(vec![
                Statement::DeclareAndAssign {
                    local: false,
                    object_type: TypeDeclaration::Normal("int".to_string()),
                    name: "a".to_string(),
                    attributes: Vec::new(),
                    value: Expression::Number(Number::I32(1)),
                },
                Statement::DeclareAndAssign {
                    local: false,
                    object_type: TypeDeclaration::Array {
                        name: "int".to_string(),
                        size: Expression::Number(Number::I32(2)),
                    },
                    name: "b".to_string(),
                    attributes: Vec::new(),
                    value: Expression::DeclareArrayValue(vec![
                        Expression::Number(Number::I32(1)),
                        Expression::Number(Number::I32(2)),
                    ]),
                },
                Statement::Declare {
                    local: false,
                    object_type: TypeDeclaration::Normal("int".to_string()),
                    name: "c".to_string(),
                    args: Vec::new(),
                    attributes: Vec::new(),
                },
            ]),
        ),
        (
            "PNG_PALETTE_PIXEL plteChunkData[chunkLen/3];",
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Array {
                    name: "PNG_PALETTE_PIXEL".to_string(),
                    size: Expression::Binary {
                        operator: BinaryOperator::Divide,
                        lhs: Box::new(Expression::Identifier("chunkLen".to_string())),
                        rhs: Box::new(Expression::Number(Number::I32(3))),
                    },
                },
                name: "plteChunkData".to_string(),
                args: Vec::new(),
                attributes: Vec::new(),
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_declare_attributes() {
    let test_data = [
        (
            "int a<>;",
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "a".to_string(),
                attributes: Vec::new(),
                args: Vec::new(),
            },
        ),
        (
            "int a<format=hex>;",
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "a".to_string(),
                args: Vec::new(),
                attributes: vec![Attribute {
                    name: "format".to_string(),
                    value: AttributeValue::Identifier("hex".to_string()),
                }],
            },
        ),
        (
            "int a<format=hex,bgcolor=0x111111> = 4;",
            Statement::DeclareAndAssign {
                local: false,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "a".to_string(),
                attributes: vec![
                    Attribute {
                        name: "format".to_string(),
                        value: AttributeValue::Identifier("hex".to_string()),
                    },
                    Attribute {
                        name: "bgcolor".to_string(),
                        value: AttributeValue::Number(Number::I32(0x111111)),
                    },
                ],
                value: Expression::Number(Number::I32(4)),
            },
        ),
        (
            "uint16 btPngSignature[4] <format=hex>;",
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Array {
                    name: "uint16".to_string(),
                    size: Expression::Number(Number::I32(4)),
                },
                name: "btPngSignature".to_string(),
                args: Vec::new(),
                attributes: vec![Attribute {
                    name: "format".to_string(),
                    value: AttributeValue::Identifier("hex".to_string()),
                }],
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_declare_function() {
    let test_data = [
        (
            "void a(){}",
            Statement::DeclareFunction {
                object_ref: ObjectRef::Function(0),
                return_type: TypeDeclaration::Normal("void".to_string()),
                args: Vec::new(),
                statements: Vec::new(),
            },
        ),
        (
            "int[10] a(){}",
            Statement::DeclareFunction {
                object_ref: ObjectRef::Function(0),
                return_type: TypeDeclaration::Array {
                    name: "int".to_string(),
                    size: Expression::Number(Number::I32(10)),
                },
                args: Vec::new(),
                statements: Vec::new(),
            },
        ),
        (
            "void a(int b, c &d) {}",
            Statement::DeclareFunction {
                object_ref: ObjectRef::Function(0),
                return_type: TypeDeclaration::Normal("void".to_string()),
                args: vec![
                    FunctionArg {
                        object_type: TypeDeclaration::Normal("int".to_string()),
                        name: "b".to_string(),
                        reference: false,
                    },
                    FunctionArg {
                        object_type: TypeDeclaration::Normal("c".to_string()),
                        name: "d".to_string(),
                        reference: true,
                    },
                ],
                statements: Vec::new(),
            },
        ),
        (
            "
                void a() {
                    int b;
                    b += 10;
                }
                "
            .trim(),
            Statement::DeclareFunction {
                object_ref: ObjectRef::Function(0),
                return_type: TypeDeclaration::Normal("void".to_string()),
                args: Vec::new(),
                statements: vec![
                    Statement::Declare {
                        local: false,
                        object_type: TypeDeclaration::Normal("int".to_string()),
                        name: "b".to_string(),
                        args: Vec::new(),
                        attributes: Vec::new(),
                    },
                    Statement::Expression(Expression::Binary {
                        operator: BinaryOperator::AssignAdd,
                        lhs: bident("b"),
                        rhs: Box::new(Expression::Number(Number::I32(10))),
                    }),
                ],
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_declare_enum() {
    let test_data = [
        (
            "enum A {};",
            Statement::DeclareEnum {
                instance_name: None,
                enum_type: None,
                variants: Vec::new(),
                object_ref: ObjectRef::Enum(0),
                attributes: Vec::new(),
            },
        ),
        (
            "
                typedef enum <int> A {
                    a = 1,
                    b,
                    c
                } a;
                ",
            Statement::DeclareEnum {
                instance_name: None,
                object_ref: ObjectRef::Enum(0),
                enum_type: Some("int".to_string()),
                variants: vec![
                    ("a".to_string(), Some(Expression::Number(Number::I32(1)))),
                    ("b".to_string(), None),
                    ("c".to_string(), None),
                ],
                attributes: Vec::new(),
            },
        ),
        (
            "enum A {} <bgcolor=0x111111>;",
            Statement::DeclareEnum {
                instance_name: None,
                object_ref: ObjectRef::Enum(0),
                enum_type: None,
                variants: Vec::new(),
                attributes: vec![Attribute {
                    name: "bgcolor".to_string(),
                    value: AttributeValue::Number(Number::I32(0x111111)),
                }],
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_declare_struct() {
    let test_data = [
        (
            "struct A {};",
            Statement::DeclareStruct {
                instance_name: None,
                object_ref: ObjectRef::Struct(0),
                args: Vec::new(),
                statements: Vec::new(),
                attributes: Vec::new(),
            },
        ),
        (
            "
                typedef struct A(int &a) {
                    local int b = 1;
                    int c;
                } d;
                ",
            Statement::DeclareStruct {
                instance_name: None,
                object_ref: ObjectRef::Struct(0),
                args: vec![FunctionArg {
                    object_type: TypeDeclaration::Normal("int".to_string()),
                    name: "a".to_string(),
                    reference: true,
                }],
                statements: vec![
                    Statement::DeclareAndAssign {
                        local: true,
                        object_type: TypeDeclaration::Normal("int".to_string()),
                        name: "b".to_string(),
                        attributes: Vec::new(),
                        value: Expression::Number(Number::I32(1)),
                    },
                    Statement::Declare {
                        local: false,
                        object_type: TypeDeclaration::Normal("int".to_string()),
                        name: "c".to_string(),
                        args: Vec::new(),
                        attributes: Vec::new(),
                    },
                ],
                attributes: Vec::new(),
            },
        ),
        (
            "struct A {} <bgcolor=0x111111>;",
            Statement::DeclareStruct {
                instance_name: None,
                object_ref: ObjectRef::Struct(0),
                args: Vec::new(),
                statements: Vec::new(),
                attributes: vec![Attribute {
                    name: "bgcolor".to_string(),
                    value: AttributeValue::Number(Number::I32(0x111111)),
                }],
            },
        ),
        ("struct A;", Statement::DeclareForwardStruct),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_return() {
    let test_data = [
        (
            "return 1;",
            Statement::Return(Expression::Number(Number::I32(1))),
        ),
        (
            "return -a;",
            Statement::Return(Expression::Unary(UnaryOperator::Minus, bident("a"))),
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_if() {
    let test_data = [
        (
            "if (true) {  }",
            Statement::If {
                condition: Expression::Number(Number::Bool(true)),
                statements: Vec::new(),
                else_ifs: Vec::new(),
                final_else: None,
            },
        ),
        (
            "if (1) { a = 1; } else if (2) { b = 2; } else { int a; }",
            Statement::If {
                condition: Expression::Number(Number::I32(1)),
                statements: vec![Statement::Expression(Expression::Binary {
                    operator: BinaryOperator::Assign,
                    lhs: bident("a"),
                    rhs: Box::new(Expression::Number(Number::I32(1))),
                })],
                else_ifs: vec![(
                    Expression::Number(Number::I32(2)),
                    vec![Statement::Expression(Expression::Binary {
                        operator: BinaryOperator::Assign,
                        lhs: bident("b"),
                        rhs: Box::new(Expression::Number(Number::I32(2))),
                    })],
                )],
                final_else: Some(vec![Statement::Declare {
                    local: false,
                    object_type: TypeDeclaration::Normal("int".to_string()),
                    name: "a".to_string(),
                    args: Vec::new(),
                    attributes: Vec::new(),
                }]),
            },
        ),
        (
            "if (true) a = b;",
            Statement::If {
                condition: Expression::Number(Number::Bool(true)),
                statements: vec![Statement::Expression(Expression::Binary {
                    operator: BinaryOperator::Assign,
                    lhs: bident("a"),
                    rhs: bident("b"),
                })],
                else_ifs: Vec::new(),
                final_else: None,
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_while() {
    let test_data = [
        (
            "while (true) {  }",
            Statement::While {
                condition: Expression::Number(Number::Bool(true)),
                statements: Vec::new(),
            },
        ),
        (
            "while (b/a) { a = 1; }",
            Statement::While {
                condition: Expression::Binary {
                    operator: BinaryOperator::Divide,
                    lhs: bident("b"),
                    rhs: bident("a"),
                },
                statements: vec![Statement::Expression(Expression::Binary {
                    operator: BinaryOperator::Assign,
                    lhs: bident("a"),
                    rhs: Box::new(Expression::Number(Number::I32(1))),
                })],
            },
        ),
        (
            "do {} while (true);",
            Statement::DoWhile {
                condition: Expression::Number(Number::Bool(true)),
                statements: Vec::new(),
            },
        ),
        (
            "do { a = 1; } while (b/a);",
            Statement::DoWhile {
                condition: Expression::Binary {
                    operator: BinaryOperator::Divide,
                    lhs: bident("b"),
                    rhs: bident("a"),
                },
                statements: vec![Statement::Expression(Expression::Binary {
                    operator: BinaryOperator::Assign,
                    lhs: bident("a"),
                    rhs: Box::new(Expression::Number(Number::I32(1))),
                })],
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_for() {
    let test_data = [
        (
            "for (;;) {  }",
            Statement::For {
                initialization: None,
                condition: None,
                increment: None,
                statements: Vec::new(),
            },
        ),
        (
            "for (i = 0; i < 10; i++) { j += 2; }",
            Statement::For {
                initialization: Some(Expression::Binary {
                    operator: BinaryOperator::Assign,
                    lhs: bident("i"),
                    rhs: Box::new(Expression::Number(Number::I32(0))),
                }),
                condition: Some(Expression::Binary {
                    operator: BinaryOperator::LessThan,
                    lhs: bident("i"),
                    rhs: Box::new(Expression::Number(Number::I32(10))),
                }),
                increment: Some(Expression::Unary(
                    UnaryOperator::SuffixIncrement,
                    bident("i"),
                )),
                statements: vec![Statement::Expression(Expression::Binary {
                    operator: BinaryOperator::AssignAdd,
                    lhs: bident("j"),
                    rhs: Box::new(Expression::Number(Number::I32(2))),
                })],
            },
        ),
    ];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_switch() {
    let test_data = [(
        "switch (a) { case 1: break; default: a; }",
        Statement::Switch {
            value: Expression::Identifier("a".to_string()),
            switches: vec![
                (
                    SwitchType::Expression(Expression::Number(Number::I32(1))),
                    vec![Statement::Break],
                ),
                (
                    SwitchType::Default,
                    vec![Statement::Expression(Expression::Identifier(
                        "a".to_string(),
                    ))],
                ),
            ],
        },
    )];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_statement_typedef() {
    let test_data = [(
        "typedef int a;",
        Statement::Typedef {
            original: "int".to_string(),
            alias: "a".to_string(),
        },
    )];
    for (input, expected) in test_data {
        test_parse_statement(input, expected)
    }
}

#[test]
fn parse_bt_basic() {
    let test_data = [(
        "
                int a;
                int b;
            ",
        vec![
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "a".to_string(),
                args: Vec::new(),
                attributes: Vec::new(),
            },
            Statement::Declare {
                local: false,
                object_type: TypeDeclaration::Normal("int".to_string()),
                name: "b".to_string(),
                args: Vec::new(),
                attributes: Vec::new(),
            },
        ],
    )];
    for (input, expected) in test_data {
        let result = parse_bt(input).unwrap();
        assert_eq!(result.statements, expected);
    }
}

#[test]
fn parse_png_bt() {
    let input = include_str!("../../test-resources/PNG.bt");
    parse_bt(input).unwrap();
}

fn test_parse_expression(input: &str, expected: Expression) {
    let parse_result = BtParser::parse(Rule::Expression, input.trim());

    let result = parse_expression(parse_result.unwrap());

    assert_eq!(result, expected);
}

fn test_parse_statement(input: &str, expected: Statement) {
    let parse_result = BtParser::parse(Rule::Statement, input.trim());

    let result = parse_statement(
        &mut ParseContext::default(),
        parse_result.unwrap().next().unwrap(),
    );

    assert_eq!(result, expected);
}
