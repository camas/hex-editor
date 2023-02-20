use std::collections::{HashMap, VecDeque};

use crate::parse::{
    AttributeValue, BasicFunction, BinaryOperator, Expression, FunctionArg, Number, ObjectRef,
    ParsedData, Statement, TypeDeclaration, UnaryOperator,
};

#[cfg(test)]
mod tests;

// Tried to pick darker colors so that if you manually go for brighter ones they'll stand out
// https://medialab.github.io/iwanthue/
const BACKGROUND_COLORS: [u32; 16] = [
    0x4a5e73, 0x364c1e, 0x492650, 0x4c7a51, 0x562027, 0x478184, 0x5c3e1d, 0x5b6f9f, 0x222c15,
    0x8d5f7b, 0x345648, 0x966151, 0x2d3050, 0x76734f, 0x1c3238, 0x47342f,
];

#[derive(thiserror::Error, Debug)]
pub enum TransformError {
    #[error("Struct with same name already defined: '{0}'")]
    StructAlreadyDefined(String),
    #[error("Cannot declare an unsized array")]
    DeclareUnsizedArray,
    #[error("Unknown object type '{0}'")]
    UnknownObjectType(String),
    #[error("Not an initializable type '{0:?}'")]
    NotAnInitializable(ObjectRef),
    #[error("Not a function '{0:?}'")]
    NotAFunction(ObjectRef),
    #[error("Variable '{0}' not declared")]
    VariableNotDeclared(String),
    #[error("Variable '{0}' already declared")]
    VariableAlreadyDeclared(String),
    #[error("Function '{0}' not declared")]
    FunctionNotDeclared(String),
    #[error("Can't assign to a non-local variable")]
    AssignNonLocalVariable,
    // TODO: Remove
    #[error("Generic error")]
    Generic(&'static str),
}

type TransformResult<T> = Result<T, TransformError>;

#[derive(Debug)]
pub struct BtProgram {
    pub functions: Vec<Function>,
    pub root_function_index: FunctionRef,
}

#[derive(Debug)]
pub struct Function {
    pub args: Vec<(ObjectRef, VariableRef)>,
    pub instructions: Vec<Instruction>,
    pub return_type: ObjectRef,
    pub label_offsets: Vec<usize>,
}

#[derive(Debug)]
pub enum Instruction {
    /// Pop single item from stack
    Pop,
    /// Pop to a variable
    PopVariable(VariableRef),
    // Push basic objects to stack
    PushVariable(VariableRef),
    PushString(Vec<u8>),
    PushWideString(Vec<u8>),
    PushBool(bool),
    PushChar(i8),
    PushU8(u8),
    PushU32(u32),
    PushI32(i32),
    PushU64(u64),
    PushI64(i64),
    PushF32(f32),
    PushF64(f64),
    /// Object declaration
    DeclareObject {
        variable_ref: VariableRef,
        object_ref: ObjectRef,
    },
    DeclareArray {
        variable_ref: VariableRef,
        object_ref: ObjectRef,
    },
    /// Object declaration by reading from data
    ReadObject {
        name: String,
        variable_ref: VariableRef,
        object_ref: ObjectRef,
        arg_count: usize,
        attributes: Vec<Attribute>,
    },
    ReadArray {
        name: String,
        variable_ref: VariableRef,
        object_ref: ObjectRef,
        attributes: Vec<Attribute>,
    },
    /// Cast top of stack to target object (Replaces)
    Cast(ObjectRef),
    /// Call a function. Args are on stack
    CallFunction {
        function_ref: FunctionRef,
        arg_count: usize,
    },
    CallBasicFunction {
        basic_function: BasicFunction,
        arg_count: usize,
    },
    Return,
    ReturnVoid,
    /// Gets item from an array
    /// Top of stack is the array, second top the index
    GetArrayIndex,
    /// Gets the member of the item on top of the stack
    GetMember(String),
    /// Array declaration values. Top n stack objects are the values
    DeclareArrayValues(usize),
    // Unary instructions. Acts on top stack value
    SuffixIncrement,
    SuffixDecrement,
    PrefixIncrement,
    PrefixDecrement,
    Positive,
    Negate,
    UnaryLogicalNot,
    UnaryBitwiseNot,
    // Binary instructions. Acts on top two stack values
    Multiply,
    Divide,
    Modulus,
    Add,
    Subtract,
    LeftShift,
    RightShift,
    LessThan,
    LessThanOrEqual,
    MoreThan,
    MoreThanOrEqual,
    Equal,
    NotEqual,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    Assign,
    AssignAdd,
    AssignSubtract,
    AssignMultiply,
    AssignDivide,
    AssignModulus,
    AssignLeftShift,
    AssignRightShift,
    AssignBitwiseAnd,
    AssignBitwiseXor,
    AssignBitwiseOr,
    Label(LabelRef),
    Jump(LabelRef),
    JumpTrue(LabelRef),
    JumpFalse(LabelRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableRef(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LabelRef(pub(crate) u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionRef(pub(crate) u64);

#[derive(Debug, Clone)]
pub enum Attribute {
    BackgroundColor(u32),
}

#[derive(Debug)]
pub struct TransformMessage {
    message: String,
    // TODO: Add location info
}

#[derive(Debug)]
struct TransformContext {
    stack: Vec<TransformContextStackItem>,
    variable_ref_count: u64,
    functions: HashMap<FunctionRef, Function>,
    object_refs: HashMap<String, ObjectRef>,
    queue: VecDeque<TransformItem>,
    messages: Vec<TransformMessage>,
    options: TransformOptions,
    background_color_index: usize,
}

#[derive(Debug)]
struct TransformContextStackItem {
    variables: HashMap<String, VariableRef>,
    label_ref_count: u64,
    function_ref: FunctionRef,
}

#[derive(Debug)]
struct TransformOptions {
    auto_add_colors: bool,
}

impl TransformContext {
    fn new(object_refs: HashMap<String, ObjectRef>) -> TransformContext {
        TransformContext {
            stack: Vec::new(),
            variable_ref_count: 0,
            functions: HashMap::new(),
            object_refs,
            queue: VecDeque::new(),
            messages: Vec::new(),
            background_color_index: 0,
            options: TransformOptions {
                auto_add_colors: true,
            },
        }
    }

    fn queue_statement(&mut self, statement: Statement) {
        self.queue
            .push_front(TransformItem::Statement(Box::new(statement)));
    }

    fn queue_expression(&mut self, expression: Expression) {
        self.queue
            .push_front(TransformItem::Expression(Box::new(expression)));
    }

    fn queue_instruction(&mut self, instruction: Instruction) {
        self.queue
            .push_front(TransformItem::Instruction(Box::new(instruction)));
    }

    fn queue_pop_stack(&mut self) {
        self.queue.push_front(TransformItem::PopStack);
    }

    fn curr_function_mut(&mut self) -> &mut Function {
        let cur_stack = self.stack.last().unwrap();
        self.functions.get_mut(&cur_stack.function_ref).unwrap()
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.curr_function_mut().instructions.push(instruction);
    }

    fn type_ref(&self, type_declaration: TypeDeclaration) -> TransformResult<ObjectRef> {
        Ok(match type_declaration {
            TypeDeclaration::Normal(name) if name == "void" => ObjectRef::Void,
            TypeDeclaration::Normal(name) => self.initializable_ref(name)?,
            TypeDeclaration::Array { .. } => todo!(),
            TypeDeclaration::UnsizedArray { .. } => todo!(),
        })
    }

    fn initializable_ref(&self, name: String) -> TransformResult<ObjectRef> {
        let object_ref = self
            .object_refs
            .get(&name)
            .cloned()
            .ok_or(TransformError::UnknownObjectType(name))?;

        if !matches!(
            object_ref,
            ObjectRef::Basic(_) | ObjectRef::Struct(_) | ObjectRef::Union(_) | ObjectRef::Enum(_)
        ) {
            return Err(TransformError::NotAnInitializable(object_ref));
        }

        Ok(object_ref)
    }

    fn function_ref(&self, name: String) -> TransformResult<ObjectRef> {
        let function_ref = self
            .object_refs
            .get(&name)
            .cloned()
            .ok_or(TransformError::FunctionNotDeclared(name))?;

        if !matches!(
            function_ref,
            ObjectRef::Function(_) | ObjectRef::BasicFunction(_)
        ) {
            return Err(TransformError::NotAFunction(function_ref));
        }

        Ok(function_ref)
    }

    fn create_variable_ref(&mut self, name: String) -> TransformResult<VariableRef> {
        let variable_ref = VariableRef(self.variable_ref_count);
        self.variable_ref_count += 1;

        if self
            .stack
            .last_mut()
            .unwrap()
            .variables
            .insert(name.clone(), variable_ref)
            .is_some()
        {
            self.messages.push(TransformMessage {
                message: format!("Variable {name} already declared"),
            })
        }

        Ok(variable_ref)
    }

    fn variable_ref(&self, name: String) -> TransformResult<VariableRef> {
        self.stack
            .iter()
            .rev()
            .find_map(|s| s.variables.get(&name).copied())
            .ok_or(TransformError::VariableNotDeclared(name))
    }

    fn create_label_ref(&mut self) -> LabelRef {
        let stack_top = &mut self.stack.last_mut().unwrap();
        let result = LabelRef(stack_top.label_ref_count);
        stack_top.label_ref_count += 1;
        result
    }

    fn start_function(
        &mut self,
        object_ref: ObjectRef,
        args: Vec<FunctionArg>,
        return_type: TypeDeclaration,
    ) -> TransformResult<()> {
        let function_ref = match object_ref {
            ObjectRef::Function(i) => FunctionRef(i),
            _ => unreachable!(),
        };

        let mut variables = HashMap::new();
        let mut arg_refs = Vec::new();
        for arg in args {
            let var_ref = self.create_variable_ref(arg.name.clone())?;
            variables.insert(arg.name, var_ref);
            arg_refs.push((self.type_ref(arg.object_type)?, var_ref));
        }

        let return_type = self.type_ref(return_type)?;

        let function = Function {
            args: arg_refs,
            instructions: Vec::new(),
            return_type,
            label_offsets: Vec::new(),
        };

        if let Some(previous) = self.functions.insert(function_ref, function) {
            return Err(TransformError::Generic("function already declared"));
        }

        self.stack.push(TransformContextStackItem {
            variables,
            function_ref,
            label_ref_count: 0,
        });

        Ok(())
    }

    fn end_function(&mut self) {
        let label_ref_count = self.stack.last().unwrap().label_ref_count;
        let function = self.curr_function_mut();

        let label_offsets = (0..label_ref_count)
            .map(|i| {
                function
                    .instructions
                    .iter()
                    .position(
                        |instr| matches!(instr, Instruction::Label(label_ref) if label_ref.0 == i),
                    )
                    .unwrap()
            })
            .collect();
        function.label_offsets = label_offsets;

        self.stack.pop();
    }

    fn next_background_color(&mut self) -> u32 {
        let next_value = (self.background_color_index + 1) % BACKGROUND_COLORS.len();
        let color = BACKGROUND_COLORS[self.background_color_index];
        self.background_color_index = next_value;
        color
    }
}

#[derive(Debug)]
enum TransformItem {
    Statement(Box<Statement>),
    Expression(Box<Expression>),
    Instruction(Box<Instruction>),
    PopStack,
}

pub fn transform(parsed_data: ParsedData) -> TransformResult<BtProgram> {
    let mut context = TransformContext::new(parsed_data.object_refs);

    let root_block_ref = ObjectRef::Function(0);
    context
        .start_function(
            root_block_ref,
            Vec::new(),
            TypeDeclaration::Normal("void".to_string()),
        )
        .unwrap();

    context.queue.extend(
        parsed_data
            .statements
            .into_iter()
            .map(Box::new)
            .map(TransformItem::Statement),
    );

    while let Some(item) = context.queue.pop_front() {
        match item {
            TransformItem::Instruction(instruction) => context.add_instruction(*instruction),
            TransformItem::Expression(expression) => {
                transform_expression(&mut context, *expression)?
            }
            TransformItem::Statement(statement) => transform_statement(&mut context, *statement)?,
            TransformItem::PopStack => context.end_function(),
        }
    }

    context.end_function();

    let functions = (0..parsed_data.function_ref_counter)
        .map(|i| context.functions.remove(&FunctionRef(i)).unwrap())
        .collect::<Vec<_>>();
    Ok(BtProgram {
        functions,
        root_function_index: FunctionRef(root_block_ref.index()),
    })
}

fn transform_statement(
    context: &mut TransformContext,
    statement: Statement,
) -> TransformResult<()> {
    match statement {
        Statement::Declare {
            local,
            object_type,
            name,
            args,
            attributes,
        } => {
            let variable_ref = context.create_variable_ref(name.clone())?;

            if local {
                if !attributes.is_empty() {
                    return Err(TransformError::Generic(
                        "local variables can't have attributes",
                    ));
                }

                match object_type {
                    TypeDeclaration::Normal(object_type) => {
                        context.queue_instruction(Instruction::DeclareObject {
                            variable_ref,
                            object_ref: context.initializable_ref(object_type)?,
                        })
                    }
                    TypeDeclaration::Array {
                        name: object_type,
                        size,
                    } => {
                        context.queue_instruction(Instruction::DeclareArray {
                            variable_ref,
                            object_ref: context.initializable_ref(object_type)?,
                        });
                        context.queue_expression(size);
                    }
                    TypeDeclaration::UnsizedArray { .. } => {
                        return Err(TransformError::DeclareUnsizedArray)
                    }
                }
            } else {
                let arg_count = args.len();
                if arg_count != 0 {
                    todo!("Args not implemented yet");
                }

                let attributes = transform_attributes(context, attributes);

                // for arg_expression in args {
                //     context.queue_expression(arg_expression);
                // }

                match object_type {
                    TypeDeclaration::Normal(object_type) => {
                        let object_ref = context.initializable_ref(object_type)?;
                        context.queue_instruction(Instruction::ReadObject {
                            name,
                            variable_ref,
                            object_ref,
                            arg_count,
                            attributes,
                        });
                    }
                    TypeDeclaration::Array {
                        name: object_type,
                        size,
                    } => {
                        context.queue_instruction(Instruction::ReadArray {
                            name,
                            variable_ref,
                            object_ref: context.initializable_ref(object_type)?,
                            attributes,
                        });
                        context.queue_expression(size);
                    }
                    TypeDeclaration::UnsizedArray { .. } => {
                        return Err(TransformError::DeclareUnsizedArray)
                    }
                }
            }
        }
        Statement::DeclareMultiple(statements) => {
            for statement in statements.into_iter().rev() {
                context.queue_statement(statement);
            }
        }
        Statement::DeclareAndAssign {
            local,
            object_type,
            name,
            attributes,
            value,
        } => {
            if !local {
                return Err(TransformError::AssignNonLocalVariable);
            }

            if !attributes.is_empty() {
                return Err(TransformError::Generic(
                    "local variables can't have attributes",
                ));
            }

            let variable_ref = context.create_variable_ref(name)?;

            context.queue_instruction(Instruction::PopVariable(variable_ref));
            context.queue_expression(value);

            match object_type {
                TypeDeclaration::Normal(object_type) => {
                    context.queue_instruction(Instruction::DeclareObject {
                        variable_ref,
                        object_ref: context.initializable_ref(object_type)?,
                    })
                }
                TypeDeclaration::Array {
                    name: object_type,
                    size,
                } => {
                    context.queue_expression(size);
                    context.queue_instruction(Instruction::DeclareArray {
                        variable_ref,
                        object_ref: context.initializable_ref(object_type)?,
                    });
                }
                TypeDeclaration::UnsizedArray { .. } => {
                    return Err(TransformError::DeclareUnsizedArray)
                }
            }
        }
        Statement::Expression(expression) => {
            context.queue_instruction(Instruction::Pop);
            context.queue_expression(expression);
        }
        Statement::DeclareFunction {
            object_ref,
            return_type,
            args,
            statements,
        } => {
            context.start_function(object_ref, args, return_type)?;

            context.queue_pop_stack();
            statements
                .into_iter()
                .rev()
                .for_each(|s| context.queue_statement(s));
        }
        Statement::Typedef { original, alias } => {
            let object_ref = *context
                .object_refs
                .get(&original)
                .ok_or(TransformError::Generic("couldn't find object"))?;
            context.object_refs.insert(alias, object_ref);
        }
        Statement::DeclareEnum {
            object_ref,
            instance_name,
            enum_type,
            variants,
            attributes,
        } => todo!(),
        Statement::DeclareForwardStruct => todo!(),
        Statement::DeclareStruct {
            object_ref,
            instance_name,
            args,
            statements,
            attributes,
        } => todo!(),
        Statement::DeclareUnion {
            object_ref,
            instance_name,
            args,
            statements,
            attributes,
        } => todo!(),
        Statement::If {
            condition,
            statements,
            else_ifs,
            final_else,
        } => {
            let final_label_ref = context.create_label_ref();
            let mut next_check_label_ref = final_label_ref;

            context.queue_instruction(Instruction::Label(final_label_ref));

            if let Some(statements) = final_else {
                next_check_label_ref = context.create_label_ref();

                for statement in statements.into_iter().rev() {
                    context.queue_statement(statement);
                }
                context.queue_instruction(Instruction::Label(next_check_label_ref));
            }

            for (condition, statements) in else_ifs.into_iter().rev() {
                context.queue_instruction(Instruction::Jump(final_label_ref));
                for statement in statements.into_iter().rev() {
                    context.queue_statement(statement);
                }
                context.queue_instruction(Instruction::JumpFalse(next_check_label_ref));
                context.queue_expression(condition);

                next_check_label_ref = context.create_label_ref();
                context.queue_instruction(Instruction::Label(next_check_label_ref));
            }

            context.queue_instruction(Instruction::Jump(final_label_ref));
            for statement in statements.into_iter().rev() {
                context.queue_statement(statement);
            }
            context.queue_instruction(Instruction::JumpFalse(next_check_label_ref));
            context.queue_expression(condition);
        }
        Statement::While {
            condition,
            statements,
        } => {
            let end_label_ref = context.create_label_ref();
            let condition_label_ref = context.create_label_ref();

            context.queue_instruction(Instruction::Label(end_label_ref));
            context.queue_instruction(Instruction::Jump(condition_label_ref));
            for statement in statements.into_iter().rev() {
                context.queue_statement(statement);
            }
            context.queue_instruction(Instruction::JumpFalse(end_label_ref));
            context.queue_expression(condition);
            context.queue_instruction(Instruction::Label(condition_label_ref));
        }
        Statement::DoWhile {
            condition,
            statements,
        } => {
            let start_label_ref = context.create_label_ref();

            context.queue_instruction(Instruction::JumpTrue(start_label_ref));
            context.queue_expression(condition);
            for statement in statements.into_iter().rev() {
                context.queue_statement(statement);
            }
            context.queue_instruction(Instruction::Label(start_label_ref));
        }
        Statement::For {
            initialization,
            condition,
            increment,
            statements,
        } => {
            let loop_start_label_ref = context.create_label_ref();
            let loop_end_label_ref = context.create_label_ref();

            context.queue_instruction(Instruction::Label(loop_end_label_ref));
            context.queue_instruction(Instruction::Jump(loop_start_label_ref));
            if let Some(increment) = increment {
                context.queue_instruction(Instruction::Pop);
                context.queue_expression(increment);
            }
            for statement in statements.into_iter().rev() {
                context.queue_statement(statement);
            }
            if let Some(condition) = condition {
                context.queue_instruction(Instruction::JumpFalse(loop_end_label_ref));
                context.queue_expression(condition);
            }
            context.queue_instruction(Instruction::Label(loop_start_label_ref));
            if let Some(initialization) = initialization {
                context.queue_expression(initialization);
            }
        }
        Statement::Switch { value, switches } => todo!(),
        Statement::Break => todo!(),
        Statement::Continue => todo!(),
        Statement::Return(value) => {
            context.queue_instruction(Instruction::Return);
            context.queue_expression(value);
        }
        Statement::ReturnVoid => context.queue_instruction(Instruction::ReturnVoid),
    }

    Ok(())
}

fn transform_attributes(
    context: &mut TransformContext,
    parsed_attributes: Vec<crate::parse::Attribute>,
) -> Vec<Attribute> {
    let mut attributes = parsed_attributes
        .into_iter()
        .flat_map(|a| {
            let attribute = Attribute::from_parse(&a);
            if attribute.is_none() {
                context.messages.push(TransformMessage {
                    message: format!("Unknown attribute {}. Ignored", a.name),
                });
            }
            attribute
        })
        .collect::<Vec<_>>();
    if context.options.auto_add_colors
        && !attributes
            .iter()
            .any(|a| matches!(a, Attribute::BackgroundColor(_)))
    {
        attributes.push(Attribute::BackgroundColor(context.next_background_color()));
    }
    attributes
}

fn transform_expression(
    context: &mut TransformContext,
    expression: Expression,
) -> TransformResult<()> {
    match expression {
        Expression::Unary(op, target) => {
            let op_instr = match op {
                UnaryOperator::SuffixIncrement => Instruction::SuffixIncrement,
                UnaryOperator::SuffixDecrement => Instruction::SuffixDecrement,
                UnaryOperator::PrefixIncrement => Instruction::PrefixIncrement,
                UnaryOperator::PrefixDecrement => Instruction::PrefixDecrement,
                UnaryOperator::Plus => Instruction::Positive, // TODO: Remove
                UnaryOperator::Minus => Instruction::Negate,
                UnaryOperator::LogicalNot => Instruction::UnaryLogicalNot,
                UnaryOperator::BitwiseNot => Instruction::UnaryBitwiseNot,
            };

            context.queue_instruction(op_instr);
            context.queue_expression(*target);
        }
        Expression::Binary { operator, lhs, rhs } => {
            let op_instr = match operator {
                BinaryOperator::Multiply => Instruction::Multiply,
                BinaryOperator::Divide => Instruction::Divide,
                BinaryOperator::Modulus => Instruction::Modulus,
                BinaryOperator::Add => Instruction::Add,
                BinaryOperator::Subtract => Instruction::Subtract,
                BinaryOperator::LeftShift => Instruction::LeftShift,
                BinaryOperator::RightShift => Instruction::RightShift,
                BinaryOperator::LessThan => Instruction::LessThan,
                BinaryOperator::LessThanOrEqual => Instruction::LessThanOrEqual,
                BinaryOperator::MoreThan => Instruction::MoreThan,
                BinaryOperator::MoreThanOrEqual => Instruction::MoreThanOrEqual,
                BinaryOperator::Equal => Instruction::Equal,
                BinaryOperator::NotEqual => Instruction::NotEqual,
                BinaryOperator::BitwiseAnd => Instruction::BitwiseAnd,
                BinaryOperator::BitwiseXor => Instruction::BitwiseXor,
                BinaryOperator::BitwiseOr => Instruction::BitwiseOr,
                BinaryOperator::LogicalAnd => Instruction::LogicalAnd,
                BinaryOperator::LogicalOr => Instruction::LogicalOr,
                BinaryOperator::Assign => Instruction::Assign,
                BinaryOperator::AssignAdd => Instruction::AssignAdd,
                BinaryOperator::AssignSubtract => Instruction::AssignSubtract,
                BinaryOperator::AssignMultiply => Instruction::AssignMultiply,
                BinaryOperator::AssignDivide => Instruction::AssignDivide,
                BinaryOperator::AssignModulus => Instruction::AssignModulus,
                BinaryOperator::AssignLeftShift => Instruction::AssignLeftShift,
                BinaryOperator::AssignRightShift => Instruction::AssignRightShift,
                BinaryOperator::AssignBitwiseAnd => Instruction::AssignBitwiseAnd,
                BinaryOperator::AssignBitwiseXor => Instruction::AssignBitwiseXor,
                BinaryOperator::AssignBitwiseOr => Instruction::AssignBitwiseOr,
            };

            context.queue_instruction(op_instr);
            context.queue_expression(*lhs);
            context.queue_expression(*rhs);
        }
        Expression::Ternary {
            condition,
            true_expression,
            false_expression,
        } => {
            let true_ref = context.create_label_ref();
            let end_ref = context.create_label_ref();

            context.queue_instruction(Instruction::Label(end_ref));
            context.queue_expression(*true_expression);
            context.queue_instruction(Instruction::Label(true_ref));
            context.queue_instruction(Instruction::Jump(end_ref));
            context.queue_expression(*false_expression);
            context.queue_instruction(Instruction::JumpTrue(true_ref));
            context.queue_expression(*condition);
        }
        Expression::Cast { target, cast_type } => {
            let cast_type_ref = context.initializable_ref(cast_type)?;

            context.queue_instruction(Instruction::Cast(cast_type_ref));
            context.queue_expression(*target);
        }
        Expression::CallFunction { target, arguments } => {
            let arg_count = arguments.len();
            let function_name = match *target {
                Expression::Identifier(name) => name,
                _ => unreachable!(),
            };
            let function_ref = context.function_ref(function_name)?;

            let function_instr = match function_ref {
                ObjectRef::Function(function_ref) => Instruction::CallFunction {
                    function_ref: FunctionRef(function_ref),
                    arg_count,
                },
                ObjectRef::BasicFunction(basic_function) => {
                    let expected_arg_count = match basic_function {
                        BasicFunction::Printf => 0..=usize::MAX,
                        BasicFunction::Warning => 0..=usize::MAX,
                        BasicFunction::LittleEndian => 0..=0,
                        BasicFunction::BigEndian => 0..=0,
                    };
                    if !expected_arg_count.contains(&arguments.len()) {
                        return Err(TransformError::Generic(
                            "invalid arg count for basic function",
                        ));
                    }
                    Instruction::CallBasicFunction {
                        basic_function,
                        arg_count,
                    }
                }
                _ => unreachable!(),
            };

            context.queue_instruction(function_instr);
            for expression in arguments.into_iter() {
                context.queue_expression(expression);
            }
        }
        Expression::GetArrayIndex { target, index } => {
            context.queue_instruction(Instruction::GetArrayIndex);
            context.queue_expression(*target);
            context.queue_expression(*index);
        }
        Expression::GetMember { target, name } => {
            context.queue_instruction(Instruction::GetMember(name));
            context.queue_expression(*target);
        }
        Expression::Identifier(name) => {
            context.add_instruction(Instruction::PushVariable(context.variable_ref(name)?))
        }
        Expression::String(value) => context.add_instruction(Instruction::PushString(value)),
        Expression::WideString(value) => {
            context.add_instruction(Instruction::PushWideString(value))
        }
        Expression::Char(value) => context.add_instruction(Instruction::PushChar(value)),
        Expression::Number(number) => context.add_instruction(match number {
            Number::Bool(value) => Instruction::PushBool(value),
            Number::U32(value) => Instruction::PushU32(value),
            Number::I32(value) => Instruction::PushI32(value),
            Number::U64(value) => Instruction::PushU64(value),
            Number::I64(value) => Instruction::PushI64(value),
            Number::F32(value) => Instruction::PushF32(value),
            Number::F64(value) => Instruction::PushF64(value),
        }),
        Expression::DeclareArrayValues(array_values) => {
            let array_value_count = array_values.len();

            context.queue_instruction(Instruction::DeclareArrayValues(array_value_count));
            for array_value in array_values.into_iter().rev() {
                context.queue_expression(array_value);
            }
        }
    }

    Ok(())
}

impl Attribute {
    fn from_parse(attribute: &crate::parse::Attribute) -> Option<Attribute> {
        Some(match attribute.name.as_str() {
            "color" | "bgcolor" => {
                Attribute::BackgroundColor(get_color_from_attribute(&attribute.value))
            }
            _ => return None,
        })
    }
}

fn get_color_from_attribute(attribute_value: &AttributeValue) -> u32 {
    // TODO: Handle this during transform
    match attribute_value {
        AttributeValue::String(_) => todo!(),
        AttributeValue::Character(_) => todo!(),
        AttributeValue::Number(n) => match n {
            crate::parse::Number::Bool(v) => *v as u32,
            crate::parse::Number::U32(v) => *v,
            crate::parse::Number::I32(v) => *v as u32,
            crate::parse::Number::U64(v) => *v as u32,
            crate::parse::Number::I64(v) => *v as u32,
            crate::parse::Number::F32(v) => *v as u32,
            crate::parse::Number::F64(v) => *v as u32,
        },
        AttributeValue::Identifier(_) => todo!(),
    }
}