use std::collections::{HashMap, VecDeque};

use crate::{
    instruction::{
        Attribute, BasicFunction, CodeBlockRef, Color, FunctionRef, Instruction, LabelRef,
        StructRef, VariableRef,
    },
    object::{number::NumberType, ObjectType},
    parse::{
        AttributeValue, BinaryOperator, CodeBlockArg, Expression, Number, ObjectRef, ParsedData,
        Statement, TypeDeclaration, UnaryOperator,
    },
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
    #[error("Unknown object type")]
    UnknownObjectType,
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
    pub functions: Vec<CodeBlock>,
    pub structs: Vec<CodeBlock>,
    pub root_function_ref: FunctionRef,
}

#[derive(Debug)]
pub struct CodeBlock {
    pub args: Vec<(ObjectType, VariableRef)>,
    pub instructions: Vec<Instruction>,
    pub return_type: ObjectType,
    pub label_offsets: Vec<usize>,
}

#[derive(Debug)]
pub struct TransformMessage {
    message: String,
    // TODO: Add location info
}

// TODO: Make this public and an argument to transform
#[derive(Debug)]
struct TransformOptions {
    auto_add_colors: bool,
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
            TransformItem::Expression(expression) => context.transform_expression(*expression)?,
            TransformItem::Statement(statement) => context.transform_statement(*statement)?,
            TransformItem::PopStack => context.end_function(),
        }
    }

    context.end_function();

    let functions = (0..parsed_data.function_ref_counter)
        .map(|i| context.functions.remove(&FunctionRef(i)).unwrap())
        .collect::<Vec<_>>();
    let structs = (0..parsed_data.struct_ref_counter)
        .map(|i| context.structs.remove(&StructRef(i)).unwrap())
        .collect::<Vec<_>>();
    Ok(BtProgram {
        functions,
        structs,
        root_function_ref: FunctionRef(root_block_ref.index()),
    })
}

#[derive(Debug)]
struct TransformContext {
    stack: Vec<TransformContextStackItem>,
    variable_ref_count: u64,
    functions: HashMap<FunctionRef, CodeBlock>,
    structs: HashMap<StructRef, CodeBlock>,
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
    code_block_ref: CodeBlockRef,
    color: Option<Color>,
}

impl TransformContext {
    fn new(object_refs: HashMap<String, ObjectRef>) -> TransformContext {
        TransformContext {
            stack: Vec::new(),
            variable_ref_count: 0,
            functions: HashMap::new(),
            structs: HashMap::new(),
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

    fn curr_code_block_mut(&mut self) -> &mut CodeBlock {
        let cur_stack = self.stack.last().unwrap();
        match cur_stack.code_block_ref {
            CodeBlockRef::Function(function_ref) => self.functions.get_mut(&function_ref).unwrap(),
            CodeBlockRef::Struct(struct_ref) => self.structs.get_mut(&struct_ref).unwrap(),
        }
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.curr_code_block_mut().instructions.push(instruction);
    }

    fn type_ref(&self, type_declaration: TypeDeclaration) -> TransformResult<ObjectType> {
        Ok(match type_declaration {
            TypeDeclaration::Normal(name) if name == "void" => ObjectType::Void,
            TypeDeclaration::Normal(name) => self.resolve_object_type(&name)?,
            TypeDeclaration::Array { .. } => todo!(),
            TypeDeclaration::UnsizedArray { .. } => todo!(),
        })
    }

    fn resolve_object_type(&self, name: &str) -> TransformResult<ObjectType> {
        let object_ref = self
            .object_refs
            .get(name)
            .cloned()
            .ok_or(TransformError::UnknownObjectType)?;

        Ok(match object_ref {
            ObjectRef::Void => ObjectType::Void,
            ObjectRef::Basic(basic) => match basic {
                crate::parse::BasicObject::U8 => ObjectType::Number(NumberType::U8),
                crate::parse::BasicObject::I8 => ObjectType::Number(NumberType::I8),
                crate::parse::BasicObject::U16 => ObjectType::Number(NumberType::U16),
                crate::parse::BasicObject::I16 => ObjectType::Number(NumberType::I16),
                crate::parse::BasicObject::U32 => ObjectType::Number(NumberType::U32),
                crate::parse::BasicObject::I32 => ObjectType::Number(NumberType::I32),
                crate::parse::BasicObject::U64 => ObjectType::Number(NumberType::U64),
                crate::parse::BasicObject::I64 => ObjectType::Number(NumberType::I64),
                crate::parse::BasicObject::F32 => ObjectType::Number(NumberType::F32),
                crate::parse::BasicObject::F64 => ObjectType::Number(NumberType::F64),
                crate::parse::BasicObject::String => ObjectType::Array(NumberType::Char),
            },
            ObjectRef::Array(basic) => match basic {
                crate::parse::BasicObject::U8 => ObjectType::Array(NumberType::U8),
                crate::parse::BasicObject::I8 => ObjectType::Array(NumberType::I8),
                crate::parse::BasicObject::U16 => ObjectType::Array(NumberType::U16),
                crate::parse::BasicObject::I16 => ObjectType::Array(NumberType::I16),
                crate::parse::BasicObject::U32 => ObjectType::Array(NumberType::U32),
                crate::parse::BasicObject::I32 => ObjectType::Array(NumberType::I32),
                crate::parse::BasicObject::U64 => ObjectType::Array(NumberType::U64),
                crate::parse::BasicObject::I64 => ObjectType::Array(NumberType::I64),
                crate::parse::BasicObject::F32 => ObjectType::Array(NumberType::F32),
                crate::parse::BasicObject::F64 => ObjectType::Array(NumberType::F64),
                crate::parse::BasicObject::String => {
                    return Err(TransformError::Generic(
                        "multidimensional arrays not supported",
                    ))
                }
            },
            o => todo!("{:?}", o),
        })
    }

    fn resolve_number_type(&self, name: &str) -> TransformResult<NumberType> {
        match self.resolve_object_type(name)? {
            ObjectType::Number(number_type) => Ok(number_type),
            _ => Err(TransformError::Generic("not a number type")),
        }
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
        args: Vec<CodeBlockArg>,
        return_type: TypeDeclaration,
    ) -> TransformResult<()> {
        let function_ref = match object_ref {
            ObjectRef::Function(i) => FunctionRef(i),
            _ => unreachable!(),
        };

        let mut variables = HashMap::new();
        let mut arg_types = Vec::new();
        for arg in args {
            let var_ref = self.create_variable_ref(arg.name.clone())?;
            variables.insert(arg.name, var_ref);
            arg_types.push((self.type_ref(arg.object_type)?, var_ref));
        }

        let return_type = self.type_ref(return_type)?;

        let function = CodeBlock {
            args: arg_types,
            instructions: Vec::new(),
            return_type,
            label_offsets: Vec::new(),
        };

        if let Some(previous) = self.functions.insert(function_ref, function) {
            return Err(TransformError::Generic("function already declared"));
        }

        self.stack.push(TransformContextStackItem {
            variables,
            code_block_ref: CodeBlockRef::Function(function_ref),
            label_ref_count: 0,
            color: None,
        });

        Ok(())
    }

    fn start_struct(
        &mut self,
        object_ref: ObjectRef,
        args: Vec<CodeBlockArg>,
        return_type: TypeDeclaration,
        color: Option<Color>,
    ) -> TransformResult<()> {
        let struct_ref = match object_ref {
            ObjectRef::Struct(i) => StructRef(i),
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

        let struct_ = CodeBlock {
            args: arg_refs,
            instructions: Vec::new(),
            return_type,
            label_offsets: Vec::new(),
        };

        if let Some(previous) = self.structs.insert(struct_ref, struct_) {
            return Err(TransformError::Generic("struct already declared"));
        }

        self.stack.push(TransformContextStackItem {
            variables,
            code_block_ref: CodeBlockRef::Struct(struct_ref),
            label_ref_count: 0,
            color,
        });

        Ok(())
    }

    fn end_function(&mut self) {
        let label_ref_count = self.stack.last().unwrap().label_ref_count;
        let function = self.curr_code_block_mut();

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

    fn next_background_color(&mut self) -> Color {
        let next_value = (self.background_color_index + 1) % BACKGROUND_COLORS.len();
        let color = BACKGROUND_COLORS[self.background_color_index];
        self.background_color_index = next_value;
        Color(color)
    }

    fn transform_statement(&mut self, statement: Statement) -> TransformResult<()> {
        match statement {
            Statement::Declare {
                local,
                object_type,
                name,
                args,
                attributes,
            } => {
                let variable_ref = self.create_variable_ref(name.clone())?;

                if local {
                    if !attributes.is_empty() {
                        return Err(TransformError::Generic(
                            "local variables can't have attributes",
                        ));
                    }

                    match object_type {
                        TypeDeclaration::Normal(object_type) => {
                            self.queue_instruction(Instruction::DeclareObject {
                                variable_ref,
                                object_type: self.resolve_object_type(&object_type)?,
                            })
                        }
                        TypeDeclaration::Array {
                            type_name: object_type,
                            size,
                        } => {
                            self.queue_instruction(Instruction::DeclareArray {
                                variable_ref,
                                number_type: self.resolve_number_type(&object_type)?,
                            });
                            self.queue_expression(size);
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
                    // for arg_expression in args {
                    //     self.queue_expression(arg_expression);
                    // }

                    let mut color = None;
                    for attribute in self.transform_attributes(attributes) {
                        match attribute {
                            Attribute::Color(v) => color = Some(v),
                        }
                    }

                    match object_type {
                        TypeDeclaration::Normal(object_type) => {
                            let object_type = self.resolve_object_type(&object_type)?;
                            self.queue_instruction(Instruction::ReadObject {
                                name,
                                variable_ref,
                                object_type,
                                arg_count,
                                color,
                            });
                        }
                        TypeDeclaration::Array { type_name, size } => {
                            let object_type = self.resolve_object_type(&type_name)?;
                            self.queue_instruction(Instruction::ReadArray {
                                name,
                                variable_ref,
                                object_type,
                                color,
                            });
                            self.queue_expression(size);
                        }
                        TypeDeclaration::UnsizedArray { .. } => {
                            return Err(TransformError::DeclareUnsizedArray)
                        }
                    }
                }
            }
            Statement::DeclareMultiple(statements) => {
                for statement in statements.into_iter().rev() {
                    self.queue_statement(statement);
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

                let variable_ref = self.create_variable_ref(name)?;

                self.queue_instruction(Instruction::PopVariable(variable_ref));
                self.queue_expression(value);

                match object_type {
                    TypeDeclaration::Normal(object_type) => {
                        self.queue_instruction(Instruction::DeclareObject {
                            variable_ref,
                            object_type: self.resolve_object_type(&object_type)?,
                        })
                    }
                    TypeDeclaration::Array {
                        type_name: object_type,
                        size,
                    } => {
                        self.queue_instruction(Instruction::DeclareArray {
                            variable_ref,
                            number_type: self.resolve_number_type(&object_type)?,
                        });
                        self.queue_expression(size);
                    }
                    TypeDeclaration::UnsizedArray { .. } => {
                        return Err(TransformError::DeclareUnsizedArray)
                    }
                }
            }
            Statement::Expression(expression) => {
                self.queue_instruction(Instruction::Pop);
                self.queue_expression(expression);
            }
            Statement::DeclareFunction {
                object_ref,
                return_type,
                args,
                statements,
            } => {
                self.start_function(object_ref, args, return_type)?;

                self.queue_pop_stack();
                statements
                    .into_iter()
                    .rev()
                    .for_each(|s| self.queue_statement(s));
            }
            Statement::Typedef { original, alias } => {
                let object_ref = *self
                    .object_refs
                    .get(&original)
                    .ok_or(TransformError::Generic("couldn't find object"))?;
                self.object_refs.insert(alias, object_ref);
            }
            Statement::DeclareEnum {
                object_ref,
                instance_name,
                enum_type,
                variants,
                attributes,
            } => todo!(),
            Statement::DeclareForwardStruct => (),
            Statement::DeclareStruct {
                object_ref,
                instance_name,
                args,
                statements,
                attributes,
            } => {
                let mut color = None;
                for attribute in self.transform_attributes(attributes) {
                    match attribute {
                        Attribute::Color(v) => color = Some(v),
                    }
                }

                let instance_name =
                    instance_name.unwrap_or(TypeDeclaration::Normal("void".to_string()));

                self.start_struct(object_ref, args, instance_name, color)?;

                self.queue_pop_stack();
                statements
                    .into_iter()
                    .rev()
                    .for_each(|s| self.queue_statement(s));
            }
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
                let final_label_ref = self.create_label_ref();
                let mut next_check_label_ref = final_label_ref;

                self.queue_instruction(Instruction::Label(final_label_ref));

                if let Some(statements) = final_else {
                    next_check_label_ref = self.create_label_ref();

                    for statement in statements.into_iter().rev() {
                        self.queue_statement(statement);
                    }
                    self.queue_instruction(Instruction::Label(next_check_label_ref));
                }

                for (condition, statements) in else_ifs.into_iter().rev() {
                    self.queue_instruction(Instruction::Jump(final_label_ref));
                    for statement in statements.into_iter().rev() {
                        self.queue_statement(statement);
                    }
                    self.queue_instruction(Instruction::JumpFalse(next_check_label_ref));
                    self.queue_expression(condition);

                    next_check_label_ref = self.create_label_ref();
                    self.queue_instruction(Instruction::Label(next_check_label_ref));
                }

                self.queue_instruction(Instruction::Jump(final_label_ref));
                for statement in statements.into_iter().rev() {
                    self.queue_statement(statement);
                }
                self.queue_instruction(Instruction::JumpFalse(next_check_label_ref));
                self.queue_expression(condition);
            }
            Statement::While {
                condition,
                statements,
            } => {
                let end_label_ref = self.create_label_ref();
                let condition_label_ref = self.create_label_ref();

                self.queue_instruction(Instruction::Label(end_label_ref));
                self.queue_instruction(Instruction::Jump(condition_label_ref));
                for statement in statements.into_iter().rev() {
                    self.queue_statement(statement);
                }
                self.queue_instruction(Instruction::JumpFalse(end_label_ref));
                self.queue_expression(condition);
                self.queue_instruction(Instruction::Label(condition_label_ref));
            }
            Statement::DoWhile {
                condition,
                statements,
            } => {
                let start_label_ref = self.create_label_ref();

                self.queue_instruction(Instruction::JumpTrue(start_label_ref));
                self.queue_expression(condition);
                for statement in statements.into_iter().rev() {
                    self.queue_statement(statement);
                }
                self.queue_instruction(Instruction::Label(start_label_ref));
            }
            Statement::For {
                initialization,
                condition,
                increment,
                statements,
            } => {
                let loop_start_label_ref = self.create_label_ref();
                let loop_end_label_ref = self.create_label_ref();

                self.queue_instruction(Instruction::Label(loop_end_label_ref));
                self.queue_instruction(Instruction::Jump(loop_start_label_ref));
                if let Some(increment) = increment {
                    self.queue_instruction(Instruction::Pop);
                    self.queue_expression(increment);
                }
                for statement in statements.into_iter().rev() {
                    self.queue_statement(statement);
                }
                if let Some(condition) = condition {
                    self.queue_instruction(Instruction::JumpFalse(loop_end_label_ref));
                    self.queue_expression(condition);
                }
                self.queue_instruction(Instruction::Label(loop_start_label_ref));
                if let Some(initialization) = initialization {
                    self.queue_instruction(Instruction::Pop);
                    self.queue_expression(initialization);
                }
            }
            Statement::Switch { value, switches } => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Return(value) => {
                self.queue_instruction(Instruction::Return);
                self.queue_expression(value);
            }
            Statement::ReturnVoid => self.queue_instruction(Instruction::ReturnVoid),
        }

        Ok(())
    }

    fn transform_attributes(
        &mut self,
        parsed_attributes: Vec<crate::parse::Attribute>,
    ) -> Vec<Attribute> {
        let mut attributes = parsed_attributes
            .into_iter()
            .flat_map(|a| {
                let attribute = Attribute::from_parse(&a);
                if attribute.is_none() {
                    self.messages.push(TransformMessage {
                        message: format!("Unknown attribute {}. Ignored", a.name),
                    });
                }
                attribute
            })
            .collect::<Vec<_>>();

        if !attributes.iter().any(|a| matches!(a, Attribute::Color(_))) {
            if let Some(stack_color) = self.stack.last().unwrap().color {
                attributes.push(Attribute::Color(stack_color));
            } else if self.options.auto_add_colors {
                attributes.push(Attribute::Color(self.next_background_color()));
            }
        }

        attributes
    }

    fn transform_expression(
        self: &mut TransformContext,
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

                self.queue_instruction(op_instr);
                self.queue_expression(*target);
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

                self.queue_instruction(op_instr);
                self.queue_expression(*lhs);
                self.queue_expression(*rhs);
            }
            Expression::Ternary {
                condition,
                true_expression,
                false_expression,
            } => {
                let true_ref = self.create_label_ref();
                let end_ref = self.create_label_ref();

                self.queue_instruction(Instruction::Label(end_ref));
                self.queue_expression(*true_expression);
                self.queue_instruction(Instruction::Label(true_ref));
                self.queue_instruction(Instruction::Jump(end_ref));
                self.queue_expression(*false_expression);
                self.queue_instruction(Instruction::JumpTrue(true_ref));
                self.queue_expression(*condition);
            }
            Expression::Cast { target, cast_type } => {
                let cast_type = self.resolve_object_type(&cast_type)?;

                self.queue_instruction(Instruction::Cast(cast_type));
                self.queue_expression(*target);
            }
            Expression::CallFunction { target, arguments } => {
                let arg_count = arguments.len();
                let function_name = match *target {
                    Expression::Identifier(name) => name,
                    _ => unreachable!(),
                };
                let function_ref = self.function_ref(function_name)?;

                let function_instr = match function_ref {
                    ObjectRef::Function(function_ref_index) => Instruction::CallFunction {
                        function_ref: FunctionRef(function_ref_index),
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

                self.queue_instruction(function_instr);
                for expression in arguments.into_iter() {
                    self.queue_expression(expression);
                }
            }
            Expression::GetArrayIndex { target, index } => {
                self.queue_instruction(Instruction::GetArrayIndex);
                self.queue_expression(*target);
                self.queue_expression(*index);
            }
            Expression::GetMember { target, name } => {
                self.queue_instruction(Instruction::GetMember(name));
                self.queue_expression(*target);
            }
            Expression::Identifier(name) => {
                self.add_instruction(Instruction::PushVariable(self.variable_ref(name)?))
            }
            Expression::String(value) => self.add_instruction(Instruction::PushString(value)),
            Expression::WideString(value) => {
                self.add_instruction(Instruction::PushWideString(value))
            }
            Expression::Char(value) => self.add_instruction(Instruction::PushChar(value)),
            Expression::Number(number) => self.add_instruction(match number {
                Number::Bool(value) => Instruction::PushBool(value),
                Number::U32(value) => Instruction::PushU32(value),
                Number::I32(value) => Instruction::PushI32(value),
                Number::U64(value) => Instruction::PushU64(value),
                Number::I64(value) => Instruction::PushI64(value),
                Number::F32(value) => Instruction::PushF32(value),
                Number::F64(value) => Instruction::PushF64(value),
            }),
            Expression::DeclareArrayValues(array_values) => {
                let array_value_count = array_values.len() as u64;

                self.queue_instruction(Instruction::DeclareArrayValues(array_value_count));
                for array_value in array_values.into_iter().rev() {
                    self.queue_expression(array_value);
                }
            }
        }

        Ok(())
    }
}

impl Attribute {
    fn from_parse(attribute: &crate::parse::Attribute) -> Option<Attribute> {
        Some(match attribute.name.as_str() {
            "color" | "bgcolor" => Attribute::Color(get_color_from_attribute(&attribute.value)),
            _ => return None,
        })
    }
}

fn get_color_from_attribute(attribute_value: &AttributeValue) -> Color {
    Color(match attribute_value {
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
    })
}
