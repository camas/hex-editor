use std::{
    collections::{HashMap, HashSet},
    io::SeekFrom,
    num::Wrapping,
};

use common::{DataSourceTraits, Reader};

use crate::{
    instruction::{
        Attribute, BasicFunction, CodeBlockRef, FunctionRef, Instruction, StructRef, VariableRef,
    },
    object::{number::NumberType, NumberArray, Object, ObjectError, ObjectResult, ObjectType},
    transform::{BtProgram, CodeBlock},
    Number,
};

use self::parsed::{ParsedObject, ParsedObjects};

pub(crate) mod parsed;

#[cfg(test)]
mod tests;

#[derive(thiserror::Error, Debug)]
pub enum AnalyzeError {
    #[error("IO Error")]
    IoError(#[from] std::io::Error),
    #[error("Function with the same name already exists")]
    FunctionWithSameName,
    #[error("Struct with the same name already exists")]
    StructWithSameName,
    #[error("Can't assign to non-local variable")]
    AssignToNonLocal,
    #[error("Unknown type")]
    UnknownType,
    #[error("Not a type")]
    NotAType,
    // TODO: Replace with actual errors instead of being lazy
    #[error("{0}")]
    GenericError(&'static str),
    #[error("Object error")]
    ObjectError(#[from] ObjectError),
    #[error("Index is out of bounds of the array")]
    IndexOutOfBounds,
}

type AnalyzeResult<T> = Result<T, AnalyzeError>;

#[derive(Debug)]
struct ExecutionContext<'a> {
    program: &'a BtProgram,
    code_block_stack: Vec<CodeBlockContext>,
    reader: Reader<&'a mut Box<dyn DataSourceTraits>>,
    object_stack: Vec<Object>,
    parsed_objects: ParsedObjects,
    little_endian: bool,
    stdout: Vec<u8>,
}

#[derive(Debug)]
struct CodeBlockContext {
    variables: HashMap<VariableRef, Object>,
    code_block_ref: CodeBlockRef,
    instruction_pointer: usize,
    color: Option<u32>,
}

#[derive(Debug)]
pub struct AnalyzedData {
    pub parsed_objects: Vec<ParsedObject>,
    pub stdout: Vec<u8>,
}

pub fn analyze_data(
    program: &BtProgram,
    data: &mut Box<dyn DataSourceTraits>,
) -> AnalyzeResult<AnalyzedData> {
    data.seek(SeekFrom::Start(0)).unwrap();
    let reader = Reader::new(data);
    let mut context = ExecutionContext {
        program,
        code_block_stack: Vec::new(),
        reader,
        object_stack: Vec::new(),
        parsed_objects: ParsedObjects::default(),
        little_endian: true,
        stdout: Vec::new(),
    };

    context.enter_function(program.root_function_ref);
    while context.can_execute() {
        context.step()?;
    }

    // TODO: Throw errors instead. Should probably remove any assert in this file
    assert!(context.code_block_stack.is_empty());
    // Script is treated as a function so returns void when exiting
    assert_eq!(context.object_stack.len(), 1);
    assert_eq!(context.object_stack[0].as_type(), ObjectType::Void);

    Ok(AnalyzedData {
        parsed_objects: context.parsed_objects.objects,
        stdout: context.stdout,
    })
}

impl<'a> ExecutionContext<'a> {
    fn step(&mut self) -> AnalyzeResult<()> {
        let code_block_context = self.curr_code_block_context();
        let code_block = self.curr_code_block();

        if code_block_context.instruction_pointer >= code_block.instructions.len() {
            self.exit_code_block()?;
            return Ok(());
        }

        macro_rules! unary_instr {
            ($method:ident) => {{
                let result = self.pop_resolve()?.$method()?;
                self.push(result);
            }};
        }

        macro_rules! binary_instr {
            ($method:ident) => {{
                let result = self.pop_resolve()?.$method(self.pop_resolve()?)?;
                self.push(result);
            }};
        }

        macro_rules! assign_instr {
            ($method:ident) => {{
                let target_ref = self.pop_ref()?;
                let value = self.pop_resolve()?;
                match target_ref {
                    Object::VariableRef(variable_ref) => {
                        let mut tmp_object = Object::Void;
                        let variable = self.variable_mut(variable_ref);
                        std::mem::swap(variable, &mut tmp_object);
                        let new_object = tmp_object.$method(value)?;
                        *variable = new_object;
                        self.push(Object::VariableRef(variable_ref));
                    }
                    Object::ArrayEntryRef {
                        variable_ref,
                        index,
                    } => {
                        let object = self.variable_mut(variable_ref);
                        let Object::Array(array) = object else {
                                        return Err(AnalyzeError::GenericError("Not an array"));
                                    };
                        array.$method(index, value)?;
                        self.push(Object::Void); // TODO: Return an actual value? Throw an error?
                    }
                    _ => unreachable!(),
                }
            }};
        }

        let instruction = &code_block.instructions[code_block_context.instruction_pointer];

        // println!("{:?}", self.object_stack);
        // println!("{:?}", instruction);

        match instruction {
            Instruction::Pop => {
                self.pop();
            }
            Instruction::PopVariable(variable_ref) => {
                let variable_ref = *variable_ref;
                let value = self.pop();
                match value {
                    // Special case: Assign temp values to an array
                    Object::TempArray(values) => {
                        let Object::Array(array) = self.variable_mut(variable_ref) else {
                            return Err(AnalyzeError::GenericError("not an array"));
                        };
                        if array.len() != values.len() {
                            return Err(AnalyzeError::GenericError("Wrong size array"));
                        }
                        array.set_values(values)?;
                    }
                    value => {
                        *self.variable_mut(variable_ref) = value;
                    }
                }
            }
            Instruction::PushVariable(variable_ref) => {
                let variable_ref = *variable_ref;
                self.push(Object::VariableRef(variable_ref));
            }
            Instruction::PushString(value) => {
                self.push(Object::Array(NumberArray::Char(value.clone())));
            }
            Instruction::PushWideString(value) => {
                // TODO: Handle wide strings properly
                self.push(Object::Array(NumberArray::Char(value.clone())));
            }
            Instruction::PushBool(value) => {
                self.push(Object::new_u8(*value as u8));
            }
            Instruction::PushChar(value) => {
                self.push(Object::new_i8(*value));
            }
            Instruction::PushU8(value) => {
                self.push(Object::new_u8(*value));
            }
            Instruction::PushU32(value) => {
                self.push(Object::new_u32(*value));
            }
            Instruction::PushI32(value) => {
                self.push(Object::new_i32(*value));
            }
            Instruction::PushU64(value) => {
                self.push(Object::new_u64(*value));
            }
            Instruction::PushI64(value) => {
                self.push(Object::new_i64(*value));
            }
            Instruction::PushF32(value) => {
                self.push(Object::new_f32(*value));
            }
            Instruction::PushF64(value) => {
                self.push(Object::new_f64(*value));
            }
            Instruction::DeclareObject {
                variable_ref,
                object_type,
            } => {
                let object = object_type.create_default()?;
                self.initialize_variable(*variable_ref, object);
            }
            Instruction::DeclareArray {
                variable_ref,
                number_type,
            } => {
                let variable_ref = *variable_ref;
                let number_type = *number_type;

                let size = match self.pop_resolve()? {
                    Object::Number(v) => v.as_u64(),
                    _ => return Err(AnalyzeError::GenericError("Invalid type for array size")),
                } as usize;
                let object = Object::Array(match number_type {
                    NumberType::Char => NumberArray::Char(vec![0; size]),
                    NumberType::U8 => NumberArray::U8(vec![0; size]),
                    NumberType::I8 => NumberArray::I8(vec![0; size]),
                    NumberType::U16 => NumberArray::U16(vec![0; size]),
                    NumberType::I16 => NumberArray::I16(vec![0; size]),
                    NumberType::U32 => NumberArray::U32(vec![0; size]),
                    NumberType::I32 => NumberArray::I32(vec![0; size]),
                    NumberType::U64 => NumberArray::U64(vec![0; size]),
                    NumberType::I64 => NumberArray::I64(vec![0; size]),
                    NumberType::F32 => NumberArray::F32(vec![0.; size]),
                    NumberType::F64 => NumberArray::F64(vec![0.; size]),
                });
                self.initialize_variable(variable_ref, object);
            }
            Instruction::ReadObject {
                name,
                variable_ref,
                object_type,
                arg_count,
                color,
            } => {
                if *arg_count > 0 {
                    todo!();
                }

                let variable_ref = *variable_ref;
                let object_type = *object_type;
                let name = name.clone();
                let color = *color;

                let start = self.reader.position()?;
                let value = self.read_object(object_type)?;
                let end = self.reader.position()?.max(start + 1) - 1;

                self.initialize_variable(variable_ref, value.clone());
                self.parsed_objects.add(name, value, start, end, color);
            }
            Instruction::ReadArray {
                name,
                variable_ref,
                object_type: number_type,
                color,
            } => {
                let variable_ref = *variable_ref;
                let number_type = *number_type;
                let name = name.clone();
                let color = *color;

                let size = self.pop_u64()?;
                let start = self.reader.position()?;
                let value = self.read_array_lazy(number_type, size)?;
                let end = self.reader.position()?.max(start + 1) - 1;

                self.initialize_variable(variable_ref, value.clone());
                self.parsed_objects.add(name, value, start, end, color);
            }
            Instruction::Cast(target) => {
                let target = *target;
                let value = self.pop();
                self.push(value.cast(target)?);
            }
            Instruction::CallFunction {
                function_ref,
                arg_count,
            } => {
                let function_ref = *function_ref;
                let arg_count = *arg_count;

                self.curr_code_block_context_mut().instruction_pointer += 1;

                self.enter_function(function_ref);

                let mut args = (0..arg_count)
                    .map(|_| self.pop_resolve())
                    .collect::<AnalyzeResult<Vec<_>>>()?;

                let code_block = self.curr_code_block();

                if code_block.args.len() != arg_count {
                    return Err(AnalyzeError::GenericError(
                        "Wrong number of arguments for function",
                    ));
                }

                for (arg, arg_ref) in args.iter_mut().zip(code_block.args.iter()) {
                    if arg.as_type() != arg_ref.0 {
                        // Try auto casting arguments
                        *arg = arg.cast(arg_ref.0)?;
                    }
                }

                let var_refs = code_block.args.iter().map(|a| a.1).collect::<Vec<_>>();
                for (arg, var_ref) in args.into_iter().zip(var_refs.into_iter()) {
                    self.initialize_variable(var_ref, arg);
                }

                return Ok(());
            }
            Instruction::CallBasicFunction {
                basic_function,
                arg_count,
            } => {
                let basic_function = *basic_function;

                let args = (0..*arg_count)
                    .map(|_| self.pop_resolve())
                    .collect::<AnalyzeResult<Vec<_>>>()?;

                let mut return_value = Object::Void;
                match basic_function {
                    BasicFunction::Printf => {
                        let result = format_string(args)?;
                        self.stdout.extend(result.as_bytes());
                    }
                    BasicFunction::Warning => {
                        let result = format_string(args)?;
                        self.stdout.extend(b"Warning: ");
                        self.stdout.extend(result.as_bytes());
                        self.stdout.push(b'\n');
                    }
                    BasicFunction::LittleEndian => {
                        self.little_endian = true;
                    }
                    BasicFunction::BigEndian => {
                        self.little_endian = false;
                    }
                }

                self.push(return_value);
            }
            Instruction::Return => {
                self.exit_code_block()?;
            }
            Instruction::ReturnVoid => {
                self.push(Object::Void);
                self.exit_code_block()?;
            }
            Instruction::GetArrayIndex => {
                let object = self.pop();
                let index = self.pop_u64()? as usize;

                let new_object = match object {
                    Object::Array(number_array) => match number_array {
                        NumberArray::Char(v) => Object::new_char(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::U8(v) => Object::new_u8(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::I8(v) => Object::new_i8(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::U16(v) => Object::new_u16(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::I16(v) => Object::new_i16(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::U32(v) => Object::new_u32(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::I32(v) => Object::new_i32(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::U64(v) => Object::new_u64(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::I64(v) => Object::new_i64(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::F32(v) => Object::new_f32(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                        NumberArray::F64(v) => Object::new_f64(
                            v.into_iter()
                                .nth(index)
                                .ok_or(AnalyzeError::IndexOutOfBounds)?,
                        ),
                    },
                    Object::VariableRef(variable_ref) => {
                        let variable = self.variable(variable_ref);
                        match variable {
                            Object::Array(number_array) => {
                                if index >= number_array.len() {
                                    return Err(AnalyzeError::IndexOutOfBounds);
                                }
                            }
                            Object::ArrayRef { size, .. } => {
                                if index as u64 >= *size {
                                    return Err(AnalyzeError::IndexOutOfBounds);
                                }
                            }
                            _ => {
                                return Err(AnalyzeError::GenericError(
                                    "Can't index an object that isn't an array",
                                ))
                            }
                        }
                        Object::ArrayEntryRef {
                            variable_ref,
                            index: index as u64,
                        }
                    }
                    _ => return Err(AnalyzeError::GenericError("Can't index this type")),
                };
                self.push(new_object);
            }
            Instruction::GetMember(_) => todo!(),
            Instruction::DeclareArrayValues(size) => {
                let mut values = (0..*size)
                    .map(|_| self.pop_resolve())
                    .collect::<AnalyzeResult<Vec<_>>>()?;
                values.reverse();
                for value in values.iter() {
                    if !matches!(value.as_type(), ObjectType::Number(_)) {
                        return Err(AnalyzeError::GenericError("Can only declare numeric types"));
                    }
                }
                self.push(Object::TempArray(values));
            }
            // Suffixed changes the variable _after_ returning it's value
            Instruction::SuffixIncrement => {
                let target_ref = self.pop_ref()?;
                match target_ref {
                    Object::VariableRef(variable_ref) => {
                        let value = self.variable(variable_ref);
                        let new_value = value.clone().add(Object::new_u8(1))?;
                        self.push(value.clone());
                        *self.variable_mut(variable_ref) = new_value;
                    }
                    Object::ArrayEntryRef {
                        variable_ref,
                        index,
                    } => {
                        todo!();
                    }
                    _ => unreachable!(),
                }
            }
            Instruction::SuffixDecrement => {
                let target_ref = self.pop_ref()?;
                match target_ref {
                    Object::VariableRef(variable_ref) => {
                        let value = self.variable(variable_ref);
                        let new_value = value.clone().subtract(Object::new_u8(1))?;
                        self.push(value.clone());
                        *self.variable_mut(variable_ref) = new_value;
                    }
                    Object::ArrayEntryRef {
                        variable_ref,
                        index,
                    } => {
                        todo!();
                    }
                    _ => unreachable!(),
                }
            }
            Instruction::PrefixIncrement => {
                let target_ref = self.pop_ref()?;
                match target_ref {
                    Object::VariableRef(variable_ref) => {
                        let value = self.variable_mut(variable_ref);
                        let new_value = value.clone().add(Object::new_u8(1))?;
                        *value = new_value.clone();
                        self.push(new_value);
                    }
                    Object::ArrayEntryRef {
                        variable_ref,
                        index,
                    } => {
                        todo!();
                    }
                    _ => unreachable!(),
                }
            }
            Instruction::PrefixDecrement => {
                let target_ref = self.pop_ref()?;
                match target_ref {
                    Object::VariableRef(variable_ref) => {
                        let value = self.variable_mut(variable_ref);
                        let new_value = value.clone().subtract(Object::new_u8(1))?;
                        *value = new_value.clone();
                        self.push(new_value);
                    }
                    Object::ArrayEntryRef {
                        variable_ref,
                        index,
                    } => {
                        todo!();
                    }
                    _ => unreachable!(),
                }
            }
            Instruction::Positive => {}
            Instruction::Negate => unary_instr!(negate),
            Instruction::UnaryLogicalNot => unary_instr!(logical_not),
            Instruction::UnaryBitwiseNot => unary_instr!(bitwise_not),
            Instruction::Multiply => binary_instr!(multiply),
            Instruction::Divide => binary_instr!(divide),
            Instruction::Modulus => binary_instr!(modulus),
            Instruction::Add => binary_instr!(add),
            Instruction::Subtract => binary_instr!(subtract),
            Instruction::LeftShift => binary_instr!(left_shift),
            Instruction::RightShift => binary_instr!(right_shift),
            Instruction::LessThan => binary_instr!(less_than),
            Instruction::LessThanOrEqual => binary_instr!(less_than_or_equal),
            Instruction::MoreThan => binary_instr!(more_than),
            Instruction::MoreThanOrEqual => binary_instr!(more_than_or_equal),
            Instruction::Equal => binary_instr!(equal),
            Instruction::NotEqual => binary_instr!(not_equal),
            Instruction::BitwiseAnd => binary_instr!(bitwise_and),
            Instruction::BitwiseXor => binary_instr!(bitwise_xor),
            Instruction::BitwiseOr => binary_instr!(bitwise_or),
            Instruction::LogicalAnd => binary_instr!(logical_and),
            Instruction::LogicalOr => binary_instr!(logical_or),
            Instruction::Assign => {
                let target_ref = self.pop_ref()?;
                let value = self.pop_resolve()?;
                match target_ref {
                    Object::VariableRef(variable_ref) => {
                        match value {
                            // Special case: Assign temp values to an array
                            Object::TempArray(values) => {
                                let Object::Array(array) = self.variable_mut(variable_ref) else {
                            return Err(AnalyzeError::GenericError("not an array"));
                        };
                                if array.len() != values.len() {
                                    return Err(AnalyzeError::GenericError("Wrong size array"));
                                }
                                array.set_values(values)?;
                            }
                            value => {
                                *self.variable_mut(variable_ref) = value;
                            }
                        }
                        self.push(Object::VariableRef(variable_ref));
                    }
                    Object::ArrayEntryRef {
                        variable_ref,
                        index,
                    } => {
                        todo!();
                    }
                    _ => unreachable!(),
                }
            }
            Instruction::AssignAdd => assign_instr!(add),
            Instruction::AssignSubtract => assign_instr!(subtract),
            Instruction::AssignMultiply => assign_instr!(multiply),
            Instruction::AssignDivide => assign_instr!(divide),
            Instruction::AssignModulus => assign_instr!(modulus),
            Instruction::AssignLeftShift => assign_instr!(left_shift),
            Instruction::AssignRightShift => assign_instr!(right_shift),
            Instruction::AssignBitwiseAnd => assign_instr!(bitwise_and),
            Instruction::AssignBitwiseXor => assign_instr!(bitwise_xor),
            Instruction::AssignBitwiseOr => assign_instr!(bitwise_or),
            Instruction::Label(_) => (),
            Instruction::Jump(label_ref) => {
                let offset = code_block.label_offsets[label_ref.0 as usize];
                self.curr_code_block_context_mut().instruction_pointer = offset;
            }
            Instruction::JumpTrue(label_ref) => {
                let label_ref_index = label_ref.0 as usize;
                let value = match self.pop_resolve()? {
                    Object::Number(v) => v.as_bool(),
                    _ => return Err(AnalyzeError::GenericError("not a bool")),
                };
                if value {
                    let offset = self.curr_code_block().label_offsets[label_ref_index];
                    self.curr_code_block_context_mut().instruction_pointer = offset;
                }
            }
            Instruction::JumpFalse(label_ref) => {
                let label_ref_index = label_ref.0 as usize;
                let value = match self.pop_resolve()? {
                    Object::Number(v) => v.as_bool(),
                    _ => return Err(AnalyzeError::GenericError("not a bool")),
                };
                if !value {
                    let offset = self.curr_code_block().label_offsets[label_ref_index];
                    self.curr_code_block_context_mut().instruction_pointer = offset;
                }
            }
        }

        self.curr_code_block_context_mut().instruction_pointer += 1;
        Ok(())
    }

    fn push(&mut self, object: Object) {
        self.object_stack.push(object);
    }

    fn pop(&mut self) -> Object {
        self.object_stack.pop().unwrap()
    }

    fn pop_resolve(&mut self) -> AnalyzeResult<Object> {
        let object = self.pop();
        self.resolve_object(object)
    }

    fn pop_ref(&mut self) -> AnalyzeResult<Object> {
        let object = self.pop();
        match &object {
            Object::VariableRef(_) | Object::ArrayEntryRef { .. } => Ok(object),
            _ => Err(AnalyzeError::GenericError("not a ref")),
        }
    }

    fn pop_u64(&mut self) -> AnalyzeResult<u64> {
        match self.pop_resolve()? {
            Object::Number(v) => Ok(v.as_u64()),
            _ => Err(AnalyzeError::GenericError("Popped stack item not a number")),
        }
    }

    fn can_execute(&self) -> bool {
        !self.code_block_stack.is_empty()
    }

    fn enter_function(&mut self, function_ref: FunctionRef) {
        self.code_block_stack.push(CodeBlockContext {
            variables: HashMap::new(),
            code_block_ref: CodeBlockRef::Function(function_ref),
            instruction_pointer: 0,
            color: None,
        });
    }

    fn enter_struct(&mut self, struct_ref: StructRef) {
        self.code_block_stack.push(CodeBlockContext {
            variables: HashMap::new(),
            code_block_ref: CodeBlockRef::Struct(struct_ref),
            instruction_pointer: 0,
            color: None,
        })
    }

    fn exit_code_block(&mut self) -> AnalyzeResult<()> {
        match self.curr_code_block_context().code_block_ref {
            CodeBlockRef::Function(_) => {
                self.push(Object::Void);
                let stack_top = self.object_stack.last().unwrap();
                if self.curr_code_block().return_type != stack_top.as_type() {
                    return Err(AnalyzeError::GenericError("Function returned wrong type"));
                }
            }
            CodeBlockRef::Struct(_) => todo!(),
        }

        self.code_block_stack.pop();

        Ok(())
    }

    fn curr_code_block(&self) -> &CodeBlock {
        match self.code_block_stack.last().unwrap().code_block_ref {
            CodeBlockRef::Function(fn_ref) => &self.program.functions[fn_ref.0 as usize],
            CodeBlockRef::Struct(struct_ref) => &self.program.structs[struct_ref.0 as usize],
        }
    }

    fn curr_code_block_context(&self) -> &CodeBlockContext {
        self.code_block_stack.last().unwrap()
    }

    fn curr_code_block_context_mut(&mut self) -> &mut CodeBlockContext {
        self.code_block_stack.last_mut().unwrap()
    }

    fn read_object(&mut self, object_type: ObjectType) -> AnalyzeResult<Object> {
        Ok(match object_type {
            ObjectType::Number(number_type) => match number_type {
                NumberType::Char => Object::new_char(self.reader.read_u8()?),
                NumberType::U8 => Object::new_u8(self.reader.read_u8()?),
                NumberType::I8 => Object::new_i8(self.reader.read_i8()?),
                NumberType::U16 => Object::new_u16(if self.little_endian {
                    self.reader.read_u16()?
                } else {
                    self.reader.read_u16_be()?
                }),
                NumberType::I16 => Object::new_i16(if self.little_endian {
                    self.reader.read_i16()?
                } else {
                    self.reader.read_i16_be()?
                }),
                NumberType::U32 => Object::new_u32(if self.little_endian {
                    self.reader.read_u32()?
                } else {
                    self.reader.read_u32_be()?
                }),
                NumberType::I32 => Object::new_i32(if self.little_endian {
                    self.reader.read_i32()?
                } else {
                    self.reader.read_i32_be()?
                }),
                NumberType::U64 => Object::new_u64(if self.little_endian {
                    self.reader.read_u64()?
                } else {
                    self.reader.read_u64_be()?
                }),
                NumberType::I64 => Object::new_i64(if self.little_endian {
                    self.reader.read_i64()?
                } else {
                    self.reader.read_i64_be()?
                }),
                NumberType::F32 => Object::new_f32(if self.little_endian {
                    self.reader.read_f32()?
                } else {
                    self.reader.read_f32_be()?
                }),
                NumberType::F64 => Object::new_f64(if self.little_endian {
                    self.reader.read_f64()?
                } else {
                    self.reader.read_f64_be()?
                }),
            },
            ObjectType::Array(NumberType::Char) => {
                let mut value = Vec::new();
                loop {
                    let next = self.reader.read_u8()?;
                    value.push(next);
                    if next == 0 {
                        break;
                    }
                }
                Object::Array(NumberArray::Char(value))
            }
            _ => return Err(AnalyzeError::GenericError("invalid read type")),
        })
    }

    fn read_array_lazy(&mut self, object_type: ObjectType, size: u64) -> AnalyzeResult<Object> {
        match object_type {
            ObjectType::Void => Err(AnalyzeError::GenericError("can't read array of voids")),
            ObjectType::Number(number_type) => {
                let start = self.reader.position()?;
                let size = size * (number_type.bitsize() / 8) as u64;
                self.reader.seek(SeekFrom::Current(size as i64))?;
                Ok(Object::ArrayRef {
                    number_type,
                    start,
                    size,
                })
            }
            ObjectType::Array(_) | ObjectType::ArrayRef(_) | ObjectType::Struct(_) => {
                Err(AnalyzeError::GenericError("can't read array of this type"))
            }
        }
    }

    fn resolve_variable(&mut self, variable_ref: VariableRef) -> AnalyzeResult<&Object> {
        let variable = self.variable(variable_ref);

        if let Object::ArrayRef {
            number_type,
            start,
            size,
        } = variable
        {
            let number_type = *number_type;
            let start = *start;
            let size = *size;

            let reader_pos = self.reader.position()?;
            self.reader.seek(SeekFrom::Start(start))?;

            macro_rules! read_array {
                ($func:ident) => {
                    (0..size)
                        .map(|_| self.reader.$func())
                        .collect::<std::io::Result<Vec<_>>>()?
                };
            }
            let result = Object::Array(match number_type {
                NumberType::Char => NumberArray::Char(read_array!(read_u8)),
                NumberType::U8 => NumberArray::U8(read_array!(read_u8)),
                NumberType::I8 => NumberArray::I8(read_array!(read_i8)),
                NumberType::U16 => NumberArray::U16(if self.little_endian {
                    read_array!(read_u16)
                } else {
                    read_array!(read_u16_be)
                }),
                NumberType::I16 => NumberArray::I16(if self.little_endian {
                    read_array!(read_i16)
                } else {
                    read_array!(read_i16_be)
                }),
                NumberType::U32 => NumberArray::U32(if self.little_endian {
                    read_array!(read_u32)
                } else {
                    read_array!(read_u32_be)
                }),
                NumberType::I32 => NumberArray::I32(if self.little_endian {
                    read_array!(read_i32)
                } else {
                    read_array!(read_i32_be)
                }),
                NumberType::U64 => NumberArray::U64(if self.little_endian {
                    read_array!(read_u64)
                } else {
                    read_array!(read_u64_be)
                }),
                NumberType::I64 => NumberArray::I64(if self.little_endian {
                    read_array!(read_i64)
                } else {
                    read_array!(read_i64_be)
                }),
                NumberType::F32 => NumberArray::F32(if self.little_endian {
                    read_array!(read_f32)
                } else {
                    read_array!(read_f32_be)
                }),
                NumberType::F64 => NumberArray::F64(if self.little_endian {
                    read_array!(read_f64)
                } else {
                    read_array!(read_f64_be)
                }),
            });

            self.reader.seek(SeekFrom::Start(reader_pos))?;

            *self.variable_mut(variable_ref) = result;
        }

        Ok(self.variable(variable_ref))
    }

    fn variable(&self, variable_ref: VariableRef) -> &Object {
        self.code_block_stack
            .iter()
            .rev()
            .find_map(|sc| sc.variables.get(&variable_ref))
            .unwrap()
    }

    fn variable_mut(&mut self, variable_ref: VariableRef) -> &mut Object {
        self.code_block_stack
            .iter_mut()
            .rev()
            .find_map(|sc| sc.variables.get_mut(&variable_ref))
            .unwrap()
    }

    fn initialize_variable(&mut self, variable_ref: VariableRef, value: Object) {
        assert!(!matches!(value, Object::VariableRef(_)));
        self.curr_code_block_context_mut()
            .variables
            .insert(variable_ref, value);
    }

    fn resolve_object(&mut self, object: Object) -> AnalyzeResult<Object> {
        Ok(match object {
            Object::VariableRef(variable_ref) => self.variable(variable_ref).clone(),
            Object::ArrayEntryRef {
                variable_ref,
                index,
            } => {
                let index = index as usize;
                let variable = self.resolve_variable(variable_ref)?;
                match variable {
                    Object::Array(number_array) => match number_array {
                        NumberArray::Char(v) => Object::new_char(v[index]),
                        NumberArray::U8(v) => Object::new_u8(v[index]),
                        NumberArray::I8(v) => Object::new_i8(v[index]),
                        NumberArray::U16(v) => Object::new_u16(v[index]),
                        NumberArray::I16(v) => Object::new_i16(v[index]),
                        NumberArray::U32(v) => Object::new_u32(v[index]),
                        NumberArray::I32(v) => Object::new_i32(v[index]),
                        NumberArray::U64(v) => Object::new_u64(v[index]),
                        NumberArray::I64(v) => Object::new_i64(v[index]),
                        NumberArray::F32(v) => Object::new_f32(v[index]),
                        NumberArray::F64(v) => Object::new_f64(v[index]),
                    },
                    _ => return Err(AnalyzeError::GenericError("Not an array")),
                }
            }
            o => o,
        })
    }
}

fn format_string(args: Vec<Object>) -> AnalyzeResult<String> {
    // https://www.sweetscape.com/010editor/manual/FuncInterface.htm#Printf

    if args.is_empty() {
        return Err(AnalyzeError::GenericError("format has no args"));
    }

    let mut args = args.into_iter();

    let format = match args.next().unwrap() {
        Object::Array(NumberArray::Char(v)) => v,
        _ => return Err(AnalyzeError::GenericError("format not a string")),
    };

    macro_rules! next_arg {
        () => {
            args.next()
                .ok_or_else(|| AnalyzeError::GenericError("more args than format specifiers"))?
        };
    }

    macro_rules! cast_arg {
        ($obj_type:ident) => {
            args.next()
                .ok_or_else(|| AnalyzeError::GenericError("more args than format specifiers"))?
                .cast(ObjectType::Number(NumberType::$obj_type))?
        };
    }

    let mut result = String::new();
    let mut escape = false;
    let mut escape_long = false;
    let mut escape_double = false;
    for v in format.iter() {
        if escape {
            match *v as char {
                'd' | 'i' => result += &cast_arg!(I32).as_string(),
                'u' => result += &cast_arg!(U32).as_string(),
                'x' => result += &cast_arg!(I32).as_hex_string(),
                'X' => result += &cast_arg!(I32).as_upper_hex_string(),
                'o' => result += &cast_arg!(I32).as_octal_string(),
                'c' => result.push(cast_arg!(U8).get_u8() as char),
                's' => result += &next_arg!().as_string(),
                'f' | 'e' | 'g' => result += &cast_arg!(F32).as_string(),
                'l' => escape_double = true,
                'L' => escape_long = true,
                _ => return Err(AnalyzeError::GenericError("invalid escape sequence")),
            }
            escape = false;
        } else if escape_long {
            match *v as char {
                'd' => result += &cast_arg!(I64).as_string(),
                'u' => result += &cast_arg!(U64).as_string(),
                'x' => result += &cast_arg!(U64).as_hex_string(),
                'X' => result += &cast_arg!(U64).as_upper_hex_string(),
                _ => return Err(AnalyzeError::GenericError("invalid escape sequence")),
            };
            escape_long = false;
        } else if escape_double {
            match *v as char {
                'd' => result += &cast_arg!(F64).as_string(),
                _ => return Err(AnalyzeError::GenericError("invalid escape sequence")),
            }
            escape_double = false;
        } else {
            match *v as char {
                '%' => escape = true,
                c => result.push(c),
            }
        }
    }

    Ok(result)
}
