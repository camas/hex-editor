use std::{collections::HashMap, io::SeekFrom, num::Wrapping, rc::Rc};

use common::{DataSourceTraits, Reader};
use log::debug;

use crate::{
    parse::{BasicFunction, BasicObject, FunctionArg, ObjectRef, Statement},
    transform::{Attribute, BtProgram, Function, FunctionRef, Instruction, VariableRef},
};

use self::{
    number::Number,
    parsed::{ParsedObject, ParsedObjects},
};

pub(crate) mod number;
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
    #[error("Can't add {0:?} to {1:?}")]
    InvalidAddType(Object, Object),
    #[error("Invalid cast target {0:?}")]
    InvalidCastTarget(ObjectRef),
    #[error("Can't {0} on {1:?}")]
    InvalidUnaryOperation(&'static str, Object),
    #[error("Can't {0} on {1:?} and {2:?}")]
    InvalidBinaryOperation(&'static str, Object, Object),
    // TODO: Replace with actual errors instead of being lazy
    #[error("{0}")]
    GenericError(&'static str),
    #[error("Int operation on float")]
    IntOperationOnFloat,
}

type AnalyzeResult<T> = Result<T, AnalyzeError>;

#[derive(Debug, Clone)]
struct Struct {
    args: Vec<FunctionArg>,
    statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Void,
    Number(Number),
    CharArray(Rc<Vec<u8>>),
    U8Array(Rc<Vec<u8>>),
    I8Array(Rc<Vec<i8>>),
    I16Array(Rc<Vec<i16>>),
    U16Array(Rc<Vec<u16>>),
    U32Array(Rc<Vec<u32>>),
    I32Array(Rc<Vec<i32>>),
    U64Array(Rc<Vec<u64>>),
    I64Array(Rc<Vec<i64>>),
    F32Array(Rc<Vec<f32>>),
    F64Array(Rc<Vec<f64>>),
    VariableRef(VariableRef),
    // TODO: Add reference arrays that only read values if needed.
    // Check if necessary first. Might not speed up performance much at all
}

#[derive(Debug)]
struct ExecutionContext<'a> {
    program: &'a BtProgram,
    function_stack: Vec<FunctionContext>,
    reader: Reader<&'a mut Box<dyn DataSourceTraits>>,
    object_stack: Vec<Object>,
    parsed_objects: ParsedObjects,
    little_endian: bool,
    stdout: Vec<u8>,
}

#[derive(Debug)]
struct FunctionContext {
    variables: HashMap<VariableRef, Object>,
    function_ref: FunctionRef,
    instruction_pointer: usize,
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
        function_stack: Vec::new(),
        reader,
        object_stack: Vec::new(),
        parsed_objects: ParsedObjects::default(),
        little_endian: true,
        stdout: Vec::new(),
    };

    context.enter_function(program.root_function_index);
    while context.can_execute() {
        context.step()?;
    }

    // TODO: Throw errors instead. Should probably remove any assert in this file
    assert!(context.function_stack.is_empty());
    // Script is treated as a function so returns void when exiting
    assert_eq!(context.object_stack, vec![Object::Void]);

    Ok(AnalyzedData {
        parsed_objects: context.parsed_objects.objects,
        stdout: context.stdout,
    })
}

impl<'a> ExecutionContext<'a> {
    fn step(&mut self) -> AnalyzeResult<()> {
        let function_context = self.curr_function_context();
        let function = self.curr_function();

        if function_context.instruction_pointer >= function.instructions.len() {
            self.push(Object::Void);
            self.exit_function()?;
            return Ok(());
        }

        macro_rules! unary_instr {
            ($method:ident) => {{
                let result = self.pop_resolve().$method()?;
                self.push(result);
            }};
        }

        macro_rules! binary_instr {
            ($method:ident) => {{
                let result = self.pop_resolve().$method(self.pop_resolve())?;
                self.push(result);
            }};
        }

        macro_rules! assign_instr {
            ($method:ident) => {{
                let variable_ref = self.pop_ref()?;
                let value = self.pop_resolve();
                let new_value = self
                    .variable(variable_ref)
                    .unwrap()
                    // TODO: Shouldn't need to be cloned
                    .clone()
                    .$method(value)?;
                self.set_variable(variable_ref, new_value);
                self.push(Object::VariableRef(variable_ref));
            }};
        }

        let instruction = &function.instructions[function_context.instruction_pointer];

        match instruction {
            Instruction::Pop => {
                self.pop();
            }
            Instruction::PopVariable(variable_ref) => {
                let variable_ref = *variable_ref;
                let value = self.pop();
                self.set_variable(variable_ref, value);
            }
            Instruction::PushVariable(variable_ref) => {
                let variable_ref = *variable_ref;
                self.push(Object::VariableRef(variable_ref));
            }
            Instruction::PushString(value) => {
                self.push(Object::CharArray(Rc::new(value.clone())));
            }
            Instruction::PushWideString(value) => {
                // TODO: Handle wide strings properly
                self.push(Object::CharArray(Rc::new(value.clone())));
            }
            Instruction::PushBool(value) => {
                self.push(Object::Number(Number::U8(Wrapping(u8::from(*value)))));
            }
            Instruction::PushChar(value) => {
                self.push(Object::new_i8(*value));
            }
            Instruction::PushU8(value) => {
                self.push(Object::Number(Number::U8(Wrapping(*value))));
            }
            Instruction::PushU32(value) => {
                self.push(Object::Number(Number::U32(Wrapping(*value))));
            }
            Instruction::PushI32(value) => {
                self.push(Object::Number(Number::I32(Wrapping(*value))));
            }
            Instruction::PushU64(value) => {
                self.push(Object::Number(Number::U64(Wrapping(*value))));
            }
            Instruction::PushI64(value) => {
                self.push(Object::Number(Number::I64(Wrapping(*value))));
            }
            Instruction::PushF32(value) => {
                self.push(Object::Number(Number::F32(*value)));
            }
            Instruction::PushF64(value) => {
                self.push(Object::Number(Number::F64(*value)));
            }
            Instruction::DeclareObject {
                variable_ref,
                object_ref,
            } => {
                let object = match object_ref {
                    ObjectRef::Basic(basic_type) => match basic_type {
                        BasicObject::U8 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::I8 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::U16 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::I16 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::U32 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::I32 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::U64 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::I64 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::F32 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::F64 => Object::Number(Number::U8(Wrapping(0))),
                        BasicObject::String => Object::CharArray(Rc::new(Vec::new())),
                    },
                    _ => return Err(AnalyzeError::GenericError("invalid local declaration type")),
                };
                self.set_variable(*variable_ref, object);
            }
            Instruction::DeclareArray {
                variable_ref,
                object_ref,
            } => {
                let variable_ref = *variable_ref;
                let object_ref = *object_ref;

                let size = match self.pop_resolve() {
                    Object::Number(v) => v.as_u64(),
                    _ => return Err(AnalyzeError::GenericError("Invalid type for array size")),
                } as usize;
                let object = match object_ref {
                    ObjectRef::Basic(basic) => match basic {
                        BasicObject::U8 => Object::U8Array(Rc::new(vec![0; size])),
                        BasicObject::I8 => Object::I8Array(Rc::new(vec![0; size])),
                        BasicObject::U16 => Object::U16Array(Rc::new(vec![0; size])),
                        BasicObject::I16 => Object::I16Array(Rc::new(vec![0; size])),
                        BasicObject::U32 => Object::U32Array(Rc::new(vec![0; size])),
                        BasicObject::I32 => Object::I32Array(Rc::new(vec![0; size])),
                        BasicObject::U64 => Object::U64Array(Rc::new(vec![0; size])),
                        BasicObject::I64 => Object::I64Array(Rc::new(vec![0; size])),
                        BasicObject::F32 => Object::F32Array(Rc::new(vec![0.; size])),
                        BasicObject::F64 => Object::F64Array(Rc::new(vec![0.; size])),
                        BasicObject::String => todo!(),
                    },
                    _ => todo!(),
                };
                self.set_variable(variable_ref, object);
            }
            Instruction::ReadObject {
                name,
                variable_ref,
                object_ref,
                arg_count,
                attributes,
            } => {
                if *arg_count > 0 {
                    todo!();
                }

                let mut bg_color = None;
                for attribute in attributes {
                    match attribute {
                        Attribute::BackgroundColor(v) => bg_color = Some(*v),
                    }
                }

                let variable_ref = *variable_ref;
                let object_ref = *object_ref;
                let name = name.clone();

                let start = self.reader.position()?;
                let value = self.read_object(object_ref)?;
                let end = self.reader.position()?.max(start + 1) - 1;

                self.set_variable(variable_ref, value.clone());
                self.parsed_objects.add(name, value, start, end, bg_color);
            }
            Instruction::ReadArray {
                name,
                variable_ref,
                object_ref,
                attributes,
            } => {
                let variable_ref = *variable_ref;
                let object_ref = *object_ref;
                let name = name.clone();

                let mut bg_color = None;
                for attribute in attributes {
                    match attribute {
                        Attribute::BackgroundColor(v) => bg_color = Some(*v),
                    }
                }

                let size = match self.pop_resolve() {
                    Object::Number(v) => v.as_u64(),
                    _ => return Err(AnalyzeError::GenericError("Invalid type for array size")),
                } as usize;

                let start = self.reader.position()?;
                let value = self.read_array(object_ref, size)?;
                let end = self.reader.position()?.max(start + 1) - 1;

                self.set_variable(variable_ref, value.clone());
                self.parsed_objects.add(name, value, start, end, bg_color);
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

                self.curr_function_context_mut().instruction_pointer += 1;

                self.enter_function(function_ref);

                let mut args = (0..arg_count)
                    .map(|_| self.pop_resolve())
                    .collect::<Vec<_>>();

                let function = self.curr_function();

                if function.args.len() != arg_count {
                    return Err(AnalyzeError::GenericError(
                        "Wrong number of arguments for function",
                    ));
                }

                for (arg, arg_ref) in args.iter_mut().zip(function.args.iter()) {
                    if arg.as_object_ref() != arg_ref.0 {
                        if matches!(arg, Object::Number(_))
                            && matches!(arg_ref.0, ObjectRef::Basic(_))
                        {
                            // TODO: Probably a better way than this
                            let mut swap_arg = Object::Void;
                            std::mem::swap(arg, &mut swap_arg);
                            *arg = swap_arg.cast(arg_ref.0)?;
                        } else {
                            return Err(AnalyzeError::GenericError("Invalid argument type"));
                        }
                    }
                }

                let var_refs = function.args.iter().map(|a| a.1).collect::<Vec<_>>();
                for (arg, var_ref) in args.into_iter().zip(var_refs.into_iter()) {
                    self.set_variable(var_ref, arg);
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
                    .collect::<Vec<_>>();

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
                self.exit_function()?;
            }
            Instruction::ReturnVoid => {
                self.push(Object::Void);
                self.exit_function()?;
            }
            Instruction::GetArrayIndex => todo!(),
            Instruction::GetMember(_) => todo!(),
            Instruction::DeclareArrayValues(_) => todo!(),
            // These change the value of a variable before/after returning a value
            // Can't just pop from stack
            Instruction::SuffixIncrement => {
                let variable_ref = self.pop_ref()?;
                let value = self.variable(variable_ref).unwrap().clone();
                self.push(value.clone());
                let value = value.add(Object::new_u8(1))?;
                self.set_variable(variable_ref, value);
            }
            Instruction::SuffixDecrement => todo!(),
            Instruction::PrefixIncrement => todo!(),
            Instruction::PrefixDecrement => todo!(),
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
                let variable_ref = self.pop_ref()?;
                let value = self.pop_resolve();
                self.set_variable(variable_ref, value);
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
                let offset = function.label_offsets[label_ref.0 as usize];
                self.curr_function_context_mut().instruction_pointer = offset;
            }
            Instruction::JumpTrue(label_ref) => {
                let label_ref_index = label_ref.0 as usize;
                let value = match self.pop_resolve() {
                    Object::Number(v) => v.as_bool(),
                    _ => return Err(AnalyzeError::GenericError("not a bool")),
                };
                if value {
                    let offset = self.curr_function().label_offsets[label_ref_index];
                    self.curr_function_context_mut().instruction_pointer = offset;
                }
            }
            Instruction::JumpFalse(label_ref) => {
                let label_ref_index = label_ref.0 as usize;
                let value = match self.pop_resolve() {
                    Object::Number(v) => v.as_bool(),
                    _ => return Err(AnalyzeError::GenericError("not a bool")),
                };
                if !value {
                    let offset = self.curr_function().label_offsets[label_ref_index];
                    self.curr_function_context_mut().instruction_pointer = offset;
                }
            }
        }

        self.curr_function_context_mut().instruction_pointer += 1;
        Ok(())
    }

    fn push(&mut self, object: Object) {
        self.object_stack.push(object);
    }

    fn pop(&mut self) -> Object {
        self.object_stack.pop().unwrap()
    }

    fn pop_resolve(&mut self) -> Object {
        let object = self.pop();
        self.resolve_object(object)
    }

    fn pop_ref(&mut self) -> AnalyzeResult<VariableRef> {
        match self.pop() {
            Object::VariableRef(variable_ref) => Ok(variable_ref),
            _ => Err(AnalyzeError::GenericError("not a ref")),
        }
    }

    fn can_execute(&self) -> bool {
        !self.function_stack.is_empty()
    }

    fn enter_function(&mut self, function_ref: FunctionRef) {
        self.function_stack.push(FunctionContext {
            variables: HashMap::new(),
            function_ref,
            instruction_pointer: 0,
        });
    }

    fn exit_function(&mut self) -> AnalyzeResult<()> {
        let stack_top = self.object_stack.last().unwrap();

        if self.curr_function().return_type != stack_top.as_object_ref() {
            return Err(AnalyzeError::GenericError(
                "Function needs to return a value",
            ));
        }

        self.function_stack.pop();

        Ok(())
    }

    fn curr_function(&self) -> &Function {
        &self.program.functions[self.function_stack.last().unwrap().function_ref.0 as usize]
    }

    fn curr_function_context(&self) -> &FunctionContext {
        self.function_stack.last().unwrap()
    }

    fn curr_function_context_mut(&mut self) -> &mut FunctionContext {
        self.function_stack.last_mut().unwrap()
    }

    fn read_object(&mut self, object_ref: ObjectRef) -> AnalyzeResult<Object> {
        Ok(match object_ref {
            ObjectRef::Basic(basic) => match basic {
                BasicObject::U8 => Object::new_u8(self.reader.read_u8()?),
                BasicObject::I8 => Object::new_i8(self.reader.read_i8()?),
                BasicObject::U16 => Object::new_u16(if self.little_endian {
                    self.reader.read_u16()?
                } else {
                    self.reader.read_u16_be()?
                }),
                BasicObject::I16 => Object::new_i16(if self.little_endian {
                    self.reader.read_i16()?
                } else {
                    self.reader.read_i16_be()?
                }),
                BasicObject::U32 => Object::new_u32(if self.little_endian {
                    self.reader.read_u32()?
                } else {
                    self.reader.read_u32_be()?
                }),
                BasicObject::I32 => Object::new_i32(if self.little_endian {
                    self.reader.read_i32()?
                } else {
                    self.reader.read_i32_be()?
                }),
                BasicObject::U64 => Object::new_u64(if self.little_endian {
                    self.reader.read_u64()?
                } else {
                    self.reader.read_u64_be()?
                }),
                BasicObject::I64 => Object::new_i64(if self.little_endian {
                    self.reader.read_i64()?
                } else {
                    self.reader.read_i64_be()?
                }),
                BasicObject::F32 => Object::new_f32(if self.little_endian {
                    self.reader.read_f32()?
                } else {
                    self.reader.read_f32_be()?
                }),
                BasicObject::F64 => Object::new_f64(if self.little_endian {
                    self.reader.read_f64()?
                } else {
                    self.reader.read_f64_be()?
                }),
                BasicObject::String => {
                    let mut value = Vec::new();
                    loop {
                        let next = self.reader.read_u8()?;
                        value.push(next);
                        if next == 0 {
                            break;
                        }
                    }
                    Object::CharArray(Rc::new(value))
                }
            },
            _ => todo!(),
        })
    }

    fn read_array(&mut self, object_ref: ObjectRef, size: usize) -> AnalyzeResult<Object> {
        macro_rules! read_array {
            ($func:ident) => {
                (0..size)
                    .map(|_| self.reader.$func())
                    .collect::<std::io::Result<Vec<_>>>()?
            };
        }
        Ok(match object_ref {
            ObjectRef::Basic(basic) => match basic {
                BasicObject::U8 => Object::U8Array(Rc::new(read_array!(read_u8))),
                BasicObject::I8 => Object::I8Array(Rc::new(read_array!(read_i8))),
                BasicObject::U16 => Object::U16Array(Rc::new(if self.little_endian {
                    read_array!(read_u16)
                } else {
                    read_array!(read_u16_be)
                })),
                BasicObject::I16 => Object::I16Array(Rc::new(if self.little_endian {
                    read_array!(read_i16)
                } else {
                    read_array!(read_i16_be)
                })),
                BasicObject::U32 => Object::U32Array(Rc::new(if self.little_endian {
                    read_array!(read_u32)
                } else {
                    read_array!(read_u32_be)
                })),
                BasicObject::I32 => Object::I32Array(Rc::new(if self.little_endian {
                    read_array!(read_i32)
                } else {
                    read_array!(read_i32_be)
                })),
                BasicObject::U64 => Object::U64Array(Rc::new(if self.little_endian {
                    read_array!(read_u64)
                } else {
                    read_array!(read_u64_be)
                })),
                BasicObject::I64 => Object::I64Array(Rc::new(if self.little_endian {
                    read_array!(read_i64)
                } else {
                    read_array!(read_i64_be)
                })),
                BasicObject::F32 => Object::F32Array(Rc::new(if self.little_endian {
                    read_array!(read_f32)
                } else {
                    read_array!(read_f32_be)
                })),
                BasicObject::F64 => Object::F64Array(Rc::new(if self.little_endian {
                    read_array!(read_f64)
                } else {
                    read_array!(read_f64_be)
                })),
                BasicObject::String => todo!(),
            },
            _ => todo!(),
        })
    }

    fn variable(&self, variable_ref: VariableRef) -> Option<&Object> {
        self.function_stack
            .iter()
            .rev()
            .find_map(|sc| sc.variables.get(&variable_ref))
    }

    fn set_variable(&mut self, variable_ref: VariableRef, value: Object) {
        assert!(!matches!(value, Object::VariableRef(_)));
        self.curr_function_context_mut()
            .variables
            .insert(variable_ref, value);
    }

    fn resolve_object(&mut self, object: Object) -> Object {
        match object {
            Object::VariableRef(variable_ref) => self.variable(variable_ref).unwrap().clone(),
            o => o,
        }
    }
}

fn format_string(args: Vec<Object>) -> AnalyzeResult<String> {
    // https://www.sweetscape.com/010editor/manual/FuncInterface.htm#Printf

    if args.is_empty() {
        return Err(AnalyzeError::GenericError("format has no args"));
    }

    let mut args = args.into_iter();

    let format = match args.next().unwrap() {
        Object::CharArray(v) => v,
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
                .cast(ObjectRef::Basic(BasicObject::$obj_type))?
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

macro_rules! unary_number_operation {
    ($operation_name:ident) => {
        fn $operation_name(self) -> AnalyzeResult<Object> {
            Ok(match self {
                Object::Number(a) => Object::Number(Number::$operation_name(a)),
                s => {
                    return Err(AnalyzeError::InvalidUnaryOperation(
                        stringify!($operation_name),
                        s,
                    ))
                }
            })
        }
    };
}

macro_rules! unary_number_result_operation {
    ($operation_name:ident) => {
        fn $operation_name(self) -> AnalyzeResult<Object> {
            Ok(match self {
                Object::Number(a) => Object::Number(Number::$operation_name(a)?),
                s => {
                    return Err(AnalyzeError::InvalidUnaryOperation(
                        stringify!($operation_name),
                        s,
                    ))
                }
            })
        }
    };
}

macro_rules! binary_number_operation {
    ($operation_name:ident) => {
        fn $operation_name(self, other: Object) -> AnalyzeResult<Object> {
            Ok(match (self, other) {
                (Object::Number(a), Object::Number(b)) => Object::Number(a.$operation_name(b)),
                (s, o) => {
                    return Err(AnalyzeError::InvalidBinaryOperation(
                        stringify!($operation_name),
                        s,
                        o,
                    ))
                }
            })
        }
    };
}

macro_rules! binary_number_result_operation {
    ($operation_name:ident) => {
        fn $operation_name(self, other: Object) -> AnalyzeResult<Object> {
            Ok(match (self, other) {
                (Object::Number(a), Object::Number(b)) => Object::Number(a.$operation_name(b)?),
                (s, o) => {
                    return Err(AnalyzeError::InvalidBinaryOperation(
                        stringify!($operation_name),
                        s,
                        o,
                    ))
                }
            })
        }
    };
}

macro_rules! object_num_init {
    ($name:ident, $type:ty, $num_type:ident) => {
        fn $name(value: $type) -> Object {
            Object::Number(Number::$num_type(Wrapping(value)))
        }
    };
}

impl Object {
    object_num_init!(new_u8, u8, U8);
    object_num_init!(new_i8, i8, I8);
    object_num_init!(new_u16, u16, U16);
    object_num_init!(new_i16, i16, I16);
    object_num_init!(new_u32, u32, U32);
    object_num_init!(new_i32, i32, I32);
    object_num_init!(new_u64, u64, U64);
    object_num_init!(new_i64, i64, I64);

    fn new_f32(value: f32) -> Object {
        Object::Number(Number::F32(value))
    }

    fn new_f64(value: f64) -> Object {
        Object::Number(Number::F64(value))
    }

    fn as_object_ref(&self) -> ObjectRef {
        match self {
            Object::Number(n) => ObjectRef::Basic(match n {
                Number::U8(_) => BasicObject::U8,
                Number::I8(_) => BasicObject::I8,
                Number::U16(_) => BasicObject::U16,
                Number::I16(_) => BasicObject::I16,
                Number::U32(_) => BasicObject::U32,
                Number::I32(_) => BasicObject::I32,
                Number::U64(_) => BasicObject::U64,
                Number::I64(_) => BasicObject::I64,
                Number::F32(_) => BasicObject::F32,
                Number::F64(_) => BasicObject::F64,
            }),
            Object::Void => ObjectRef::Void,
            _ => todo!(),
        }
    }

    fn cast(self, target: ObjectRef) -> AnalyzeResult<Object> {
        if let Object::Number(object) = self {
            Ok(match target {
                ObjectRef::Basic(basic) => Object::Number(match basic {
                    BasicObject::U8 => Number::U8(Wrapping(object.as_u64() as u8)),
                    BasicObject::I8 => Number::I8(Wrapping(object.as_u64() as i8)),
                    BasicObject::U16 => Number::U16(Wrapping(object.as_u64() as u16)),
                    BasicObject::I16 => Number::I16(Wrapping(object.as_u64() as i16)),
                    BasicObject::U32 => Number::U32(Wrapping(object.as_u64() as u32)),
                    BasicObject::I32 => Number::I32(Wrapping(object.as_u64() as i32)),
                    BasicObject::U64 => Number::U64(Wrapping(object.as_u64())),
                    BasicObject::I64 => Number::I64(Wrapping(object.as_u64() as i64)),
                    BasicObject::F32 => Number::F32(object.as_f64() as f32),
                    BasicObject::F64 => Number::F64(object.as_f64()),
                    BasicObject::String => return Err(AnalyzeError::InvalidCastTarget(target)),
                }),
                _ => return Err(AnalyzeError::InvalidCastTarget(target)),
            })
        } else {
            Err(AnalyzeError::InvalidCastTarget(target))
        }
    }

    fn as_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::U8(v) => v.0.to_string(),
                Number::I8(v) => v.0.to_string(),
                Number::U16(v) => v.0.to_string(),
                Number::I16(v) => v.0.to_string(),
                Number::U32(v) => v.0.to_string(),
                Number::I32(v) => v.0.to_string(),
                Number::U64(v) => v.0.to_string(),
                Number::I64(v) => v.0.to_string(),
                Number::F32(v) => v.to_string(),
                Number::F64(v) => v.to_string(),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn as_hex_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::U8(v) => format!("{:x}", v.0),
                Number::I8(v) => format!("{:x}", v.0),
                Number::U16(v) => format!("{:x}", v.0),
                Number::I16(v) => format!("{:x}", v.0),
                Number::U32(v) => format!("{:x}", v.0),
                Number::I32(v) => format!("{:x}", v.0),
                Number::U64(v) => format!("{:x}", v.0),
                Number::I64(v) => format!("{:x}", v.0),
                Number::F32(_) => unreachable!(),
                Number::F64(_) => unreachable!(),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn as_upper_hex_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::U8(v) => format!("{:X}", v.0),
                Number::I8(v) => format!("{:X}", v.0),
                Number::U16(v) => format!("{:X}", v.0),
                Number::I16(v) => format!("{:X}", v.0),
                Number::U32(v) => format!("{:X}", v.0),
                Number::I32(v) => format!("{:X}", v.0),
                Number::U64(v) => format!("{:X}", v.0),
                Number::I64(v) => format!("{:X}", v.0),
                Number::F32(_) => unreachable!(),
                Number::F64(_) => unreachable!(),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn as_octal_string(&self) -> String {
        match self {
            Object::Number(number) => match number {
                Number::U8(v) => format!("{:o}", v.0),
                Number::I8(v) => format!("{:o}", v.0),
                Number::U16(v) => format!("{:o}", v.0),
                Number::I16(v) => format!("{:o}", v.0),
                Number::U32(v) => format!("{:o}", v.0),
                Number::I32(v) => format!("{:o}", v.0),
                Number::U64(v) => format!("{:o}", v.0),
                Number::I64(v) => format!("{:o}", v.0),
                Number::F32(_) => unreachable!(),
                Number::F64(_) => unreachable!(),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn get_u8(self) -> u8 {
        match self {
            Object::Number(Number::U8(v)) => v.0,
            _ => unreachable!(),
        }
    }

    fn add(self, other: Object) -> AnalyzeResult<Object> {
        Ok(match (self, other) {
            (Object::Number(a), Object::Number(b)) => Object::Number(a.add(b)),
            (Object::CharArray(a), Object::CharArray(b)) => {
                Object::CharArray(Rc::new([&a[..], &b[..]].concat()))
            }
            (Object::CharArray(a), Object::Number(Number::U8(b)))
            | (Object::Number(Number::U8(b)), Object::CharArray(a)) => {
                let mut a = (*a).clone();
                a.push(b.0);
                Object::CharArray(Rc::new(a))
            }
            (s, o) => return Err(AnalyzeError::InvalidAddType(s, o)),
        })
    }

    unary_number_operation!(negate);
    unary_number_operation!(logical_not);
    unary_number_result_operation!(bitwise_not);
    binary_number_operation!(multiply);
    binary_number_operation!(divide);
    binary_number_operation!(modulus);
    binary_number_operation!(subtract);
    binary_number_result_operation!(left_shift);
    binary_number_result_operation!(right_shift);
    binary_number_operation!(less_than);
    binary_number_operation!(less_than_or_equal);
    binary_number_operation!(more_than);
    binary_number_operation!(more_than_or_equal);
    binary_number_operation!(equal);
    binary_number_operation!(not_equal);
    binary_number_result_operation!(bitwise_and);
    binary_number_result_operation!(bitwise_xor);
    binary_number_result_operation!(bitwise_or);
    binary_number_operation!(logical_and);
    binary_number_operation!(logical_or);
}
