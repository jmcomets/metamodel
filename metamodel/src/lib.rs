use blake2::{Digest, Blake2b};

use generic_array::GenericArray;
use generic_array::typenum::U64;

pub trait Model {
    fn to_type() -> Type;
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Unit,
    Byte,
    Bool,
    Int,
    Long,
    Float,
    Double,
    Str,
    List(Box<Type>),
    Dict(Box<Type>, Box<Type>),
    Tuple(Box<[Type]>),
}

impl Type {
    pub fn to_type_id(&self) -> TypeId {
        TypeId({
            let mut ids = vec![];
            to_flattened_ids(self, &mut ids);
            Blake2b::digest(&ids)
        })
    }
}

const UNIT_TYPE: u8 = 0;
const BYTE_TYPE: u8 = 1;
const BOOL_TYPE: u8 = 2;
const INT_TYPE: u8 = 3;
const LONG_TYPE: u8 = 4;
const FLOAT_TYPE: u8 = 5;
const DOUBLE_TYPE: u8 = 6;
const STR_TYPE: u8 = 7;
const LIST_TYPE: u8 = 8;
const DICT_TYPE: u8 = 9;
const TUPLE_TYPE: u8 = 10;

type TypeDigest = GenericArray<u8, U64>;

fn to_flattened_ids(ty: &Type, buf: &mut Vec<u8>) {
    use Type::*;
    match ty {
        Unit   => { buf.push(UNIT_TYPE); }
        Byte   => { buf.push(BYTE_TYPE); }
        Bool   => { buf.push(BOOL_TYPE); }
        Int    => { buf.push(INT_TYPE); }
        Long   => { buf.push(LONG_TYPE); }
        Float  => { buf.push(FLOAT_TYPE); }
        Double => { buf.push(DOUBLE_TYPE); }
        Str    => { buf.push(STR_TYPE); }

        List(element_ty) => {
            buf.push(LIST_TYPE);
            to_flattened_ids(element_ty, buf);
        }

        Dict(key_ty, value_ty) => {
            buf.push(DICT_TYPE);
            to_flattened_ids(key_ty, buf);
            to_flattened_ids(value_ty, buf);
        }

        Tuple(types) => {
            buf.push(TUPLE_TYPE);
            buf.push(types.len() as u8);
            for ty in types.iter() {
                to_flattened_ids(ty, buf);
            }
        }
    }
}

fn from_flattened_ids<T>(it: &mut T) -> Type
    where T: Iterator<Item=u8> + Clone,
{
    match it.next() {
        Some(UNIT_TYPE)   => Type::Unit,
        Some(BYTE_TYPE)   => Type::Byte,
        Some(BOOL_TYPE)   => Type::Bool,
        Some(INT_TYPE)    => Type::Int,
        Some(LONG_TYPE)   => Type::Long,
        Some(FLOAT_TYPE)  => Type::Float,
        Some(DOUBLE_TYPE) => Type::Double,
        Some(STR_TYPE)    => Type::Str,

        Some(LIST_TYPE) => {
            let element_type = from_flattened_ids(it);
            Type::List(Box::new(element_type))
        }

        Some(DICT_TYPE) => {
            let key_type = from_flattened_ids(it);
            let value_type = from_flattened_ids(it);
            Type::Dict(Box::new(key_type), Box::new(value_type))
        }

        Some(TUPLE_TYPE) => {
            if let Some(len) = it.next() {
                let mut types = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    types.push(from_flattened_ids(it));
                }
                Type::Tuple(types.into_boxed_slice())
            } else {
                unimplemented!();
            }
        }

        _ => unimplemented!(),
    }
}

/// A type id identifies the entire Type it was built from.
///
/// The main idea behind a TypeId is that you can save a considerable amount of space by leading
/// a payload of data by mapping directly to the underlying type.
///
/// By default, uniqueness of TypeIds is 100% guaranteed.
///
/// This is a sane default for good performance, however if the type's size is problematic, you can
/// add use a less secure identification method. The trade-off here is that a invalid TypeId to
/// Type mapping will at best cause a transmission error, at worst corrupted data.
///
/// TODO add hashing option
#[derive(Debug, PartialEq)]
pub struct TypeId(TypeDigest);
