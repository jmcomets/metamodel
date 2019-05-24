pub trait Model {
    fn to_type() -> Type;
}

#[derive(Debug, PartialEq)]
pub struct Type {
    field_types: Box<[FieldType]>,
}

impl Type {
    pub fn new(field_types: Box<[FieldType]>) -> Self {
        Type { field_types }
    }

    pub fn field_types(&self) -> impl Iterator<Item=&FieldType> {
        self.field_types.iter()
    }
}

#[derive(Debug)]
pub struct FieldId(i8);

#[derive(Debug, PartialEq)]
pub enum FieldType {
    Unit,
    Byte,
    Bool,
    Int,
    Long,
    Float,
    Double,
    Str,
    List(Box<FieldType>),
    Dict(Box<FieldType>, Box<FieldType>),
    Tuple(Box<[FieldType]>),
}

#[derive(Debug, PartialEq)]
pub struct FieldTypeId(i8);
