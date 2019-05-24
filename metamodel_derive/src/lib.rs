extern crate proc_macro;
extern crate proc_macro2;

use std::fmt;

use quote::{quote, ToTokens};
use syn;

#[proc_macro_derive(Model)]
pub fn into_model_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let output: proc_macro2::TokenStream = {
        let name = &ast.ident;

        let fields = {
            match ast.data {
                syn::Data::Struct(data) => {
                    match data.fields {
                        syn::Fields::Named(fields)   => to_fields(&fields.named),
                        syn::Fields::Unnamed(fields) => to_fields(&fields.unnamed),
                        syn::Fields::Unit            => quote! { vec![] },
                    }
                }

                syn::Data::Enum(_)  => unimplemented!("enum types are not supported"),
                syn::Data::Union(_) => unimplemented!("union types are not supported"),
            }
        };

        quote! {
            impl Model for #name {
                fn to_type() -> Type {
                    Type::new(vec![#fields])
                }
            }
        }
    };

    proc_macro::TokenStream::from(output)
}

fn to_fields<T>(punctuated: &syn::punctuated::Punctuated<syn::Field, T>) -> proc_macro2::TokenStream {
    punctuated.iter()
        .map(|field| to_field_type(&field.ty))
        .collect()
}

fn to_field_type(ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        // TODO map to a list
        syn::Type::Slice(_slice) => unimplemented!("\"Slice\" types are not supported"),
        syn::Type::Array(_array) => unimplemented!("\"Array\" types are not supported"),

        syn::Type::Tuple(type_tuple) => {
            let mut element_types = vec![];

            for ty in type_tuple.elems.iter() {
                element_types.push(to_field_type(ty));
            }

            let element_types = quote! { vec![ #( #element_types )* ] };
            return quote! { ::metamodel::FieldType::Tuple(#element_types), };
        }

        syn::Type::Path(type_path) => {
            macro_rules! idents {
                ($($x:expr),*) => {
                    &[
                        $(
                            ::syn::parse_str::<::syn::Ident>($x).unwrap()
                        ),*
                    ]
                }
            }

            if let Some(ref pair) = type_path.path.segments.last() {
                let segment = pair.value();

                if segment_matches(segment, idents!["i8"]) {
                    return quote! { ::metamodel::FieldType::Byte, };
                }

                if segment_matches(segment, idents!["i32"]) {
                    return quote! { ::metamodel::FieldType::Int, };
                }

                if segment_matches(segment, idents!["i64"]) {
                    return quote! { ::metamodel::FieldType::Long, };
                }

                if segment_matches(segment, idents!["bool"]) {
                    return quote! { ::metamodel::FieldType::Bool, };
                }

                if segment_matches(segment, idents!["f32"]) {
                    return quote! { ::metamodel::FieldType::Float, };
                }

                if segment_matches(segment, idents!["f64"]) {
                    return quote! { ::metamodel::FieldType::Double, };
                }

                if segment_matches(segment, idents!["String", "str"]) {
                    return quote! { ::metamodel::FieldType::Str, };
                }

                if segment_matches(segment, idents!["Vec", "VecDeque", "LinkedList"]) {
                    if let syn::PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                        let mut element_type = None;

                        for arg in generic_args.args.iter() {
                            if let syn::GenericArgument::Type(ty) = arg {
                                element_type = Some(ty);
                                break;
                            }
                        }

                        if let Some(element_type) = element_type {
                            let element_field_type = to_field_type(&element_type);
                            return quote! { ::metamodel::FieldType::List(Box::new(#element_field_type)), };
                        }
                    }

                    panic!("unsupported list type {}", PathDisplayExt(&type_path.path));
                }

                if segment_matches(segment, idents!["HashMap", "BTreeMap"]) {
                    if let syn::PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                        let mut key_type = None;
                        let mut value_type = None;

                        for arg in generic_args.args.iter() {
                            if let syn::GenericArgument::Type(ty) = arg {
                                if key_type.is_some() {
                                    value_type = Some(ty);
                                    break;
                                } else {
                                    key_type = Some(ty);
                                }
                            }
                        }

                        if let (Some(key_type), Some(value_type)) = (key_type, value_type) {
                            let key_field_type = to_field_type(&key_type);
                            let value_field_type = to_field_type(&value_type);
                            return quote! { ::metamodel::FieldType::Dict(Box::new(#key_field_type), Box::new(#value_field_type)), };
                        }
                    }

                    panic!("unsupported map type {}", PathDisplayExt(&type_path.path));
                }

                if segment_matches(segment, idents!["Box"]) {
                    if let syn::PathArguments::AngleBracketed(ref generic_args) = segment.arguments {
                        let mut element_type = None;

                        for arg in generic_args.args.iter() {
                            if let syn::GenericArgument::Type(ty) = arg {
                                element_type = Some(ty);
                                break;
                            }
                        }

                        if let Some(element_type) = element_type {
                            return to_field_type(&element_type);
                        }
                    }

                    panic!("unsupported box type {}", PathDisplayExt(&type_path.path));
                }
            }

            panic!("unsupported type {}", PathDisplayExt(&type_path.path))
        }

        syn::Type::Reference(type_reference) => to_field_type(&type_reference.elem),

        syn::Type::Ptr(_type_ptr)                  => panic!("\"Ptr\" types are not supported"),
        syn::Type::BareFn(_type_bare_fn)           => panic!("\"BareFn\" types are not supported"),
        syn::Type::Never(_type_never)              => panic!("\"Never\" types are not supported"),
        syn::Type::TraitObject(_type_trait_object) => panic!("\"TraitObject\" types are not supported"),
        syn::Type::ImplTrait(_type_impl_trait)     => panic!("\"ImplTrait\" types are not supported"),
        syn::Type::Paren(_type_paren)              => panic!("\"Paren\" types are not supported"),
        syn::Type::Group(_type_group)              => panic!("\"Group\" types are not supported"),
        syn::Type::Infer(_type_infer)              => panic!("\"Infer\" types are not supported"),
        syn::Type::Macro(_type_macro)              => panic!("\"Macro\" types are not supported"),
        syn::Type::Verbatim(_type_verbatim)        => panic!("\"Verbatim\" types are not supported"),
    }
}

struct PathDisplayExt<'a>(&'a syn::Path);

impl<'a> fmt::Display for PathDisplayExt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.into_token_stream().fmt(f)
    }
}

fn segment_matches<'a, T>(segment: &syn::PathSegment, candidates: T) -> bool
    where T: IntoIterator<Item=&'a syn::Ident>
{
    candidates.into_iter()
        .any(|ident| &segment.ident == ident)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
