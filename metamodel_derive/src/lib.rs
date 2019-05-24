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

        let ty = {
            match ast.data {
                syn::Data::Struct(data) => {
                    match data.fields {
                        syn::Fields::Named(fields) => {
                            let fields = to_fields(&fields.named);
                            quote! {
                                ::metamodel::Type::Tuple(Box::new([#fields]))
                            }
                        }

                        syn::Fields::Unnamed(fields) => {
                            let fields = to_fields(&fields.unnamed);
                            quote! {
                                ::metamodel::Type::Tuple(Box::new([#fields]))
                            }
                        }

                        syn::Fields::Unit => {
                            quote! {
                                ::metamodel::Type::Unit
                            }
                        },
                    }
                }

                syn::Data::Enum(_)  => unimplemented!("enum types are not supported"),
                syn::Data::Union(_) => unimplemented!("union types are not supported"),
            }
        };

        quote! {
            impl Model for #name {
                fn to_type() -> Type {
                    #ty
                }
            }
        }
    };

    proc_macro::TokenStream::from(output)
}

fn to_fields<T>(punctuated: &syn::punctuated::Punctuated<syn::Field, T>) -> proc_macro2::TokenStream {
    punctuated.iter()
        .map(|field| type_to_field_type(&field.ty))
        .collect()
}

macro_rules! idents {
    ($($x:expr),*) => {
        &[
            $(
                ::syn::parse_str::<::syn::Ident>($x).unwrap()
            ),*
        ]
    }
}

fn type_to_field_type(ty: &syn::Type) -> proc_macro2::TokenStream {
    match ty {
        syn::Type::Path(type_path) => {
            if let Some(ref pair) = type_path.path.segments.last() {
                path_segment_to_field_type(pair.value(), &type_path.path)
            } else {
                unreachable!("empty type {}", PathDisplayExt(&type_path.path))
            }
        }

        syn::Type::Tuple(type_tuple) => {
            let element_types = type_tuple.elems.iter()
                .map(type_to_field_type);

            quote! {
                ::metamodel::Type::Tuple(Box::new([
                    #( #element_types )*
                ])),
            }
        }

        // TODO map to the list type
        syn::Type::Array(_array) => unimplemented!("array types are not supported"),

        // TODO clarify if reference types can be supported
        //
        // To support reference types, ownership needs to be addressed. Things can be tricky to
        // get right, especially for ser/de features. Slice types probably have the same issue.
        syn::Type::Reference(_type_reference) => unimplemented!("reference types are not supported"),

        // TODO clarify if slice types can be supported (see support for reference types)
        syn::Type::Slice(_slice) => unimplemented!("slice types are not supported"),

        // no need to implement these
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

fn path_segment_to_field_type(segment: &syn::PathSegment, parent_path: &syn::Path) -> proc_macro2::TokenStream {
    // TODO either convert to signed or extend field types
    if segment_matches(segment, idents!["u8", "u32", "u64"]) {
        unimplemented!("unsupported unsigned type {}", PathDisplayExt(parent_path));
    }

    if segment_matches(segment, idents!["i8"]) {
        return quote! { ::metamodel::Type::Byte, };
    }

    if segment_matches(segment, idents!["i32"]) {
        return quote! { ::metamodel::Type::Int, };
    }

    if segment_matches(segment, idents!["i64"]) {
        return quote! { ::metamodel::Type::Long, };
    }

    if segment_matches(segment, idents!["bool"]) {
        return quote! { ::metamodel::Type::Bool, };
    }

    if segment_matches(segment, idents!["f32"]) {
        return quote! { ::metamodel::Type::Float, };
    }

    if segment_matches(segment, idents!["f64"]) {
        return quote! { ::metamodel::Type::Double, };
    }

    if segment_matches(segment, idents!["String", "str"]) {
        return quote! { ::metamodel::Type::Str, };
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
                let element_field_type = type_to_field_type(&element_type);
                return quote! { ::metamodel::Type::List(Box::new(#element_field_type)), };
            }
        }

        panic!("unsupported list type {}", PathDisplayExt(parent_path));
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
                let key_field_type = type_to_field_type(&key_type);
                let value_field_type = type_to_field_type(&value_type);
                return quote! { ::metamodel::Type::Dict(Box::new(#key_field_type), Box::new(#value_field_type)), };
            }
        }

        panic!("unsupported map type {}", PathDisplayExt(parent_path));
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
                return type_to_field_type(&element_type);
            }
        }

        panic!("unsupported box type {}", PathDisplayExt(parent_path));
    }

    panic!("unsupported type {}", PathDisplayExt(parent_path))
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
