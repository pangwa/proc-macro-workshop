use proc_macro::TokenStream;
extern crate proc_macro2;
extern crate syn;
use proc_macro2::Span;

#[macro_use]
extern crate quote;

fn extract_type_from_vec(ty: &syn::Type) -> Option<&syn::Type> {
    use syn::{GenericArgument, Path, PathArguments, PathSegment};

    fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
        match *ty {
            syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
            _ => None,
        }
    }

    fn extract_vec_segment(path: &Path) -> Option<&PathSegment> {
        let idents_of_path = path
            .segments
            .iter()
            .into_iter()
            .fold(String::new(), |mut acc, v| {
                acc.push_str(&v.ident.to_string());
                acc.push('|');
                acc
            });
        vec!["Vec|"]
            .into_iter()
            .find(|s| &idents_of_path == *s)
            .and_then(|_| path.segments.last())
    }

    extract_type_path(ty)
        .and_then(|path| extract_vec_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;
            // It should have only on angle-bracketed param ("<String>"):
            match *type_params {
                PathArguments::AngleBracketed(ref params) => params.args.first(),
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

fn extract_type_from_option(ty: &syn::Type) -> Option<&syn::Type> {
    use syn::{GenericArgument, Path, PathArguments, PathSegment};

    fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
        match *ty {
            syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
            _ => None,
        }
    }

    fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
        let idents_of_path = path
            .segments
            .iter()
            .into_iter()
            .fold(String::new(), |mut acc, v| {
                acc.push_str(&v.ident.to_string());
                acc.push('|');
                acc
            });
        vec!["Option|", "std|option|Option|", "core|option|Option|"]
            .into_iter()
            .find(|s| &idents_of_path == *s)
            .and_then(|_| path.segments.last())
    }

    extract_type_path(ty)
        .and_then(|path| extract_option_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;
            // It should have only on angle-bracketed param ("<String>"):
            match *type_params {
                PathArguments::AngleBracketed(ref params) => params.args.first(),
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::ItemStruct = syn::parse(input).unwrap();
    let name = &ast.ident;
    let builder_name = format!("{}BuilderImpl", name);
    let builder_type = syn::Ident::new(&builder_name, Span::call_site());
    let mut init_values = quote!();
    let mut fields_vec = quote!();
    let mut setter_vec = quote!();
    let mut each_setter_vec = quote!();
    let mut builder_vec = quote!();
    let mut result_vec = quote!();

    let mut error: Option<proc_macro2::TokenStream> = None;

    match ast.fields {
        syn::Fields::Named(named) => {
            for field in named.named.iter() {
                let fname = field.ident.as_ref().unwrap();
                let ty = &field.ty;
                let maybe_option_ty = extract_type_from_option(ty);
                let maybe_vec_ty = extract_type_from_vec(ty);
                let each_attr = field
                    .attrs
                    .iter()
                    .map(|attr| {
                        if !attr.path.is_ident("builder") {
                            return None;
                        }
                        let meta: syn::Meta =
                            attr.parse_args().expect("builder requires each field");
                        match meta.clone() {
                            syn::Meta::NameValue(m) => {
                                if !m.path.is_ident("each") {
                                    error = Some(
                                        syn::Error::new_spanned(
                                            meta,
                                            r#"expected `builder(each = "...")`"#,
                                        )
                                        .to_compile_error(),
                                    );
                                    return None;
                                }
                                match m.lit {
                                    syn::Lit::Str(s) => {
                                        Some(syn::Ident::new(&s.value(), Span::call_site()))
                                    }
                                    _ => None,
                                }
                            }
                            _ => None,
                        }
                    })
                    .find(|v| v.is_some())
                    .flatten();
                if error.is_some() {
                    break;
                }

                let (filed_ty, setter_ty) =
                    match (maybe_option_ty, maybe_vec_ty, each_attr.is_some()) {
                        (Some(_), _, _) => {
                            init_values.extend(quote! {
                                #fname: core::option::Option::None,
                            });
                            result_vec.extend(quote! {
                                #fname: self.#fname.clone(),
                            });
                            (quote!(#ty), quote!(#maybe_option_ty))
                        }
                        (None, Some(_), true) => {
                            init_values.extend(quote! {
                                #fname: std::vec::Vec::new(),
                            });

                            result_vec.extend(quote! {
                                #fname: self.#fname.clone(),
                            });
                            (quote!(#ty), quote!(#maybe_vec_ty))
                        }
                        _ => {
                            init_values.extend(quote! {
                                #fname: None,
                            });
                            builder_vec.extend(quote! {
                                if self.#fname == core::option::Option::None {
                                    return Err("field missing".into());
                                }
                            });

                            result_vec.extend(quote! {
                                #fname: self.#fname.clone().unwrap(),
                            });

                            (quote!(core::option::Option<#ty>), quote!(#ty))
                        }
                    };

                fields_vec.extend(quote! {
                    #fname: #filed_ty,
                });

                if each_attr.is_none() {
                    setter_vec.extend(quote! {
                        fn #fname(&mut self, v: #setter_ty) -> &mut Self {
                            self.#fname = Some(v);
                            self
                        }
                    });
                }

                match each_attr {
                    None => (),
                    Some(name) => {
                        each_setter_vec.extend(quote! {
                        fn #name(&mut self, v: #maybe_vec_ty) -> &mut Self {
                                  self.#fname.push(v);
                                  self
                             }
                        });
                    }
                }
            }
        }
        _ => unimplemented!(),
    }
    if error.is_some() {
        let e = error.unwrap();
        return quote! {
            #e
        }
        .into();
    }

    let gen = quote! {
        use std::error::Error;

        pub struct #builder_type{
            #fields_vec
        }

        impl #builder_type {
            #each_setter_vec

            #setter_vec

            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #builder_vec
                std::result::Result::Ok(#name { #result_vec })
            }

            fn new() -> Self {
                Self { #init_values }
            }
        }

        impl #name {
            fn builder() -> #builder_type{
                #builder_type::new()
            }
        }

    };

    gen.into()
}
