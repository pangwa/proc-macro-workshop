use proc_macro::TokenStream;
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
use proc_macro2::Span;

#[macro_use]
extern crate quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::ItemStruct = syn::parse(input).unwrap();
    let name = &ast.ident;
    let builder_name = format!("{}BuilderImpl", name);
    let builder_type = syn::Ident::new(&builder_name, Span::call_site());
    let mut init_values = quote!();
    let mut fields_vec = quote!();
    let mut setter_vec = quote!();
    let mut builder_vec = quote!();
    let mut result_vec = quote!();
    match ast.fields {
        syn::Fields::Named(named) => {
            for field in named.named.iter() {
                let fname = field.ident.as_ref().unwrap();
                let ty = &field.ty;
                init_values.extend(quote! {
                    #fname: None,
                });
                fields_vec.extend(quote! {
                    #fname: Option<#ty>,
                });
                setter_vec.extend(quote! {
                    fn #fname(&mut self, v: #ty) -> &mut Self {
                        self.#fname = Some(v);
                        self
                    }
                });
                builder_vec.extend(quote! {
                    if self.#fname == None {
                        return None;
                    }
                });
                result_vec.extend(quote! {
                    #fname: self.#fname.clone().unwrap(),
                });
            }
        }
        _ => unimplemented!(),
    }

    let gen = quote! {
        pub struct #builder_type{
            #fields_vec
        }

        impl #builder_type {
            #setter_vec

            fn build(&self) -> Option<#name> {
                #builder_vec
                Some(#name { #result_vec })
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
