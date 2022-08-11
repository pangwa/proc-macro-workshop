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
    let mut fields_vec_innards = quote!();
    match ast.fields {
        syn::Fields::Named(named) => {
            for field in named.named.iter() {
                let fname = field.ident.as_ref().unwrap();
                let ty = &field.ty;
                fields_vec_innards.extend(quote! {
                    pub fn #fname(&mut self, v: #ty) {

                    }
                });
            }
        }
        _ => unimplemented!(),
    }

    let gen = quote! {
        pub struct #builder_type{
        }

        impl #builder_type {
            #fields_vec_innards
        }

        impl #name {
            fn builder() -> #builder_type{
                #builder_type{}
            }
        }

    };

    gen.into()
}
