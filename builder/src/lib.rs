extern crate proc_macro;

use core::panic;

use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;


fn ty_inner_ty<'a> (wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type>{
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = &p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn builder_of(f: syn::Field) -> bool {
    for attr in &f.attrs {
        if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
            return true;
        }
    }
    false
}
fn extended_method (f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)>{
    let name = f.ident.as_ref().unwrap();
    for attr in &f.attrs {
        if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
            match  &attr.meta {
                syn::Meta::List(list) => {
                    let mut tokens = list.tokens.clone().into_iter();
                    match tokens.next().unwrap() {
                        TokenTree::Ident(ref i) => assert_eq!(i.to_string(), "each"),
                        tt => panic!("Unexpected token: {:?}", tt),
                    }
                    match tokens.next().unwrap() {
                        TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
                        tt => panic!("Expected '=', found: {}", tt),
                    }
                    let arg = match tokens.next().unwrap() {
                        TokenTree::Literal(l) => l,
                        tt => panic!("Expected string, found: {}", tt),
                    };
                    match syn::Lit::new(arg) {
                        syn::Lit::Str(s) => {
                            let method_name = syn::Ident::new(&s.value(), s.span());
                            let inner_ty = ty_inner_ty("Vec", &f.ty).unwrap();
                            let method = quote!{
                                pub fn #method_name(&mut self, #method_name: #inner_ty) -> &mut Self{
                                    self.#name.push(#method_name);
                                    self
                                }
                            };
                            return Some((&method_name == name, method));
                        },
                        lit => panic!("Expected string, found: {:?}", lit),
                    }

                },
                mm => {panic!("Unexpected meta type: {:?}", mm)},

            };
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&builder_name, name.span());
    let fields = if let syn::Data::Struct(
        syn::DataStruct{
            fields: syn::Fields::Named(
                syn::FieldsNamed {
                    ref named, ..
                }
            ),
            ..
        }
    ) = ast.data {
        named
    } else {
        panic!("Builder derive only works with structs")
    };


    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_ty("Option", ty).is_some() || builder_of(f.clone()) {
            quote!{
                #name: #ty
            }
        } else {
            quote!{
                #name: std::option::Option<#ty>
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let set_method = if let Some(inner_ty) = ty_inner_ty("Option", ty)  {
            quote!{
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self{
                    self.#name = Some(#name);
                    self
                }
            }
        } else if builder_of(f.clone())  {
            quote!{
                pub fn #name(&mut self, #name: #ty) -> &mut Self{
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote!{
                pub fn #name(&mut self, #name: #ty) -> &mut Self{
                    self.#name = Some(#name);
                    self
                }
            }
        };

        match extended_method(&f) {

            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => quote!(
                #set_method
                #extend_method
            ),
            None => set_method,

        }

    });
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if ty_inner_ty("Option", &f.ty).is_some() || builder_of(f.clone()){
            quote!{
                #name: self.#name.clone()
            }
        } else {
            quote!{
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " field is required"))?
            }
        }

    });
    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f.clone()) {
            quote!{
                #name: Vec::new()
            }
        } else {
            quote!{
                #name: None
            }
        }

    });
    let expanded = quote!(
        struct #builder_ident {
            #(#optionized,)*
        }
        impl #name {
            pub fn builder() -> #builder_ident{
                #builder_ident{
                    #(#build_empty,)*
                }
            }
        }
        impl #builder_ident{
            #(#methods)*
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>>{
                Ok(#name{
                    #(#build_fields,)*
                })
            }
        }
    );
    expanded.into()
}