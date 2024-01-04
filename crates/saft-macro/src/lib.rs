#![feature(box_patterns, iterator_try_collect)]

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{PatIdent, TypeReference};

enum Parameter {
    Normal(syn::Type),
    Span { by_ref: bool },
    Interpreter,
}

fn parse_param(arg: &syn::FnArg) -> Parameter {
    match arg {
        syn::FnArg::Typed(syn::PatType {
            box ty, box pat, ..
        }) => match pat {
            syn::Pat::Ident(PatIdent { ident, .. }) if ident == "interpreter" => {
                match ty {
                    syn::Type::Reference(TypeReference { .. }) => {}
                    _ => panic!("Must take interpreter by reference"),
                }
                Parameter::Interpreter
            }
            syn::Pat::Ident(PatIdent { ident, .. }) if ident == "span" => match ty {
                syn::Type::Reference(TypeReference { .. }) => Parameter::Span { by_ref: true },
                _ => Parameter::Span { by_ref: false },
            },
            _ => Parameter::Normal(ty.clone()),
        },
        syn::FnArg::Receiver(_) => panic!("`self` is not allowed for native functions"),
    }
}

fn expand_native_function(mut fn_: syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let name = fn_.sig.ident.clone();
    let name_s = name.to_string();
    let inner = format_ident!("inner");
    fn_.sig.ident = inner.clone();

    let params: Vec<_> = fn_.sig.inputs.iter().map(parse_param).collect();

    let mut inner_args = Vec::<(syn::Ident, proc_macro2::TokenStream)>::new();

    let mut arg_i = 0usize;
    for (i, param) in params.iter().enumerate() {
        let ident = format_ident!("arg{}", i);

        let expr = match param {
            Parameter::Normal(ty) => {
                let ts = parse_normal(arg_i, ty);
                arg_i += 1;
                ts
            }
            Parameter::Interpreter => {
                quote!(interpreter)
            }
            Parameter::Span { by_ref } => {
                if *by_ref {
                    quote!(span)
                } else {
                    quote!(span.clone())
                }
            }
        };

        inner_args.push((ident, expr));
    }
    let normal_params = arg_i;

    let (arg_ident, arg_expr): (Vec<_>, Vec<_>) = inner_args.iter().cloned().unzip();

    Ok(quote! {
        #[allow(non_camel_case_types)]
        #[derive(Debug)]
        struct #name {}

        impl NativeFunc for #name {
            fn data() -> NativeFuncData {
                fn #name(interpreter: &mut Interpreter, span: &Span, args: Vec<Spanned<Value>>) -> Result<Value, ControlFlow> {
                    #fn_

                    if args.len() != #normal_params {
                        return Err(Exception::ArgMismatch {
                            span: span.clone(),
                            expected: #normal_params,
                            got: args.len(),
                        }.into())
                    }

                    #(let #arg_ident = #arg_expr;)*

                    let res: NativeRes = #inner(#(#arg_ident),*).into();
                    res.0
                }

                NativeFuncData {
                    name: #name_s,
                    f: #name,
                }
            }
        }
    })
}

fn parse_normal(i: usize, ty: &syn::Type) -> proc_macro2::TokenStream {
    quote! {
        {
            let arg = &args[#i];
            Cast::<#ty>::cast(arg.v.clone()).ok_or::<ControlFlow>(
                cast_error!(arg.clone(),
                    <#ty as CastFrom::<Value>>::ty_name()
                )
            )?
        }
    }
}

/// Create a custom native function
///
/// Uses the `CastFrom` trait to cast dynamic values into the desired type, note that this can be
/// slow for large data structures like strings and vectors, then it may be more appropriate to
/// take the value itself and match it on your own.
///
/// It permits two special parameters, `span` and `interpreter` which give you the span of the call
/// and the interpreter reference respectively.
#[proc_macro_attribute]
pub fn native_function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    expand_native_function(syn::parse_macro_input!(item as syn::ItemFn))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
