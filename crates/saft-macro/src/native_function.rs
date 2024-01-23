use quote::{format_ident, quote};
use syn::{PatIdent, TypeReference};

enum Parameter {
    Normal(syn::Type),
    Span { by_ref: bool },
    Vm,
}

fn parse_param(arg: &syn::FnArg) -> Parameter {
    match arg {
        syn::FnArg::Typed(syn::PatType {
            box ty, box pat, ..
        }) => match pat {
            syn::Pat::Ident(PatIdent { ident, .. }) if ident == "vm" => {
                match ty {
                    syn::Type::Reference(TypeReference { .. }) => {}
                    _ => panic!("Must take vm by reference"),
                }
                Parameter::Vm
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

pub fn expand_native_function(fn_: syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let name = fn_.sig.ident.clone();
    let name_s = name.to_string();

    let visibility = fn_.vis.clone();

    let params = fn_.sig.inputs.iter().map(parse_param).collect::<Vec<_>>();

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
            Parameter::Vm => {
                quote!(vm)
            }
            Parameter::Span { by_ref } => {
                if *by_ref {
                    quote!(&span)
                } else {
                    quote!(span.clone())
                }
            }
        };

        inner_args.push((ident, expr));
    }

    let normal_params = arg_i;

    let (rev_arg_ident, rev_arg_expr): (Vec<_>, Vec<_>) = inner_args.iter().rev().cloned().unzip();
    let arg_ident = rev_arg_ident.iter().rev().collect::<Vec<_>>();

    Ok(quote! {
        #[allow(non_upper_case_globals)]
        #visibility const #name: NativeFunction = {
            fn inner(vm: &mut vm::Vm, mut args: Vec<Value>, span: Span) -> Result<Value, vm::Error> {
                #fn_

                if args.len() != #normal_params {
                    return Err(vm::Error::Exotic {
                        message: "Wrong parameters".into(),
                        span,
                        note: Some(format!(
                            "Function expected {} arguments but got {}",
                            #normal_params,
                            args.len()
                        )),
                    });
                }

                #(let #rev_arg_ident = #rev_arg_expr;)*

                NativeRes::from(#name(#(#arg_ident),*)).0
            }

            NativeFunction { f: inner, name: #name_s }
        };
    })
}

fn parse_normal(i: usize, ty: &syn::Type) -> proc_macro2::TokenStream {
    quote! {
        {
            let arg = args.pop().unwrap();
            let casted: #ty = arg.cast().ok_or_else(|| vm::Error::Exotic {
                message: "Cast error".into(),
                span: span.clone(),
                note: Some(format!(
                    "Could not cast argument {} of type {} to {}",
                    #i,
                    arg.ty().name(),
                    <Value as Cast<#ty>>::name(),
                )),
            })?;
            casted
        }
    }
}
