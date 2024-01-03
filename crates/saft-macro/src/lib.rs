#![feature(box_patterns)]

use proc_macro::TokenStream;
use quote::{format_ident, quote};

fn parse_param_ty(arg: &syn::FnArg) -> syn::Type {
    match arg {
        syn::FnArg::Typed(syn::PatType { ty: box ty, .. }) => ty.clone(),
        _ => panic!("Hej"),
    }
}

#[proc_macro_attribute]
pub fn native_function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut fn_ = syn::parse_macro_input!(item as syn::ItemFn);

    let name = fn_.sig.ident.clone();
    let name_s = name.to_string();
    let inner = format_ident!("inner");
    fn_.sig.ident = inner.clone();
    let n_params = fn_.sig.inputs.len();

    let arg_tys = fn_.sig.inputs.iter().map(|input| parse_param_ty(input));
    let arg_i = 0..arg_tys.len();

    quote! {
        #[allow(non_camel_case_types)]
        #[derive(Debug)]
        struct #name {}

        impl NativeFunc for #name {
            fn data() -> NativeFuncData {
                fn #name(args: Vec<Spanned<Value>>) -> Result<Value, ControlFlow> {
                    #fn_

                    assert!(args.len() == #n_params);

                    let res: NativeRes = #inner(#(Cast::<#arg_tys>::cast(args[#arg_i].clone())?),*).into();
                    res.0
                }

                NativeFuncData {
                    name: #name_s,
                    f: #name,
                }
            }
        }
    }
    .into()
}
