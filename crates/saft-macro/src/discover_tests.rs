use std::path::PathBuf;

use darling::{ast::NestedMeta, FromMeta};
use glob::glob;
use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[derive(FromMeta)]
#[darling()]
struct Args {
    path: String,
}

pub fn expand_discover_tests(
    attr: TokenStream,
    test_fn: syn::ItemFn,
) -> syn::Result<proc_macro2::TokenStream> {
    let args = match NestedMeta::parse_meta_list(attr.into()) {
        Ok(v) => match Args::from_list(&v) {
            Ok(v) => v,
            Err(e) => {
                return Ok(e.write_errors());
            }
        },
        Err(e) => {
            return Ok(darling::Error::from(e).write_errors());
        }
    };

    let test_fn_ident = &test_fn.sig.ident.clone();
    let dirs = glob(&args.path).unwrap();

    let tests = dirs
        .map(|path| {
            let path = path.unwrap();
            let file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../..")
                .join(
                    path.to_str()
                        .unwrap()
                        .to_string()
                        .strip_suffix(".out")
                        .unwrap(),
                )
                .to_str()
                .unwrap()
                .to_string();
            let file_expected = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../..")
                .join(path.clone())
                .to_str()
                .unwrap()
                .to_string();

            let test_name = format_ident!(
                "{}_{}",
                test_fn_ident,
                path.file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string()
                    .strip_suffix(".saf.out")
                    .unwrap()
            );

            quote! {
                #[test]
                fn #test_name() {
                    #test_fn_ident(#file_path, #file_expected);
                }
            }
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        #test_fn

        #(#tests)*
    })
}
