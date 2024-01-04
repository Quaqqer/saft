#![feature(box_patterns, iterator_try_collect)]

use proc_macro::TokenStream;

mod native_function;

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
    native_function::expand_native_function(syn::parse_macro_input!(item as syn::ItemFn))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_attribute]
pub fn discover_tests(attr: TokenStream, item: TokenStream) -> TokenStream {
    discover_tests::expand_discover_tests(attr, syn::parse_macro_input!(item as syn::ItemFn))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

mod discover_tests {
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
}
