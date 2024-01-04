#![feature(box_patterns, iterator_try_collect)]

use proc_macro::TokenStream;

mod discover_tests;
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
