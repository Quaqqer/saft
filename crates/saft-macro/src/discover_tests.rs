use quote::{format_ident, quote};
use std::{collections::HashMap, path::PathBuf};

use darling::{ast::NestedMeta, FromMeta};
use glob::glob;
use proc_macro::TokenStream;

#[derive(FromMeta)]
#[darling()]
struct Args {
    root: String,
    glob: String,
}

#[derive(Debug)]
enum TestNode {
    Node(HashMap<String, TestNode>),
    Test(PathBuf),
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
    let dirs = glob(PathBuf::from(&args.root).join(&args.glob).to_str().unwrap()).unwrap();

    let mut root_node = TestNode::Node(HashMap::new());

    for dir in dirs {
        let path = dir.unwrap();

        let root_s = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join(&args.root)
            .canonicalize()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join(&path)
            .canonicalize()
            .unwrap();

        let parts = PathBuf::from((file_path.clone()).strip_prefix(&root_s).unwrap())
            .components()
            .map(|c| c.as_os_str().to_str().unwrap().to_string())
            .collect::<Vec<_>>();

        let mut curr = &mut root_node;

        let parts = parts.iter().enumerate().collect::<Vec<_>>();

        for (i, part) in &parts {
            let last = *i == parts.len() - 1;

            const CLASH_MSG: &str = "File and directory name clash, a directory and a name cannot exist with the same name in the same path";

            let TestNode::Node(children) = curr else {
                panic!("{}", CLASH_MSG)
            };

            if last {
                let prev = children.insert(part.to_string(), TestNode::Test(file_path.clone()));

                if prev.is_some() {
                    panic!("{}", CLASH_MSG);
                }
            } else {
                children
                    .entry(part.to_string())
                    .or_insert(TestNode::Node(HashMap::new()));

                curr = if let TestNode::Node(children) = curr {
                    children.get_mut(*part).unwrap()
                } else {
                    panic!("Was just inserted?")
                };
            }
        }
    }

    fn create_tests(
        node: &TestNode,
        test_fn_ident: &proc_macro2::Ident,
    ) -> proc_macro2::TokenStream {
        match node {
            TestNode::Node(children) => {
                let mut toks = vec![];

                for (dir, node) in children.iter() {
                    match node {
                        TestNode::Node(_) => {
                            let id = format_ident!("{}", dir);
                            let node_quote = create_tests(node, test_fn_ident);
                            toks.push(quote! {
                            mod #id {
                                #node_quote
                            }

                                })
                        }
                        TestNode::Test(_) => {
                            toks.push(create_tests(node, test_fn_ident));
                        }
                    }
                }

                quote!(#(#toks)*)
            }
            TestNode::Test(file_path) => {
                let test_name = format_ident!(
                    "{}",
                    file_path
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string()
                        .split_once('.')
                        .unwrap()
                        .0
                        .to_string()
                );

                let file_path_s = file_path.to_str().unwrap().to_string();

                quote! {
                    #[test]
                    fn #test_name() {
                        #test_fn_ident(#file_path_s);
                    }
                }
            }
        }
    }

    let tests = create_tests(&root_node, test_fn_ident);

    Ok(quote! {
        #test_fn

        #tests
    })
}
