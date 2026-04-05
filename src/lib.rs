extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro]
pub fn schema(input: TokenStream) -> TokenStream {
    // Note: macro parsing is handled in floz_macros_core with syn::parse2 directly,
    // so we convert input to proc_macro2::TokenStream and let the core handle parsing and generation
    let input2: proc_macro2::TokenStream = input.into();
    let schema_input = match syn::parse2::<floz_macros_core::ast::SchemaInput>(input2) {
        Ok(s) => s,
        Err(e) => return e.to_compile_error().into(),
    };
    floz_macros_core::codegen::generate(&schema_input).into()
}

#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream) -> TokenStream {
    floz_macros_core::route::expand_route(attr.into(), item.into()).into()
}

#[proc_macro_attribute]
pub fn task(attr: TokenStream, item: TokenStream) -> TokenStream {
    floz_macros_core::task::expand_task(attr.into(), item.into()).into()
}

#[proc_macro_attribute]
pub fn main(_attr: TokenStream, item: TokenStream) -> TokenStream {
    floz_macros_core::route::expand_main(item.into()).into()
}
