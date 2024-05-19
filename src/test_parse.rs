use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, LitInt, LitStr, Token};

pub struct ResponseSpec {
    pub status: u16,
    pub description: String,
    pub content_type: Option<String>,
    pub schema_type: Option<syn::TypePath>,
}

impl Parse for ResponseSpec {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);

        let status_lit: LitInt = content.parse()?;
        let status: u16 = status_lit.base10_parse()?;

        content.parse::<Token![,]>()?;
        let desc_lit: LitStr = content.parse()?;
        let description = desc_lit.value();

        let mut content_type = None;
        let mut schema_type = None;

        if content.peek(Token![,]) {
            content.parse::<Token![,]>()?;
            if !content.is_empty() {
                if content.peek(LitStr) {
                    let ct_lit: LitStr = content.parse()?;
                    content_type = Some(ct_lit.value());
                } else {
                    let path: syn::TypePath = content.parse()?;
                    schema_type = Some(path);
                }
            }
        }

        Ok(ResponseSpec { status, description, content_type, schema_type })
    }
}
pub fn main() {}
