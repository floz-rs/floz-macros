//! `#[route(...)]` attribute proc macro.
//!
//! Parses a single annotation that defines everything about a handler:
//! HTTP method, URL path, tag, description, response specs — and auto-registers it
//! via `inventory::submit!` so no manual route wiring is needed.
//!
//! # Example
//!
//! ```ignore
//! #[route(
//!     get: "/users/:id",
//!     tag: "Users",
//!     desc: "Get a user by ID",
//!     resps: [
//!         (200, "User found"),
//!         (404, "User not found"),
//!     ],
//! )]
//! async fn get_user(ctx: Ctx, Path(id): Path<i32>) -> Result<Json<User>, ApiError> {
//!     // ...
//! }
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Parse, parse::ParseStream, Ident, LitStr, LitInt, Token, ItemFn, Result,
    bracketed, parenthesized,
};

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Attribute parsing
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// A single response specification: (status_code, description, optional content_type)
pub struct ResponseSpec {
    pub status: u16,
    pub description: String,
    pub content_type: Option<String>,
    pub schema_type: Option<syn::TypePath>,
}

/// Parsed contents of `#[route(...)]`
pub struct RouteAttr {
    pub method: HttpMethod,
    pub path: String,
    pub tag: Option<String>,
    pub desc: Option<String>,
    pub resps: Vec<ResponseSpec>,
    pub auth: Option<String>,
    pub rate: Option<String>,
    pub wrap: Vec<syn::Expr>,
}

#[derive(Clone, Copy)]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Patch,
    Delete,
}

impl HttpMethod {
    fn as_str(&self) -> &'static str {
        match self {
            HttpMethod::Get => "get",
            HttpMethod::Post => "post",
            HttpMethod::Put => "put",
            HttpMethod::Patch => "patch",
            HttpMethod::Delete => "delete",
        }
    }

    fn as_ident(&self) -> proc_macro2::Ident {
        proc_macro2::Ident::new(self.as_str(), proc_macro2::Span::call_site())
    }
}

/// Parse a single response tuple: (200, "description") or (200, "description", "text/html")
impl Parse for ResponseSpec {
    fn parse(input: ParseStream) -> Result<Self> {
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
                    // Try to parse as TypePath e.g., Json<User>
                    let path: syn::TypePath = content.parse()?;
                    schema_type = Some(path);
                }
            }
        }

        Ok(ResponseSpec { status, description, content_type, schema_type })
    }
}

impl Parse for RouteAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut method: Option<HttpMethod> = None;
        let mut path: Option<String> = None;
        let mut tag: Option<String> = None;
        let mut desc: Option<String> = None;
        let mut resps: Vec<ResponseSpec> = Vec::new();
        let mut auth: Option<String> = None;
        let mut rate: Option<String> = None;
        let mut wrap: Vec<syn::Expr> = Vec::new();

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "get" => {
                    method = Some(HttpMethod::Get);
                    let lit: LitStr = input.parse()?;
                    path = Some(lit.value());
                }
                "post" => {
                    method = Some(HttpMethod::Post);
                    let lit: LitStr = input.parse()?;
                    path = Some(lit.value());
                }
                "put" => {
                    method = Some(HttpMethod::Put);
                    let lit: LitStr = input.parse()?;
                    path = Some(lit.value());
                }
                "patch" => {
                    method = Some(HttpMethod::Patch);
                    let lit: LitStr = input.parse()?;
                    path = Some(lit.value());
                }
                "delete" => {
                    method = Some(HttpMethod::Delete);
                    let lit: LitStr = input.parse()?;
                    path = Some(lit.value());
                }
                "tag" => {
                    let lit: LitStr = input.parse()?;
                    tag = Some(lit.value());
                }
                "desc" => {
                    let lit: LitStr = input.parse()?;
                    desc = Some(lit.value());
                }
                "resps" => {
                    let content;
                    bracketed!(content in input);
                    while !content.is_empty() {
                        let resp: ResponseSpec = content.parse()?;
                        resps.push(resp);
                        if content.peek(Token![,]) {
                            content.parse::<Token![,]>()?;
                        }
                    }
                }
                "auth" => {
                    // auth: jwt | api_key | none (parsed as ident, not string)
                    let ident: Ident = input.parse()?;
                    auth = Some(ident.to_string());
                }
                "rate" => {
                    let lit: LitStr = input.parse()?;
                    rate = Some(lit.value());
                }
                "wrap" => {
                    let content;
                    syn::bracketed!(content in input);
                    while !content.is_empty() {
                        let expr: syn::Expr = content.parse()?;
                        wrap.push(expr);
                        if content.peek(Token![,]) {
                            content.parse::<Token![,]>()?;
                        }
                    }
                }
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!(
                            "unknown route attribute `{}`. Expected: get/post/put/patch/delete, tag, desc, resps, auth, rate, wrap",
                            other
                        ),
                    ));
                }
            }

            // consume optional trailing comma
            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        let method = method.ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "#[route] requires an HTTP method (get, post, put, patch, delete)",
            )
        })?;

        let path = path.ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "#[route] requires a path string",
            )
        })?;

        Ok(RouteAttr { method, path, tag, desc, resps, auth, rate, wrap })
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Code generation
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/// Translate `:id` style path params to `{id}` for ntex.
fn translate_path(path: &str) -> String {
    let mut result = String::with_capacity(path.len());
    let mut chars = path.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == ':' {
            result.push('{');
            while let Some(&next) = chars.peek() {
                if next == '/' || next == '.' || next == '-' {
                    break;
                }
                result.push(chars.next().unwrap());
            }
            result.push('}');
        } else {
            result.push(ch);
        }
    }

    result
}

pub fn expand_route(attr: TokenStream, item: TokenStream) -> TokenStream {
    let route_attr = syn::parse_macro_input!(attr as RouteAttr);
    let handler_fn = syn::parse_macro_input!(item as ItemFn);

    let fn_name = &handler_fn.sig.ident;
    let fn_vis = &handler_fn.vis;
    let fn_attrs = &handler_fn.attrs;
    let fn_sig = &handler_fn.sig;
    let fn_block = &handler_fn.block;

    // Translate :param → {param} for ntex
    let ntex_path = translate_path(&route_attr.path);
    let original_path = &route_attr.path;
    let method_ident = route_attr.method.as_ident();
    let method_str = route_attr.method.as_str();

    // Optional metadata
    let tag_expr = match &route_attr.tag {
        Some(t) => quote! { ::core::option::Option::Some(#t) },
        None => quote! { ::core::option::Option::None },
    };
    let desc_expr = match &route_attr.desc {
        Some(d) => quote! { ::core::option::Option::Some(#d) },
        None => quote! { ::core::option::Option::None },
    };

    // Response specs — serialize as static array of (u16, &str, Option<&str>)
    let resp_count = route_attr.resps.len();
    let resp_entries: Vec<_> = route_attr.resps.iter().map(|r| {
        let status = r.status;
        let desc = &r.description;
        let ct = match &r.content_type {
            Some(ct) => quote! { ::core::option::Option::Some(#ct) },
            None => quote! { ::core::option::Option::None },
        };
        let schema_fn = match &r.schema_type {
            Some(type_path) => {
                // If it's something like Json<User>, extract the inner generic type.
                // Or if it's just `User`, use it directly. 
                // We'll trust the user to provide a type that implements `ToSchema`.
                // For simplicity, we just use the type_path directly. If it fails, compiler error.
                // Wait! If they wrote `Json<User>`, `Json<User>` maybe doesn't implement ToSchema.
                // We will try extracting the generic if the last segment has arguments.
                let mut inner_type = quote!{ #type_path };
                if let Some(segment) = type_path.path.segments.last() {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            inner_type = quote!{ #inner };
                        }
                    }
                }

                quote! {
                    ::core::option::Option::Some(|__vec| {
                        <#inner_type as ::floz::utoipa::ToSchema>::schemas(__vec);
                        let __name = <#inner_type as ::floz::utoipa::ToSchema>::name().into_owned();
                        let __schema = <#inner_type as ::floz::utoipa::__dev::ComposeSchema>::compose(::std::vec![]);
                        (__name, __schema)
                    })
                }
            },
            None => quote! { ::core::option::Option::None },
        };

        quote! {
            ::floz::router::ResponseMeta {
                status: #status,
                description: #desc,
                content_type: #ct,
                schema_fn: #schema_fn,
            }
        }
    }).collect();

    // Generate a unique static name for this route's registrar and response array
    let register_fn_name = syn::Ident::new(
        &format!("__floz_register_{}", fn_name),
        fn_name.span(),
    );
    let resps_static_name = syn::Ident::new(
        &format!("__FLOZ_RESPS_{}", fn_name.to_string().to_uppercase()),
        fn_name.span(),
    );

    // Auth and rate metadata
    let auth_expr = match &route_attr.auth {
        Some(a) => quote! { ::core::option::Option::Some(#a) },
        None => quote! { ::core::option::Option::None },
    };
    let rate_expr = match &route_attr.rate {
        Some(r) => quote! { ::core::option::Option::Some(#r) },
        None => quote! { ::core::option::Option::None },
    };

    let wrap_calls = route_attr.wrap.iter().map(|w| {
        quote! { .wrap(#w) }
    });

    let expanded = quote! {
        // The pure handler function without any ntex attribute macros overriding it
        #(#fn_attrs)*
        #fn_vis #fn_sig #fn_block

        // Static response metadata array
        #[allow(non_upper_case_globals)]
        static #resps_static_name: [::floz::router::ResponseMeta; #resp_count] = [
            #(#resp_entries),*
        ];

        // Auto-register this route natively via inventory, giving us complete
        // control to inject middleware `.wrap()` calls.
        fn #register_fn_name(cfg: &mut ::floz::ntex::web::ServiceConfig) {
            let route = ::floz::ntex::web::resource(#ntex_path)
                #(#wrap_calls)*
                .route(::floz::ntex::web::#method_ident().to(#fn_name));
            
            cfg.service(route);
        }

        ::floz::inventory::submit! {
            ::floz::router::RouteEntry::new(
                #method_str,
                #original_path,
                #tag_expr,
                #desc_expr,
                #register_fn_name,
                &#resps_static_name,
                #auth_expr,
                #rate_expr,
            )
        }
    };

    expanded.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translate_path_simple() {
        assert_eq!(translate_path("/users"), "/users");
    }

    #[test]
    fn test_translate_path_single_param() {
        assert_eq!(translate_path("/users/:id"), "/users/{id}");
    }

    #[test]
    fn test_translate_path_multiple_params() {
        assert_eq!(
            translate_path("/posts/:post_id/comments/:comment_id"),
            "/posts/{post_id}/comments/{comment_id}"
        );
    }

    #[test]
    fn test_translate_path_no_params() {
        assert_eq!(translate_path("/health"), "/health");
    }

    #[test]
    fn test_parse_response_spec() {
        let ts: proc_macro2::TokenStream = quote::quote! { (200, "Success", "application/json") };
        let spec: ResponseSpec = syn::parse2(ts).unwrap();
        assert_eq!(spec.status, 200);
        assert_eq!(spec.description, "Success");
        assert_eq!(spec.content_type.unwrap(), "application/json");
        assert!(spec.schema_type.is_none());
        
        let ts2: proc_macro2::TokenStream = quote::quote! { (404, "Not Found") };
        let spec2: ResponseSpec = syn::parse2(ts2).unwrap();
        assert_eq!(spec2.status, 404);
        assert_eq!(spec2.description, "Not Found");
        assert!(spec2.content_type.is_none());
        assert!(spec2.schema_type.is_none());
        
        let ts3: proc_macro2::TokenStream = quote::quote! { (201, "Created", Json<User>) };
        let spec3: ResponseSpec = syn::parse2(ts3).unwrap();
        assert_eq!(spec3.status, 201);
        assert_eq!(spec3.description, "Created");
        assert!(spec3.content_type.is_none());
        assert!(spec3.schema_type.is_some());
    }

    #[test]
    fn test_parse_route_attr() {
        let ts: proc_macro2::TokenStream = quote::quote! {
            get: "/users/:id",
            tag: "Users",
            desc: "Get user",
            resps: [
                (200, "found")
            ],
            auth: jwt,
            rate: "10/m"
        };
        let route: RouteAttr = syn::parse2(ts).unwrap();
        assert!(matches!(route.method, HttpMethod::Get));
        assert_eq!(route.path, "/users/:id");
        assert_eq!(route.tag.unwrap(), "Users");
        assert_eq!(route.desc.unwrap(), "Get user");
        assert_eq!(route.resps.len(), 1);
        assert_eq!(route.auth.unwrap(), "jwt");
        assert_eq!(route.rate.unwrap(), "10/m");
    }
}
