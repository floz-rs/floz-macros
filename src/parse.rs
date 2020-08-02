//! Parser for the `schema!` DSL.
//!
//! Parses the token stream from `schema! { ... }` into a `SchemaInput` AST.
//! Uses `syn` for token parsing.

use proc_macro2::Ident;
use syn::parse::{Parse, ParseStream};
use syn::{braced, parenthesized, LitInt, LitStr, Token};

use crate::ast::*;

/// Maximum columns per model (u64 bitmask limit for dirty tracking).
const MAX_COLUMNS: usize = 64;

// ═══════════════════════════════════════════════════════════════
// SchemaInput
// ═══════════════════════════════════════════════════════════════

impl Parse for SchemaInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut models = Vec::new();

        while !input.is_empty() {
            models.push(input.parse::<ModelDef>()?);
        }

        if models.is_empty() {
            return Err(input.error("schema! requires at least one model definition"));
        }

        Ok(SchemaInput { models })
    }
}

// ═══════════════════════════════════════════════════════════════
// ModelDef
// ═══════════════════════════════════════════════════════════════

impl Parse for ModelDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse optional #[hooks] attribute
        let mut has_custom_hooks = false;
        if input.peek(Token![#]) {
            let attrs = syn::Attribute::parse_outer(input)?;
            for attr in attrs {
                if attr.path().is_ident("hooks") {
                    has_custom_hooks = true;
                } else {
                    return Err(syn::Error::new_spanned(attr, "unknown attribute on model. Did you mean #[hooks]?"));
                }
            }
        }

        // `model`
        let kw: Ident = input.parse()?;
        if kw != "model" {
            return Err(syn::Error::new(kw.span(), "expected `model`"));
        }

        // Model name (e.g., `User`)
        let name: Ident = input.parse()?;

        // Table name in parens: `("users")`
        let table_content;
        parenthesized!(table_content in input);
        let table_lit: LitStr = table_content.parse()?;
        let table_name = table_lit.value();

        // Fields in braces: `{ ... }`
        let body_content;
        braced!(body_content in input);

        let mut db_columns = Vec::new();
        let mut relationships = Vec::new();
        let mut constraints = Vec::new();

        while !body_content.is_empty() {
            // Check for table constraints: @primary_key(...), @unique(...), @index(...)
            if body_content.peek(Token![@]) {
                let constraint = parse_constraint(&body_content)?;
                constraints.push(constraint);
                // Optional trailing comma
                let _ = body_content.parse::<Token![,]>();
                continue;
            }

            // Parse field: `name: type_fn("col", ...args).modifier()...`
            let field = parse_field(&body_content)?;

            match field {
                ParsedField::Column(f) => db_columns.push(f),
                ParsedField::Relationship(r) => relationships.push(r),
            }

            // Optional trailing comma
            let _ = body_content.parse::<Token![,]>();
        }

        // Validate: max 64 columns
        if db_columns.len() > MAX_COLUMNS {
            return Err(syn::Error::new(
                name.span(),
                format!(
                    "Model `{}` has {} columns, but the maximum is {} (u64 bitmask limit). \
                     Consider normalizing your schema.",
                    name, db_columns.len(), MAX_COLUMNS
                ),
            ));
        }

        Ok(ModelDef {
            name,
            table_name,
            db_columns,
            relationships,
            constraints,
            has_custom_hooks,
        })
    }
}

// ═══════════════════════════════════════════════════════════════
// Field Parsing
// ═══════════════════════════════════════════════════════════════

/// A parsed field — either a column or a relationship.
enum ParsedField {
    Column(FieldDef),
    Relationship(RelDef),
}

/// Parse a single field: `name: type_fn("col", ...args).modifier()...`
fn parse_field(input: ParseStream) -> syn::Result<ParsedField> {
    // Rust field name
    let rust_name: Ident = input.parse()?;

    // Colon
    input.parse::<Token![:]>()?;

    // Type function name (e.g., `integer`, `varchar`, `array`)
    let type_fn: Ident = input.parse()?;

    // Type function arguments in parens
    let args_content;
    parenthesized!(args_content in input);

    // Parse based on type function name
    let type_fn_str = type_fn.to_string();

    // Check if this is a relationship
    if type_fn_str == "array" {
        let rel = parse_relationship(rust_name, &args_content)?;
        // Consume any trailing modifiers (shouldn't have any, but be lenient)
        consume_modifiers(input)?;
        return Ok(ParsedField::Relationship(rel));
    }

    // Parse as a column
    let (column_name, type_info) = parse_type_fn(&type_fn_str, &type_fn, &args_content)?;

    // Parse modifier chain: .primary().nullable().auto_increment()...
    let modifiers = parse_modifiers(input)?;

    Ok(ParsedField::Column(FieldDef {
        rust_name,
        column_name,
        type_info,
        modifiers,
    }))
}

/// Parse the type function arguments and return (column_name, TypeInfo).
fn parse_type_fn(
    type_fn_str: &str,
    type_fn_ident: &Ident,
    args: ParseStream,
) -> syn::Result<(String, TypeInfo)> {
    match type_fn_str {
        // Simple types: type("column_name")
        "integer" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Integer))
        }
        "short" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Short))
        }
        "bigint" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::BigInt))
        }
        "real" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Real))
        }
        "double" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Double))
        }
        "text" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Text))
        }
        "bool" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Bool))
        }
        "date" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Date))
        }
        "time" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Time))
        }
        "datetime" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::DateTime))
        }
        "uuid" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Uuid))
        }
        "binary" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Binary))
        }
        "json" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Json))
        }
        "jsonb" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Jsonb))
        }
        "ltree" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Ltree))
        }

        // enumeration("column", RustType)
        "enumeration" => {
            let col: LitStr = args.parse()?;
            args.parse::<Token![,]>()?;
            let rust_type: syn::Ident = args.parse()?;
            Ok((col.value(), TypeInfo::Enum { rust_type: rust_type.to_string() }))
        }

        // varchar("column", max_length)
        "varchar" => {
            let col: LitStr = args.parse()?;
            args.parse::<Token![,]>()?;
            let len: LitInt = args.parse()?;
            let max_length: u32 = len.base10_parse()?;
            Ok((col.value(), TypeInfo::Varchar { max_length }))
        }

        // decimal("column", precision, scale)
        "decimal" => {
            let col: LitStr = args.parse()?;
            args.parse::<Token![,]>()?;
            let p: LitInt = args.parse()?;
            args.parse::<Token![,]>()?;
            let s: LitInt = args.parse()?;
            Ok((
                col.value(),
                TypeInfo::Decimal {
                    precision: p.base10_parse()?,
                    scale: s.base10_parse()?,
                },
            ))
        }

        // col(RustType, "column") — generic escape hatch
        "col" => {
            let rust_type: Ident = args.parse()?;
            args.parse::<Token![,]>()?;
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::Col { rust_type: rust_type.to_string() }))
        }

        // Native PG array types
        "text_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::TextArray))
        }
        "int_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::IntArray))
        }
        "bigint_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::BigIntArray))
        }
        "uuid_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::UuidArray))
        }
        "bool_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::BoolArray))
        }
        "real_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::RealArray))
        }
        "double_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::DoubleArray))
        }
        "short_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::ShortArray))
        }
        "varchar_array" => {
            let col: LitStr = args.parse()?;
            Ok((col.value(), TypeInfo::VarcharArray))
        }

        _ => Err(syn::Error::new(
            type_fn_ident.span(),
            format!(
                "unknown type function `{}`\n\
                 available types: integer, short, bigint, real, double, decimal, \
                 varchar, text, bool, date, time, datetime, uuid, binary, col, \
                 text_array, int_array, bigint_array, uuid_array, bool_array, \
                 real_array, double_array, short_array, varchar_array",
                type_fn_str
            ),
        )),
    }
}

/// Parse a relationship: `array(TargetModel, "fk_column")`
fn parse_relationship(rust_name: Ident, args: ParseStream) -> syn::Result<RelDef> {
    let target_model: Ident = args.parse()?;
    args.parse::<Token![,]>()?;
    let fk_col: LitStr = args.parse()?;

    Ok(RelDef {
        rust_name,
        target_model,
        fk_column: fk_col.value(),
    })
}

// ═══════════════════════════════════════════════════════════════
// Modifier Parsing
// ═══════════════════════════════════════════════════════════════

/// Parse a chain of modifiers: `.primary().nullable().auto_increment()`
fn parse_modifiers(input: ParseStream) -> syn::Result<Vec<Modifier>> {
    let mut modifiers = Vec::new();

    while input.peek(Token![.]) {
        input.parse::<Token![.]>()?;
        let modifier_name: Ident = input.parse()?;

        let modifier = match modifier_name.to_string().as_str() {
            "primary" => {
                parse_empty_parens(input)?;
                Modifier::Primary
            }
            "auto_increment" => {
                parse_empty_parens(input)?;
                Modifier::AutoIncrement
            }
            "nullable" => {
                parse_empty_parens(input)?;
                Modifier::Nullable
            }
            "unique" => {
                parse_empty_parens(input)?;
                Modifier::Unique
            }
            "now" => {
                parse_empty_parens(input)?;
                Modifier::Now
            }
            "tz" => {
                parse_empty_parens(input)?;
                Modifier::Tz
            }
            "default" => {
                let content;
                parenthesized!(content in input);
                let val: LitStr = content.parse()?;
                Modifier::Default(val.value())
            }
            other => {
                return Err(syn::Error::new(
                    modifier_name.span(),
                    format!(
                        "unknown modifier `.{}`\n\
                         available modifiers: .primary(), .auto_increment(), .nullable(), \
                         .unique(), .default(\"...\"), .now(), .tz()",
                        other
                    ),
                ));
            }
        };

        modifiers.push(modifier);
    }

    Ok(modifiers)
}

/// Consume modifiers without storing them (for relationships).
fn consume_modifiers(input: ParseStream) -> syn::Result<()> {
    while input.peek(Token![.]) {
        input.parse::<Token![.]>()?;
        let _name: Ident = input.parse()?;
        if input.peek(syn::token::Paren) {
            let _content;
            parenthesized!(_content in input);
        }
    }
    Ok(())
}

/// Parse empty parentheses: `()`
fn parse_empty_parens(input: ParseStream) -> syn::Result<()> {
    let _content;
    parenthesized!(_content in input);
    Ok(())
}

// ═══════════════════════════════════════════════════════════════
// Constraint Parsing
// ═══════════════════════════════════════════════════════════════

/// Parse a table constraint: `@primary_key(col1, col2)`
fn parse_constraint(input: ParseStream) -> syn::Result<TableConstraint> {
    input.parse::<Token![@]>()?;
    let constraint_name: Ident = input.parse()?;

    let content;
    parenthesized!(content in input);

    let mut columns = Vec::new();
    while !content.is_empty() {
        let col: Ident = content.parse()?;
        columns.push(col.to_string());
        if content.peek(Token![,]) {
            content.parse::<Token![,]>()?;
        }
    }

    match constraint_name.to_string().as_str() {
        "primary_key" => Ok(TableConstraint::PrimaryKey(columns)),
        "unique" => Ok(TableConstraint::Unique(columns)),
        "index" => Ok(TableConstraint::Index(columns)),
        other => Err(syn::Error::new(
            constraint_name.span(),
            format!(
                "unknown constraint `@{}`\n\
                 available constraints: @primary_key(), @unique(), @index()",
                other
            ),
        )),
    }
}

// ═══════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> SchemaInput {
        syn::parse_str::<SchemaInput>(input).unwrap()
    }

    fn try_parse(input: &str) -> syn::Result<SchemaInput> {
        syn::parse_str::<SchemaInput>(input)
    }

    // ── Basic model parsing ──

    #[test]
    fn parse_simple_model() {
        let schema = parse(r#"
            model User("users") {
                id: integer("id").auto_increment().primary(),
            }
        "#);
        assert_eq!(schema.models.len(), 1);
        assert_eq!(schema.models[0].name.to_string(), "User");
        assert_eq!(schema.models[0].table_name, "users");
        assert_eq!(schema.models[0].db_columns.len(), 1);
    }

    #[test]
    fn parse_model_name_and_table() {
        let schema = parse(r#"
            model AppUser("app_users") {
                id: integer("user_id"),
            }
        "#);
        assert_eq!(schema.models[0].name.to_string(), "AppUser");
        assert_eq!(schema.models[0].table_name, "app_users");
        assert_eq!(schema.models[0].db_columns[0].column_name, "user_id");
    }

    // ── Field types ──

    #[test]
    fn parse_all_basic_types() {
        let schema = parse(r#"
            model Test("test") {
                a: integer("a"),
                b: short("b"),
                c: bigint("c"),
                d: real("d"),
                e: double("e"),
                f: text("f"),
                g: bool("g"),
                h: date("h"),
                i: time("i"),
                j: datetime("j"),
                k: uuid("k"),
                l: binary("l"),
            }
        "#);
        assert_eq!(schema.models[0].db_columns.len(), 12);
        assert_eq!(schema.models[0].db_columns[0].type_info, TypeInfo::Integer);
        assert_eq!(schema.models[0].db_columns[1].type_info, TypeInfo::Short);
        assert_eq!(schema.models[0].db_columns[2].type_info, TypeInfo::BigInt);
        assert_eq!(schema.models[0].db_columns[3].type_info, TypeInfo::Real);
        assert_eq!(schema.models[0].db_columns[4].type_info, TypeInfo::Double);
        assert_eq!(schema.models[0].db_columns[5].type_info, TypeInfo::Text);
        assert_eq!(schema.models[0].db_columns[6].type_info, TypeInfo::Bool);
        assert_eq!(schema.models[0].db_columns[7].type_info, TypeInfo::Date);
        assert_eq!(schema.models[0].db_columns[8].type_info, TypeInfo::Time);
        assert_eq!(schema.models[0].db_columns[9].type_info, TypeInfo::DateTime);
        assert_eq!(schema.models[0].db_columns[10].type_info, TypeInfo::Uuid);
        assert_eq!(schema.models[0].db_columns[11].type_info, TypeInfo::Binary);
    }

    #[test]
    fn parse_varchar_with_length() {
        let schema = parse(r#"
            model T("t") { name: varchar("name", 100) }
        "#);
        assert_eq!(
            schema.models[0].db_columns[0].type_info,
            TypeInfo::Varchar { max_length: 100 }
        );
    }

    #[test]
    fn parse_decimal_with_precision() {
        let schema = parse(r#"
            model T("t") { rating: decimal("rating", 5, 2) }
        "#);
        assert_eq!(
            schema.models[0].db_columns[0].type_info,
            TypeInfo::Decimal { precision: 5, scale: 2 }
        );
    }

    #[test]
    fn parse_col_generic() {
        let schema = parse(r#"
            model T("t") { data: col(Value, "data") }
        "#);
        assert_eq!(
            schema.models[0].db_columns[0].type_info,
            TypeInfo::Col { rust_type: "Value".to_string() }
        );
    }

    #[test]
    fn parse_array_types() {
        let schema = parse(r#"
            model T("t") {
                a: text_array("a"),
                b: int_array("b"),
                c: uuid_array("c"),
            }
        "#);
        assert_eq!(schema.models[0].db_columns[0].type_info, TypeInfo::TextArray);
        assert_eq!(schema.models[0].db_columns[1].type_info, TypeInfo::IntArray);
        assert_eq!(schema.models[0].db_columns[2].type_info, TypeInfo::UuidArray);
    }

    // ── Modifiers ──

    #[test]
    fn parse_modifiers() {
        let schema = parse(r#"
            model T("t") {
                id: integer("id").auto_increment().primary(),
                name: varchar("name", 50).nullable().unique(),
                created: datetime("created").now().tz(),
            }
        "#);
        let id = &schema.models[0].db_columns[0];
        assert!(id.is_auto_increment());
        assert!(id.is_primary());

        let name = &schema.models[0].db_columns[1];
        assert!(name.is_nullable());
        assert!(name.is_unique());

        let created = &schema.models[0].db_columns[2];
        assert!(created.modifiers.contains(&Modifier::Now));
        assert!(created.is_tz());
    }

    #[test]
    fn parse_default_modifier() {
        let schema = parse(r#"
            model T("t") { status: varchar("status", 20).default("active") }
        "#);
        let field = &schema.models[0].db_columns[0];
        assert!(field.modifiers.contains(&Modifier::Default("active".to_string())));
    }

    // ── Relationships ──

    #[test]
    fn parse_relationship() {
        let schema = parse(r#"
            model User("users") {
                id: integer("id"),
                posts: array(Post, "author_id"),
            }
        "#);
        assert_eq!(schema.models[0].db_columns.len(), 1); // only id
        assert_eq!(schema.models[0].relationships.len(), 1);
        assert_eq!(schema.models[0].relationships[0].rust_name.to_string(), "posts");
        assert_eq!(schema.models[0].relationships[0].target_model.to_string(), "Post");
        assert_eq!(schema.models[0].relationships[0].fk_column, "author_id");
    }

    #[test]
    fn parse_relationship_excluded_from_columns() {
        let schema = parse(r#"
            model Post("posts") {
                id: integer("id"),
                title: varchar("title", 255),
                authors: array(User, "author_id"),
            }
        "#);
        // Only id and title are db_columns — authors is a relationship
        assert_eq!(schema.models[0].db_columns.len(), 2);
        assert_eq!(schema.models[0].relationships.len(), 1);
    }

    // ── Constraints ──

    #[test]
    fn parse_primary_key_constraint() {
        let schema = parse(r#"
            model PostTag("post_tags") {
                post_id: integer("post_id"),
                tag_id: integer("tag_id"),
                @primary_key(post_id, tag_id),
            }
        "#);
        assert_eq!(schema.models[0].constraints.len(), 1);
        assert_eq!(
            schema.models[0].constraints[0],
            TableConstraint::PrimaryKey(vec!["post_id".into(), "tag_id".into()])
        );
    }

    #[test]
    fn parse_unique_constraint() {
        let schema = parse(r#"
            model T("t") {
                a: integer("a"),
                b: integer("b"),
                @unique(a, b),
            }
        "#);
        assert_eq!(
            schema.models[0].constraints[0],
            TableConstraint::Unique(vec!["a".into(), "b".into()])
        );
    }

    #[test]
    fn parse_index_constraint() {
        let schema = parse(r#"
            model T("t") {
                name: varchar("name", 100),
                @index(name),
            }
        "#);
        assert_eq!(
            schema.models[0].constraints[0],
            TableConstraint::Index(vec!["name".into()])
        );
    }

    // ── Multiple models ──

    #[test]
    fn parse_multiple_models() {
        let schema = parse(r#"
            model User("users") {
                id: integer("id").primary(),
                name: varchar("name", 100),
            }
            model Post("posts") {
                id: integer("id").primary(),
                title: text("title"),
            }
        "#);
        assert_eq!(schema.models.len(), 2);
        assert_eq!(schema.models[0].name.to_string(), "User");
        assert_eq!(schema.models[1].name.to_string(), "Post");
    }

    // ── Validation ──

    #[test]
    fn parse_unknown_type_error() {
        let result = try_parse(r#"
            model T("t") { id: intgr("id") }
        "#);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("unknown type function `intgr`"));
        assert!(err.contains("available types"));
    }

    #[test]
    fn parse_unknown_modifier_error() {
        let result = try_parse(r#"
            model T("t") { id: integer("id").primry() }
        "#);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("unknown modifier `.primry`"));
    }

    #[test]
    fn parse_unknown_constraint_error() {
        let result = try_parse(r#"
            model T("t") {
                id: integer("id"),
                @foreign_key(id),
            }
        "#);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("unknown constraint `@foreign_key`"));
    }

    #[test]
    fn parse_empty_schema_error() {
        let result = try_parse("");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("at least one model"));
    }

    // ── Helper methods ──

    #[test]
    fn primary_key_from_modifier() {
        let schema = parse(r#"
            model T("t") {
                id: integer("id").primary(),
                name: text("name"),
            }
        "#);
        let pks = schema.models[0].primary_key_columns();
        assert_eq!(pks.len(), 1);
        assert_eq!(pks[0].rust_name.to_string(), "id");
    }

    #[test]
    fn primary_key_from_constraint() {
        let schema = parse(r#"
            model T("t") {
                a: integer("a"),
                b: integer("b"),
                @primary_key(a, b),
            }
        "#);
        let pks = schema.models[0].primary_key_columns();
        assert_eq!(pks.len(), 2);
    }

    #[test]
    fn no_primary_key() {
        let schema = parse(r#"
            model AuditLog("audit_logs") {
                event: text("event"),
                timestamp: datetime("timestamp"),
            }
        "#);
        assert!(!schema.models[0].has_primary_key());
    }

    #[test]
    fn rust_type_resolution() {
        assert_eq!(TypeInfo::Integer.rust_type(false), "i32");
        assert_eq!(TypeInfo::Integer.rust_type(true), "Option<i32>");
        assert_eq!(TypeInfo::Varchar { max_length: 50 }.rust_type(false), "String");
        assert_eq!(TypeInfo::Text.rust_type(true), "Option<String>");
        assert_eq!(TypeInfo::DateTime.rust_type(false), "chrono::NaiveDateTime");
        assert_eq!(TypeInfo::TextArray.rust_type(false), "Vec<String>");
        assert_eq!(TypeInfo::IntArray.rust_type(true), "Option<Vec<i32>>");
    }

    // ── Column count ──

    #[test]
    fn column_count() {
        let schema = parse(r#"
            model T("t") {
                a: integer("a"),
                b: text("b"),
                c: bool("c"),
                rel: array(Other, "fk"),
            }
        "#);
        assert_eq!(schema.models[0].column_count(), 3); // rel not counted
    }

    // ── Rust field name vs DB column name ──

    #[test]
    fn field_and_column_names_differ() {
        let schema = parse(r#"
            model T("t") {
                bio: text("bio_db_column"),
            }
        "#);
        assert_eq!(schema.models[0].db_columns[0].rust_name.to_string(), "bio");
        assert_eq!(schema.models[0].db_columns[0].column_name, "bio_db_column");
    }
}
