//! Code generation — transforms the parsed AST into Rust code.
//!
//! For each model, generates:
//! 1. A DAO struct with derives (Debug, Clone, Serialize, Deserialize, FromRow)
//! 2. A Table namespace struct with typed Column constants
//! 3. A Default impl for testing/mocking

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::ast::*;

/// Generate all code for a schema.
pub fn generate(schema: &SchemaInput) -> TokenStream {
    let models: Vec<TokenStream> = schema
        .models
        .iter()
        .map(generate_model)
        .collect();

    quote! { #(#models)* }
}

/// Generate all code for a single model.
fn generate_model(model: &ModelDef) -> TokenStream {
    let struct_tokens = generate_struct(model);
    let table_tokens = generate_table(model);
    let default_tokens = generate_default(model);
    let setters_tokens = generate_setters(model);
    let dao_tokens = generate_dao(model);
    let ddl_tokens = generate_ddl(model);
    let hooks_tokens = generate_hooks(model);
    let rel_methods_tokens = generate_rel_methods(model);

    quote! {
        #struct_tokens
        #table_tokens
        #default_tokens
        #setters_tokens
        #dao_tokens
        #ddl_tokens
        #hooks_tokens
        #rel_methods_tokens
    }
}

// ═══════════════════════════════════════════════════════════════
// Struct Generation
// ═══════════════════════════════════════════════════════════════

fn generate_struct(model: &ModelDef) -> TokenStream {
    let name = &model.name;

    let fields: Vec<TokenStream> = model
        .db_columns
        .iter()
        .map(|field| {
            let rust_name = &field.rust_name;
            let rust_type = type_tokens(&field.type_info, field.is_nullable(), field.is_tz());
            let col_name = &field.column_name;

            // Add #[sqlx(rename = "...")] if rust field name != db column name
            let rename_attr = if rust_name.to_string() != *col_name {
                quote! { #[sqlx(rename = #col_name)] }
            } else {
                quote! {}
            };

            quote! {
                #rename_attr
                pub #rust_name: #rust_type,
            }
        })
        .collect();

    let rel_fields: Vec<TokenStream> = model
        .relationships
        .iter()
        .map(|rel| {
            let rel_name = format_ident!("_rel_{}", rel.rust_name);
            let target_type = &rel.target_model;
            quote! {
                /// Eager-loaded relationship items.
                #[serde(skip)]
                #[sqlx(skip)]
                pub #rel_name: Vec<#target_type>,
            }
        })
        .collect();

    quote! {
        #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, sqlx::FromRow)]
        pub struct #name {
            #(#fields)*

            /// Dirty tracking bitmask — each bit corresponds to a column.
            /// Skipped by sqlx (not a DB column) and serde (internal state).
            #[serde(skip)]
            #[sqlx(skip)]
            pub _dirty_flags: u64,

            #(#rel_fields)*
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Table Namespace Generation
// ═══════════════════════════════════════════════════════════════

fn generate_table(model: &ModelDef) -> TokenStream {
    let table_struct = format_ident!("{}Table", model.name);
    let table_name_str = &model.table_name;

    let column_consts: Vec<TokenStream> = model
        .db_columns
        .iter()
        .map(|field| {
            let const_name = &field.rust_name;
            let col_name = &field.column_name;
            let rust_type = type_tokens(&field.type_info, field.is_nullable(), field.is_tz());

            quote! {
                #[allow(non_upper_case_globals)]
                pub const #const_name: floz::Column<#rust_type> =
                    floz::Column::new(#col_name, #table_name_str);
            }
        })
        .collect();

    // Collect all column names as a const array (useful for SELECT generation)
    let col_names: Vec<&String> = model.db_columns.iter().map(|f| &f.column_name).collect();
    let col_count = model.db_columns.len();

    quote! {
        pub struct #table_struct;

        #[allow(non_upper_case_globals)]
        impl #table_struct {
            /// The PostgreSQL table name.
            pub const TABLE_NAME: &'static str = #table_name_str;

            /// All column names in declaration order.
            pub const COLUMNS: [&'static str; #col_count] = [#(#col_names),*];

            #(#column_consts)*
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Default Generation
// ═══════════════════════════════════════════════════════════════

fn generate_default(model: &ModelDef) -> TokenStream {
    let name = &model.name;

    let default_fields: Vec<TokenStream> = model
        .db_columns
        .iter()
        .map(|field| {
            let rust_name = &field.rust_name;
            let default_val = default_value_tokens(&field.type_info, field.is_nullable(), field.is_tz());
            quote! { #rust_name: #default_val, }
        })
        .collect();

    let default_rels: Vec<TokenStream> = model
        .relationships
        .iter()
        .map(|rel| {
            let rel_name = format_ident!("_rel_{}", rel.rust_name);
            quote! { #rel_name: Vec::new(), }
        })
        .collect();

    quote! {
        impl Default for #name {
            fn default() -> Self {
                Self {
                    #(#default_fields)*
                    _dirty_flags: 0,
                    #(#default_rels)*
                }
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Setter Generation (dirty-tracking)
// ═══════════════════════════════════════════════════════════════

fn generate_setters(model: &ModelDef) -> TokenStream {
    let name = &model.name;

    let setters: Vec<TokenStream> = model
        .db_columns
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            let setter_name = format_ident!("set_{}", field.rust_name);
            let field_name = &field.rust_name;
            let rust_type = type_tokens(&field.type_info, field.is_nullable(), field.is_tz());
            let bit: u64 = 1u64 << idx;

            quote! {
                /// Set the value and mark the column as dirty.
                pub fn #setter_name(&mut self, val: #rust_type) {
                    self.#field_name = val;
                    self._dirty_flags |= #bit;
                }
            }
        })
        .collect();

    // is_dirty() helper
    let dirty_check = quote! {
        /// Returns true if any field has been modified.
        pub fn is_dirty(&self) -> bool {
            self._dirty_flags != 0
        }

        /// Returns true if a specific column (by bit index) is dirty.
        pub fn is_field_dirty(&self, bit: usize) -> bool {
            self._dirty_flags & (1u64 << bit) != 0
        }

        /// Reset dirty flags (called after save).
        pub fn clear_dirty(&mut self) {
            self._dirty_flags = 0;
        }
    };

    quote! {
        impl #name {
            #(#setters)*
            #dirty_check
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Type Mapping
// ═══════════════════════════════════════════════════════════════

/// Convert a TypeInfo to a Rust type TokenStream.
fn type_tokens(type_info: &TypeInfo, nullable: bool, tz: bool) -> TokenStream {
    let base = match type_info {
        TypeInfo::Integer => quote! { i32 },
        TypeInfo::Short => quote! { i16 },
        TypeInfo::BigInt => quote! { i64 },
        TypeInfo::Real => quote! { f32 },
        TypeInfo::Double => quote! { f64 },
        TypeInfo::Decimal { .. } => quote! { sqlx::types::BigDecimal },
        TypeInfo::Varchar { .. } | TypeInfo::Text => quote! { String },
        TypeInfo::Bool => quote! { bool },
        TypeInfo::Date => quote! { chrono::NaiveDate },
        TypeInfo::Time => quote! { chrono::NaiveTime },
        TypeInfo::DateTime => {
            if tz {
                quote! { chrono::DateTime<chrono::Utc> }
            } else {
                quote! { chrono::NaiveDateTime }
            }
        }
        TypeInfo::Uuid => quote! { uuid::Uuid },
        TypeInfo::Binary => quote! { Vec<u8> },
        TypeInfo::Col { rust_type } => {
            let ty = format_ident!("{}", rust_type);
            quote! { #ty }
        }
        TypeInfo::Json | TypeInfo::Jsonb => quote! { serde_json::Value },
        TypeInfo::Ltree => quote! { String },
        TypeInfo::Enum { rust_type } => {
            let ty = format_ident!("{}", rust_type);
            quote! { #ty }
        }
        // Native PG arrays
        TypeInfo::TextArray | TypeInfo::VarcharArray => quote! { Vec<String> },
        TypeInfo::IntArray => quote! { Vec<i32> },
        TypeInfo::ShortArray => quote! { Vec<i16> },
        TypeInfo::BigIntArray => quote! { Vec<i64> },
        TypeInfo::UuidArray => quote! { Vec<uuid::Uuid> },
        TypeInfo::BoolArray => quote! { Vec<bool> },
        TypeInfo::RealArray => quote! { Vec<f32> },
        TypeInfo::DoubleArray => quote! { Vec<f64> },
    };

    if nullable {
        quote! { Option<#base> }
    } else {
        base
    }
}

/// Generate a default value TokenStream for a given type.
fn default_value_tokens(type_info: &TypeInfo, nullable: bool, tz: bool) -> TokenStream {
    if nullable {
        return quote! { None };
    }

    match type_info {
        TypeInfo::Integer => quote! { 0i32 },
        TypeInfo::Short => quote! { 0i16 },
        TypeInfo::BigInt => quote! { 0i64 },
        TypeInfo::Real => quote! { 0.0f32 },
        TypeInfo::Double => quote! { 0.0f64 },
        TypeInfo::Decimal { .. } => quote! { Default::default() },
        TypeInfo::Varchar { .. } | TypeInfo::Text => quote! { String::new() },
        TypeInfo::Bool => quote! { false },
        TypeInfo::Date => {
            quote! { chrono::NaiveDate::from_ymd_opt(1970, 1, 1).unwrap() }
        }
        TypeInfo::Time => {
            quote! { chrono::NaiveTime::from_hms_opt(0, 0, 0).unwrap() }
        }
        TypeInfo::DateTime => {
            if tz {
                quote! {
                    chrono::DateTime::from_timestamp(0, 0).unwrap()
                }
            } else {
                quote! {
                    chrono::NaiveDate::from_ymd_opt(1970, 1, 1)
                        .unwrap()
                        .and_hms_opt(0, 0, 0)
                        .unwrap()
                }
            }
        }
        TypeInfo::Uuid => quote! { uuid::Uuid::nil() },
        TypeInfo::Binary => quote! { Vec::new() },
        TypeInfo::Col { .. } | TypeInfo::Enum { .. } => quote! { Default::default() },
        TypeInfo::Json | TypeInfo::Jsonb => quote! { serde_json::Value::Null },
        TypeInfo::Ltree => quote! { String::new() },
        // Arrays
        TypeInfo::TextArray | TypeInfo::VarcharArray | TypeInfo::IntArray
        | TypeInfo::ShortArray | TypeInfo::BigIntArray | TypeInfo::UuidArray
        | TypeInfo::BoolArray | TypeInfo::RealArray | TypeInfo::DoubleArray => {
            quote! { Vec::new() }
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Hooks and Relationships Generation
// ═══════════════════════════════════════════════════════════════

fn generate_hooks(model: &ModelDef) -> TokenStream {
    if model.has_custom_hooks {
        quote! {} // User provides their own `impl floz::FlozHooks for #name {}`
    } else {
        let name = &model.name;
        quote! {
            impl floz::FlozHooks for #name {}
        }
    }
}

fn generate_rel_methods(model: &ModelDef) -> TokenStream {
    let name = &model.name;

    let rel_methods: Vec<TokenStream> = model
        .relationships
        .iter()
        .map(|rel| {
            let fetch_fn = generate_fetch_method(model, rel);
            let preload_fn = generate_preload_method(model, rel);

            quote! {
                #fetch_fn
                #preload_fn
            }
        })
        .collect();

    if rel_methods.is_empty() {
        quote! {}
    } else {
        quote! {
            impl #name {
                #(#rel_methods)*
            }
        }
    }
}

/// Generates the `fetch_{relation}()` lazy-load method.
fn generate_fetch_method(model: &ModelDef, rel: &RelDef) -> TokenStream {
    let rel_name = &rel.rust_name;
    let target = &rel.target_model;
    let target_table = format_ident!("{}Table", target);
    let fk_col = format_ident!("{}", rel.fk_column);
    let fetch_name = format_ident!("fetch_{}", rel_name);

    // Default to 'id' if no primary key is explicitly found.
    // In actual usage, models involved in relations almost always have a primary key.
    let pk_struct_field = model
        .primary_key_columns()
        .first()
        .map(|f| &f.rust_name)
        .cloned()
        .unwrap_or_else(|| format_ident!("id"));

    quote! {
        /// Lazy-fetch related entities. Executes one query per call.
        pub async fn #fetch_name(
            &self,
            db: &impl floz::Executor
        ) -> Result<Vec<#target>, floz::FlozError> {
            #target::filter(#target_table::#fk_col.eq(self.#pk_struct_field.clone()), db).await
        }
    }
}

/// Generates the `preload_{relation}()` batch-load method.
fn generate_preload_method(model: &ModelDef, rel: &RelDef) -> TokenStream {
    let rel_name = &rel.rust_name;
    let target = &rel.target_model;
    let target_table = format_ident!("{}Table", target);
    let fk_col = format_ident!("{}", rel.fk_column);
    let preload_name = format_ident!("preload_{}", rel_name);
    let rel_field = format_ident!("_rel_{}", rel_name);

    // Preload requires a primary key column to extract IDs.
    let Some(pk) = model.primary_key_columns().first().cloned() else {
        return quote! {};
    };

    let pk_rust_name = &pk.rust_name;

    quote! {
        /// Batch-preload related entities to avoid N+1 queries.
        pub async fn #preload_name(
            entities: &mut [Self],
            db: &impl floz::Executor
        ) -> Result<(), floz::FlozError> {
            if entities.is_empty() { 
                return Ok(()); 
            }
            
            // Extract all IDs from the parent slice
            let ids: Vec<_> = entities.iter().map(|e| e.#pk_rust_name.clone()).collect();
            
            // Fetch all related entities in one batch query
            let all_related = #target::filter(#target_table::#fk_col.in_list(ids), db).await?;

            // Group related entities by the foreign key to map them back correctly.
            // (Uses cloned() to distribute into each parent's `Vec`).
            for entity in entities.iter_mut() {
                let entity_id = &entity.#pk_rust_name;
                entity.#rel_field = all_related
                    .iter()
                    .filter(|related| &related.#fk_col == entity_id)
                    .cloned()
                    .collect();
            }

            Ok(())
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// DDL Method Generation
// ═══════════════════════════════════════════════════════════════

fn build_create_table_sql(model: &ModelDef) -> String {
    let mut sql = format!("CREATE TABLE IF NOT EXISTS {} (\n", model.table_name);
    let mut columns = Vec::new();

    for field in &model.db_columns {
        let mut col_def = format!("    {}", field.column_name);

        let pg_type = match &field.type_info {
            TypeInfo::Integer if field.is_auto_increment() => "SERIAL".to_string(),
            TypeInfo::BigInt if field.is_auto_increment() => "BIGSERIAL".to_string(),
            TypeInfo::Short if field.is_auto_increment() => "SMALLSERIAL".to_string(),
            TypeInfo::Integer => "INT".to_string(),
            TypeInfo::Short => "SMALLINT".to_string(),
            TypeInfo::BigInt => "BIGINT".to_string(),
            TypeInfo::Real => "REAL".to_string(),
            TypeInfo::Double => "DOUBLE PRECISION".to_string(),
            TypeInfo::Decimal { precision, scale } => format!("NUMERIC({}, {})", precision, scale),
            TypeInfo::Varchar { max_length } => format!("VARCHAR({})", max_length),
            TypeInfo::Text => "TEXT".to_string(),
            TypeInfo::Bool => "BOOLEAN".to_string(),
            TypeInfo::Date => "DATE".to_string(),
            TypeInfo::Time => "TIME".to_string(),
            TypeInfo::DateTime => if field.is_tz() { "TIMESTAMPTZ".to_string() } else { "TIMESTAMP".to_string() },
            TypeInfo::Uuid => "UUID".to_string(),
            TypeInfo::Binary => "BYTEA".to_string(),
            TypeInfo::Json | TypeInfo::Jsonb => "JSONB".to_string(),
            TypeInfo::Ltree => "LTREE".to_string(),
            TypeInfo::Enum { .. } => "VARCHAR".to_string(),
            TypeInfo::Col { .. } => "TEXT".to_string(),
            TypeInfo::TextArray => "TEXT[]".to_string(),
            TypeInfo::VarcharArray => "VARCHAR[]".to_string(),
            TypeInfo::IntArray => "INT[]".to_string(),
            TypeInfo::ShortArray => "SMALLINT[]".to_string(),
            TypeInfo::BigIntArray => "BIGINT[]".to_string(),
            TypeInfo::UuidArray => "UUID[]".to_string(),
            TypeInfo::BoolArray => "BOOLEAN[]".to_string(),
            TypeInfo::RealArray => "REAL[]".to_string(),
            TypeInfo::DoubleArray => "DOUBLE PRECISION[]".to_string(),
        };

        col_def.push_str(&format!(" {}", pg_type));

        if !field.is_nullable() && !field.is_auto_increment() {
            col_def.push_str(" NOT NULL");
        }

        for modifier in &field.modifiers {
            match modifier {
                Modifier::Primary => col_def.push_str(" PRIMARY KEY"),
                Modifier::Unique => col_def.push_str(" UNIQUE"),
                Modifier::Default(val) => {
                    col_def.push_str(&format!(" DEFAULT {}", val))
                },
                Modifier::Now => col_def.push_str(" DEFAULT CURRENT_TIMESTAMP"),
                _ => {}
            }
        }

        columns.push(col_def);
    }

    for constraint in &model.constraints {
        match constraint {
            TableConstraint::PrimaryKey(cols) => {
                columns.push(format!("    PRIMARY KEY ({})", cols.join(", ")));
            }
            TableConstraint::Unique(cols) => {
                columns.push(format!("    UNIQUE ({})", cols.join(", ")));
            }
            _ => {}
        }
    }

    sql.push_str(&columns.join(",\n"));
    sql.push_str("\n)");
    
    sql
}

fn generate_ddl(model: &ModelDef) -> TokenStream {
    let name = &model.name;
    let create_sql = build_create_table_sql(model);
    let table = &model.table_name;
    let drop_sql = format!("DROP TABLE IF EXISTS {} CASCADE", table);
    let empty_sql = format!("TRUNCATE TABLE {} CASCADE", table);

    quote! {
        impl #name {
            /// Create the table in the database if it does not exist.
            pub async fn create_table(db: &impl floz::Executor) -> Result<(), floz::FlozError> {
                db.execute_raw(#create_sql, vec![]).await?;
                Ok(())
            }

            /// Drop the table from the database if it exists (cascade).
            pub async fn drop_table(db: &impl floz::Executor) -> Result<(), floz::FlozError> {
                db.execute_raw(#drop_sql, vec![]).await?;
                Ok(())
            }

            /// Truncate the table, deleting all rows (cascade).
            pub async fn empty(db: &impl floz::Executor) -> Result<(), floz::FlozError> {
                db.execute_raw(#empty_sql, vec![]).await?;
                Ok(())
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// DAO Method Generation
// ═══════════════════════════════════════════════════════════════

/// Quote a table name for generated SQL strings.
fn quote_table_str(name: &str) -> String {
    if name.contains('.') {
        name.split('.')
            .map(|p| format!("\"{}\"", p))
            .collect::<Vec<_>>()
            .join(".")
    } else {
        name.to_string()
    }
}

/// Generate all DAO methods for a model.
fn generate_dao(model: &ModelDef) -> TokenStream {
    let name = &model.name;

    // Methods available for all models
    let create_fn = generate_create(model);
    let all_fn = generate_all(model);
    let filter_fn = generate_filter(model);
    let paginate_fn = generate_paginate(model);

    // Methods only available for models with a primary key
    let pk_methods = if model.has_primary_key() {
        let pk_cols = model.primary_key_columns();
        let get_fn = generate_get(model, &pk_cols);
        let find_fn = generate_find(model, &pk_cols);
        let save_fn = generate_save(model, &pk_cols);
        let delete_fn = generate_delete(model, &pk_cols);
        quote! {
            #get_fn
            #find_fn
            #save_fn
            #delete_fn
        }
    } else {
        quote! {}
    };

    quote! {
        impl #name {
            #create_fn
            #all_fn
            #filter_fn
            #paginate_fn
            #pk_methods
        }
    }
}

fn generate_paginate(model: &ModelDef) -> TokenStream {
    let table = &model.table_name;
    quote! {
        /// Create a paginated query builder for this model.
        pub fn paginate() -> floz::PaginateQuery<Self> {
            floz::PaginateQuery::new(#table)
        }
    }
}

/// Generate `create()` — INSERT non-auto-increment fields, RETURNING *.
fn generate_create(model: &ModelDef) -> TokenStream {
    let table = quote_table_str(&model.table_name);

    // Columns to insert: all except auto_increment
    let insert_fields: Vec<&FieldDef> = model
        .db_columns
        .iter()
        .filter(|f| !f.is_auto_increment())
        .collect();

    // Build SQL: INSERT INTO table (col1, col2) VALUES ($1, $2) RETURNING *
    let col_list = insert_fields
        .iter()
        .map(|f| f.column_name.as_str())
        .collect::<Vec<_>>()
        .join(", ");

    let param_list = (1..=insert_fields.len())
        .map(|i| format!("${}", i))
        .collect::<Vec<_>>()
        .join(", ");

    let sql = format!(
        "INSERT INTO {} ({}) VALUES ({}) RETURNING *",
        table, col_list, param_list
    );

    // Build params vec
    let param_exprs: Vec<TokenStream> = insert_fields
        .iter()
        .map(|f| to_value_expr(f, quote! { self }))
        .collect();

    quote! {
        /// Insert this entity into the database, returning the full row
        /// (with auto-generated columns like `id`).
        pub async fn create(&self, db: &impl floz::Executor) -> Result<Self, floz::FlozError> {
            floz::FlozHooks::before_create(self)?;
            let params: Vec<floz::Value> = vec![#(#param_exprs),*];
            let result = db.fetch_one(#sql, params).await?;
            floz::FlozHooks::after_create(&result);
            Ok(result)
        }
    }
}

/// Generate `all()` — SELECT * FROM table.
fn generate_all(model: &ModelDef) -> TokenStream {
    let table = quote_table_str(&model.table_name);
    let sql = format!("SELECT * FROM {}", table);

    quote! {
        /// Fetch all rows from the table.
        pub async fn all(db: &impl floz::Executor) -> Result<Vec<Self>, floz::FlozError> {
            db.fetch_all(#sql, vec![]).await
        }
    }
}

/// Generate `filter()` — SELECT * FROM table WHERE expr.
fn generate_filter(model: &ModelDef) -> TokenStream {
    let table = quote_table_str(&model.table_name);
    let prefix = format!("SELECT * FROM {} WHERE ", table);

    quote! {
        /// Fetch all rows matching the given filter expression.
        pub async fn filter(
            expr: floz::Expr,
            db: &impl floz::Executor,
        ) -> Result<Vec<Self>, floz::FlozError> {
            let mut sql = String::from(#prefix);
            let mut params = Vec::new();
            let mut idx = 0usize;
            expr.to_sql(&mut sql, &mut params, &mut idx);
            db.fetch_all(&sql, params).await
        }
    }
}

/// Generate `get()` — SELECT * FROM table WHERE pk = $1. Returns error if not found.
fn generate_get(model: &ModelDef, pk_cols: &[&FieldDef]) -> TokenStream {
    let table = quote_table_str(&model.table_name);

    let (fn_params, where_clause, param_exprs) = pk_query_parts(pk_cols);
    let sql = format!("SELECT * FROM {} WHERE {}", table, where_clause);

    quote! {
        /// Fetch a single row by primary key. Returns `FlozError::NotFound` if missing.
        pub async fn get(#(#fn_params,)* db: &impl floz::Executor) -> Result<Self, floz::FlozError> {
            let params: Vec<floz::Value> = vec![#(#param_exprs),*];
            db.fetch_one(#sql, params).await
        }
    }
}

/// Generate `find()` — like get() but returns Option<Self>.
fn generate_find(model: &ModelDef, pk_cols: &[&FieldDef]) -> TokenStream {
    let table = quote_table_str(&model.table_name);

    let (fn_params, where_clause, param_exprs) = pk_query_parts(pk_cols);
    let sql = format!("SELECT * FROM {} WHERE {}", table, where_clause);

    quote! {
        /// Fetch a single row by primary key, returning `None` if not found.
        pub async fn find(#(#fn_params,)* db: &impl floz::Executor) -> Result<Option<Self>, floz::FlozError> {
            let params: Vec<floz::Value> = vec![#(#param_exprs),*];
            db.fetch_optional(#sql, params).await
        }
    }
}

/// Generate `save()` — UPDATE only dirty fields WHERE pk.
fn generate_save(model: &ModelDef, pk_cols: &[&FieldDef]) -> TokenStream {
    let table = quote_table_str(&model.table_name);

    // Generate a conditional SET block for each column
    let set_blocks: Vec<TokenStream> = model
        .db_columns
        .iter()
        .enumerate()
        .map(|(idx, field)| {
            let bit: u64 = 1u64 << idx;
            let col_name = &field.column_name;
            let value_expr = to_value_expr(field, quote! { self });

            quote! {
                if self._dirty_flags & #bit != 0 {
                    if !first { sql.push_str(", "); }
                    param_idx += 1;
                    sql.push_str(&format!("{} = ${}", #col_name, param_idx));
                    params.push(#value_expr);
                    first = false;
                }
            }
        })
        .collect();

    // WHERE clause for PK
    let pk_where_parts: Vec<TokenStream> = pk_cols
        .iter()
        .enumerate()
        .map(|(i, pk)| {
            let col_name = &pk.column_name;
            let value_expr = to_value_expr(pk, quote! { self });
            if i == 0 {
                quote! {
                    param_idx += 1;
                    sql.push_str(&format!(" WHERE {} = ${}", #col_name, param_idx));
                    params.push(#value_expr);
                }
            } else {
                quote! {
                    param_idx += 1;
                    sql.push_str(&format!(" AND {} = ${}", #col_name, param_idx));
                    params.push(#value_expr);
                }
            }
        })
        .collect();

    let table_str = format!("UPDATE {} SET ", table);

    quote! {
        /// Update only the dirty (modified) fields in the database.
        /// Does nothing if no fields have been modified.
        pub async fn save(&mut self, db: &impl floz::Executor) -> Result<(), floz::FlozError> {
            floz::FlozHooks::before_save(self)?;

            if !self.is_dirty() {
                floz::FlozHooks::after_save(self);
                return Ok(());
            }

            let mut sql = String::from(#table_str);
            let mut params: Vec<floz::Value> = Vec::new();
            let mut param_idx = 0usize;
            let mut first = true;

            #(#set_blocks)*

            // If nothing was actually dirty (shouldn't happen due to check above)
            if first {
                floz::FlozHooks::after_save(self);
                return Ok(());
            }

            // Add WHERE pk = $N
            #(#pk_where_parts)*

            db.execute_raw(&sql, params).await?;
            self.clear_dirty();
            
            floz::FlozHooks::after_save(self);
            Ok(())
        }
    }
}

/// Generate `delete()` — DELETE FROM table WHERE pk.
fn generate_delete(model: &ModelDef, pk_cols: &[&FieldDef]) -> TokenStream {
    let table = quote_table_str(&model.table_name);

    let (_, where_clause, _) = pk_query_parts(pk_cols);
    let sql = format!("DELETE FROM {} WHERE {}", table, where_clause);

    let param_exprs: Vec<TokenStream> = pk_cols
        .iter()
        .map(|f| to_value_expr(f, quote! { self }))
        .collect();

    quote! {
        /// Delete this entity from the database by primary key.
        pub async fn delete(&self, db: &impl floz::Executor) -> Result<(), floz::FlozError> {
            floz::FlozHooks::before_delete(self)?;
            let params: Vec<floz::Value> = vec![#(#param_exprs),*];
            db.execute_raw(#sql, params).await?;
            floz::FlozHooks::after_delete(self);
            Ok(())
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Helpers
// ═══════════════════════════════════════════════════════════════

/// Generate function parameters, WHERE clause, and param expressions for PK queries.
/// Returns (fn_params, where_sql, param_exprs).
fn pk_query_parts(pk_cols: &[&FieldDef]) -> (Vec<TokenStream>, String, Vec<TokenStream>) {
    let mut fn_params = Vec::new();
    let mut where_parts = Vec::new();
    let mut param_exprs = Vec::new();

    for (i, pk) in pk_cols.iter().enumerate() {
        let param_name = &pk.rust_name;
        let param_type = type_tokens(&pk.type_info, pk.is_nullable(), pk.is_tz());
        let col_name = &pk.column_name;
        let idx = i + 1;

        fn_params.push(quote! { #param_name: #param_type });
        where_parts.push(format!("{} = ${}", col_name, idx));
        param_exprs.push(to_value_expr_ident(pk, param_name));
    }

    let where_clause = where_parts.join(" AND ");
    (fn_params, where_clause, param_exprs)
}

/// Convert a field value on `self` to a `Value` expression.
fn to_value_expr(field: &FieldDef, receiver: TokenStream) -> TokenStream {
    let field_name = &field.rust_name;
    to_value_tokens(&field.type_info, field.is_nullable(), field.is_tz(), quote! { #receiver.#field_name })
}

/// Convert a field value from a named parameter to a `Value` expression.
fn to_value_expr_ident(field: &FieldDef, ident: &proc_macro2::Ident) -> TokenStream {
    to_value_tokens(&field.type_info, field.is_nullable(), field.is_tz(), quote! { #ident })
}

/// Map a Rust field expression to the appropriate `Value` variant.
fn to_value_tokens(type_info: &TypeInfo, nullable: bool, tz: bool, expr: TokenStream) -> TokenStream {
    if nullable {
        return match type_info {
            TypeInfo::Integer => quote! { floz::Value::OptionInt(#expr) },
            TypeInfo::Short => quote! { floz::Value::OptionShort(#expr) },
            TypeInfo::BigInt => quote! { floz::Value::OptionBigInt(#expr) },
            TypeInfo::Real => quote! { floz::Value::OptionReal(#expr) },
            TypeInfo::Double => quote! { floz::Value::OptionDouble(#expr) },
            TypeInfo::Bool => quote! { floz::Value::OptionBool(#expr) },
            TypeInfo::Varchar { .. } | TypeInfo::Text => quote! { floz::Value::OptionString(#expr.clone()) },
            TypeInfo::Uuid => quote! { floz::Value::OptionUuid(#expr) },
            TypeInfo::DateTime => {
                if tz {
                    quote! { floz::Value::OptionDateTime(#expr) }
                } else {
                    quote! { floz::Value::OptionNaiveDateTime(#expr) }
                }
            }
            TypeInfo::Date => quote! { floz::Value::OptionNaiveDate(#expr) },
            TypeInfo::Time => quote! { floz::Value::OptionNaiveTime(#expr) },
            TypeInfo::Binary => quote! { floz::Value::OptionBytes(#expr.clone()) },
            TypeInfo::Json => quote! { floz::Value::OptionJson(#expr.clone()) },
            TypeInfo::Jsonb => quote! { floz::Value::OptionJsonb(#expr.clone()) },
            TypeInfo::Ltree => quote! { floz::Value::OptionString(#expr.clone()) },
            TypeInfo::Enum { .. } => quote! { floz::Value::OptionString(#expr.clone().map(|v| v.to_string())) },
            _ => quote! { floz::Value::OptionString(#expr.clone().map(|v| format!("{:?}", v))) },
        };
    }

    match type_info {
        TypeInfo::Integer => quote! { floz::Value::Int(#expr) },
        TypeInfo::Short => quote! { floz::Value::Short(#expr) },
        TypeInfo::BigInt => quote! { floz::Value::BigInt(#expr) },
        TypeInfo::Real => quote! { floz::Value::Real(#expr) },
        TypeInfo::Double => quote! { floz::Value::Double(#expr) },
        TypeInfo::Bool => quote! { floz::Value::Bool(#expr) },
        TypeInfo::Varchar { .. } | TypeInfo::Text | TypeInfo::Ltree => quote! { floz::Value::String(#expr.clone()) },
        TypeInfo::Uuid => quote! { floz::Value::Uuid(#expr) },
        TypeInfo::DateTime => {
            if tz {
                quote! { floz::Value::DateTime(#expr) }
            } else {
                quote! { floz::Value::NaiveDateTime(#expr) }
            }
        }
        TypeInfo::Date => quote! { floz::Value::NaiveDate(#expr) },
        TypeInfo::Time => quote! { floz::Value::NaiveTime(#expr) },
        TypeInfo::Binary => quote! { floz::Value::Bytes(#expr.clone()) },
        TypeInfo::Json => quote! { floz::Value::Json(#expr.clone()) },
        TypeInfo::Jsonb => quote! { floz::Value::Jsonb(#expr.clone()) },
        TypeInfo::Enum { .. } => quote! { floz::Value::String(#expr.to_string()) },
        _ => quote! { floz::Value::String(format!("{:?}", #expr)) },
    }
}
