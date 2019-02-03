//! AST types for the parsed `schema!` DSL.
//!
//! These types represent the internal structure that the parser produces
//! and the code generator consumes.

use proc_macro2::Ident;

/// The entire schema input — one or more model declarations.
#[derive(Debug)]
pub struct SchemaInput {
    pub models: Vec<ModelDef>,
}

/// A single model declaration.
///
/// ```ignore
/// model User("users") {
///     id: integer("id").auto_increment().primary(),
///     ...
///     @primary_key(id),
/// }
/// ```
#[derive(Debug)]
pub struct ModelDef {
    /// The Rust struct name (e.g., `User`)
    pub name: Ident,
    /// The PostgreSQL table name (e.g., `"users"`)
    pub table_name: String,
    /// Database columns — generate struct fields + Column constants
    pub db_columns: Vec<FieldDef>,
    /// Relationships — only generate fetch_/add_/remove_ methods
    pub relationships: Vec<RelDef>,
    /// Table-level constraints (@primary_key, @unique, @index)
    pub constraints: Vec<TableConstraint>,
    /// If true, user provides custom `impl FlozHooks` (don't generate default)
    pub has_custom_hooks: bool,
}

/// A database column field definition.
///
/// ```ignore
/// name: varchar("name", 100).nullable().unique(),
/// ```
#[derive(Debug)]
pub struct FieldDef {
    /// The Rust field name (e.g., `name`)
    pub rust_name: Ident,
    /// The database column name (e.g., `"name"`)
    pub column_name: String,
    /// The column type
    pub type_info: TypeInfo,
    /// Chained modifiers
    pub modifiers: Vec<Modifier>,
}

/// A relationship declaration.
///
/// ```ignore
/// posts: array(Post, "author_id"),
/// ```
#[derive(Debug)]
pub struct RelDef {
    /// The Rust field name used for the relationship accessor
    pub rust_name: Ident,
    /// The target model (e.g., `Post`)
    pub target_model: Ident,
    /// The foreign key column name in the target table
    pub fk_column: String,
}

/// Column type information from the type function.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    // Numeric
    Integer,
    Short,
    BigInt,
    Real,
    Double,
    Decimal { precision: u32, scale: u32 },

    // String
    Varchar { max_length: u32 },
    Text,

    // Other scalars
    Bool,
    Date,
    Time,
    DateTime,
    Uuid,
    Binary,

    // JSON types
    Json,
    Jsonb,

    // Enum type
    Enum { rust_type: String },

    // Postgres Extension Types
    Ltree,

    // Generic escape hatch: col(Type, "column")
    Col { rust_type: String },

    // Native PostgreSQL array types
    TextArray,
    IntArray,
    BigIntArray,
    UuidArray,
    BoolArray,
    RealArray,
    DoubleArray,
    ShortArray,
    VarcharArray,
}

/// A modifier chained onto a field type.
#[derive(Debug, Clone, PartialEq)]
pub enum Modifier {
    /// `.primary()` — PRIMARY KEY
    Primary,
    /// `.auto_increment()` — SERIAL / BIGSERIAL
    AutoIncrement,
    /// `.nullable()` — wraps Rust type in Option<T>
    Nullable,
    /// `.unique()` — UNIQUE constraint
    Unique,
    /// `.default("expression")` — DEFAULT <expr>
    Default(String),
    /// `.now()` — DEFAULT now()
    Now,
    /// `.tz()` — WITH TIME ZONE
    Tz,
}

/// Table-level constraints.
#[derive(Debug, Clone, PartialEq)]
pub enum TableConstraint {
    /// `@primary_key(col1, col2)` — composite primary key
    PrimaryKey(Vec<String>),
    /// `@unique(col1, col2)` — composite unique constraint
    Unique(Vec<String>),
    /// `@index(col1, col2)` — index
    Index(Vec<String>),
}

// ── Helper methods ──

impl FieldDef {
    /// Check if this field has the `.primary()` modifier.
    pub fn is_primary(&self) -> bool {
        self.modifiers.iter().any(|m| matches!(m, Modifier::Primary))
    }

    /// Check if this field has the `.nullable()` modifier.
    pub fn is_nullable(&self) -> bool {
        self.modifiers.iter().any(|m| matches!(m, Modifier::Nullable))
    }

    /// Check if this field has the `.auto_increment()` modifier.
    pub fn is_auto_increment(&self) -> bool {
        self.modifiers.iter().any(|m| matches!(m, Modifier::AutoIncrement))
    }

    /// Check if this field has the `.unique()` modifier.
    pub fn is_unique(&self) -> bool {
        self.modifiers.iter().any(|m| matches!(m, Modifier::Unique))
    }

    /// Check if this field has the `.tz()` modifier.
    pub fn is_tz(&self) -> bool {
        self.modifiers.iter().any(|m| matches!(m, Modifier::Tz))
    }
}

impl ModelDef {
    /// Find primary key column(s). Checks field modifiers first,
    /// then falls back to `@primary_key(...)` constraint.
    pub fn primary_key_columns(&self) -> Vec<&FieldDef> {
        // First: check for @primary_key constraint
        for constraint in &self.constraints {
            if let TableConstraint::PrimaryKey(cols) = constraint {
                return self.db_columns.iter()
                    .filter(|f| cols.contains(&f.rust_name.to_string()))
                    .collect();
            }
        }

        // Fallback: fields with .primary() modifier
        self.db_columns.iter()
            .filter(|f| f.is_primary())
            .collect()
    }

    /// Returns true if this model has no primary key.
    pub fn has_primary_key(&self) -> bool {
        !self.primary_key_columns().is_empty()
    }

    /// Total number of database columns (for the 64-column limit check).
    pub fn column_count(&self) -> usize {
        self.db_columns.len()
    }
}

impl TypeInfo {
    /// The Rust type string for this column type.
    pub fn rust_type(&self, nullable: bool) -> String {
        let base = match self {
            TypeInfo::Integer => "i32".to_string(),
            TypeInfo::Short => "i16".to_string(),
            TypeInfo::BigInt => "i64".to_string(),
            TypeInfo::Real => "f32".to_string(),
            TypeInfo::Double => "f64".to_string(),
            TypeInfo::Decimal { .. } => "sqlx::types::BigDecimal".to_string(),
            TypeInfo::Varchar { .. } | TypeInfo::Text => "String".to_string(),
            TypeInfo::Bool => "bool".to_string(),
            TypeInfo::Date => "chrono::NaiveDate".to_string(),
            TypeInfo::Time => "chrono::NaiveTime".to_string(),
            TypeInfo::DateTime => "chrono::NaiveDateTime".to_string(), // .tz() changes this
            TypeInfo::Uuid => "uuid::Uuid".to_string(),
            TypeInfo::Binary => "Vec<u8>".to_string(),
            TypeInfo::Col { rust_type } => rust_type.clone(),
            // Native PG arrays
            TypeInfo::TextArray | TypeInfo::VarcharArray => "Vec<String>".to_string(),
            TypeInfo::IntArray => "Vec<i32>".to_string(),
            TypeInfo::ShortArray => "Vec<i16>".to_string(),
            TypeInfo::BigIntArray => "Vec<i64>".to_string(),
            TypeInfo::UuidArray => "Vec<uuid::Uuid>".to_string(),
            TypeInfo::BoolArray => "Vec<bool>".to_string(),
            TypeInfo::RealArray => "Vec<f32>".to_string(),
            TypeInfo::DoubleArray => "Vec<f64>".to_string(),
            TypeInfo::Json | TypeInfo::Jsonb => "serde_json::Value".to_string(),
            TypeInfo::Enum { rust_type } => rust_type.clone(),
            TypeInfo::Ltree => "String".to_string(),
        };

        if nullable {
            format!("Option<{}>", base)
        } else {
            base
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::Span;

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::call_site())
    }

    #[test]
    fn test_field_def_modifiers() {
        let mut field = FieldDef {
            rust_name: make_ident("id"),
            column_name: "id".to_string(),
            type_info: TypeInfo::Integer,
            modifiers: vec![],
        };
        
        assert!(!field.is_primary());
        assert!(!field.is_auto_increment());
        assert!(!field.is_nullable());
        assert!(!field.is_unique());
        assert!(!field.is_tz());

        field.modifiers.push(Modifier::Primary);
        field.modifiers.push(Modifier::AutoIncrement);
        field.modifiers.push(Modifier::Nullable);
        field.modifiers.push(Modifier::Unique);
        field.modifiers.push(Modifier::Tz);

        assert!(field.is_primary());
        assert!(field.is_auto_increment());
        assert!(field.is_nullable());
        assert!(field.is_unique());
        assert!(field.is_tz());
    }

    #[test]
    fn test_model_def_primary_key() {
        let field_id = FieldDef {
            rust_name: make_ident("id"),
            column_name: "id".to_string(),
            type_info: TypeInfo::Integer,
            modifiers: vec![Modifier::Primary],
        };
        
        let field_name = FieldDef {
            rust_name: make_ident("name"),
            column_name: "name".to_string(),
            type_info: TypeInfo::Text,
            modifiers: vec![],
        };

        let model = ModelDef {
            name: make_ident("User"),
            table_name: "users".to_string(),
            db_columns: vec![field_id, field_name],
            relationships: vec![],
            constraints: vec![],
            has_custom_hooks: false,
        };

        assert!(model.has_primary_key());
        let pk_cols = model.primary_key_columns();
        assert_eq!(pk_cols.len(), 1);
        assert_eq!(pk_cols[0].rust_name.to_string(), "id");
    }

    #[test]
    fn test_model_def_composite_primary_key() {
        let field_c1 = FieldDef {
            rust_name: make_ident("c1"),
            column_name: "c1".to_string(),
            type_info: TypeInfo::Integer,
            modifiers: vec![],
        };
        
        let field_c2 = FieldDef {
            rust_name: make_ident("c2"),
            column_name: "c2".to_string(),
            type_info: TypeInfo::Integer,
            modifiers: vec![],
        };

        let model = ModelDef {
            name: make_ident("Composite"),
            table_name: "composites".to_string(),
            db_columns: vec![field_c1, field_c2],
            relationships: vec![],
            constraints: vec![TableConstraint::PrimaryKey(vec!["c1".to_string(), "c2".to_string()])],
            has_custom_hooks: false,
        };

        assert!(model.has_primary_key());
        let pk_cols = model.primary_key_columns();
        assert_eq!(pk_cols.len(), 2);
        assert_eq!(pk_cols[0].rust_name.to_string(), "c1");
        assert_eq!(pk_cols[1].rust_name.to_string(), "c2");
    }
}
