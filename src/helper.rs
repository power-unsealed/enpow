use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::HashSet;
use syn::{
    parenthesized, parse::Parse, parse::ParseStream, punctuated::Punctuated, spanned::Spanned,
    visit::Visit, Attribute, Data, DataEnum, DeriveInput, Error, Field, Fields, FieldsNamed,
    FieldsUnnamed, GenericParam, Generics, Ident, ItemStruct, Lifetime, LifetimeDef, Path, Token,
    TypeParam, TypePath, Variant, Visibility, WherePredicate,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodType {
    All,
    Variant,
    IsVariant,
    VariantAsRef,
    UnwrapVariant,
    ExpectVariant,
}

impl MethodType {
    pub fn from_attribute(attribute: TokenStream) -> Result<HashSet<MethodType>, Error> {
        let info: EnpowAttributeInfo = syn::parse2(attribute)?;
        Ok(info.methods)
    }

    pub fn needs_self_type(&self) -> bool {
        match self {
            MethodType::All => true,
            MethodType::Variant => true,
            MethodType::IsVariant => false,
            MethodType::VariantAsRef => false,
            MethodType::UnwrapVariant => true,
            MethodType::ExpectVariant => true,
        }
    }

    pub fn needs_ref_type(&self) -> bool {
        match self {
            MethodType::All => true,
            MethodType::Variant => false,
            MethodType::IsVariant => true,
            MethodType::VariantAsRef => true,
            MethodType::UnwrapVariant => false,
            MethodType::ExpectVariant => false,
        }
    }

    pub fn needs_mut_type(&self) -> bool {
        match self {
            MethodType::All => true,
            MethodType::Variant => false,
            MethodType::IsVariant => false,
            MethodType::VariantAsRef => true,
            MethodType::UnwrapVariant => false,
            MethodType::ExpectVariant => false,
        }
    }
}

pub struct EnpowAttributeInfo {
    methods: HashSet<MethodType>,
}

impl Parse for EnpowAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut items: Punctuated<_, Token![,]> = input.parse_terminated(Ident::parse)?;
        let mut methods = HashSet::new();

        // If there are no arguments, insert a fake "All"
        if items.is_empty() {
            items.push(Ident::new("All", Span::call_site()));
        }

        for item in items {
            match item.to_string().as_str() {
                "All" => {
                    methods.insert(MethodType::All);
                }
                "Var" => {
                    methods.insert(MethodType::Variant);
                }
                "IsVar" => {
                    methods.insert(MethodType::IsVariant);
                }
                "VarAsRef" => {
                    methods.insert(MethodType::VariantAsRef);
                }
                "UnwrapVar" => {
                    methods.insert(MethodType::UnwrapVariant);
                }
                "ExpectVar" => {
                    methods.insert(MethodType::ExpectVariant);
                }
                some => {
                    return Err(Error::new_spanned(
                        item,
                        format!("Unknown argument `{some}`"),
                    ));
                }
            }
        }

        Ok(EnpowAttributeInfo { methods })
    }
}

pub struct VarDeriveAttributeInfo {
    pub derives: Vec<Path>,
}

impl Parse for VarDeriveAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.is_empty() {
            Ok(VarDeriveAttributeInfo {
                derives: Vec::new(),
            })
        } else {
            // Parse the derive arguments in parenthesis
            let content;
            parenthesized!(content in input);
            let derives: Punctuated<_, Token![,]> = content.parse_terminated(Path::parse)?;

            Ok(VarDeriveAttributeInfo {
                derives: derives.into_iter().collect(),
            })
        }
    }
}

pub struct EnumInfo {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub generics: Generics,
    pub identifier: Ident,
    pub visibility: Visibility,
    pub data: DataEnum,
}

pub trait ExtractEnumInfo {
    fn extract_info(self) -> Result<EnumInfo, Error>;
}

impl ExtractEnumInfo for DeriveInput {
    fn extract_info(self) -> Result<EnumInfo, Error> {
        let span = self.span();

        let data = match self.data {
            Data::Enum(data) => data,
            _ => {
                return Err(Error::new(
                    Span::call_site(),
                    "Can only be applied to enums",
                ));
            }
        };

        Ok(EnumInfo {
            span,
            generics: self.generics,
            attributes: self.attrs,
            identifier: self.ident,
            visibility: self.vis,
            data,
        })
    }
}

pub enum VariantType {
    Unit,
    Field,
    Unnamed,
    Named,
}

pub struct VariantInfo {
    pub var_type: VariantType,
    pub identifier: Ident,
    pub snake_case: String,
    pub docs: Vec<Attribute>,
    /// Data type of variant data in self, ref, and mut version
    pub data_type: (TokenStream, TokenStream, TokenStream),
    /// Type definition of variant data in self, ref, and mut version
    pub type_def: (
        Option<TokenStream>,
        Option<TokenStream>,
        Option<TokenStream>,
    ),
    pub pattern: TokenStream,
    /// Construction of variant data in self, ref, and mut version
    pub construction: (TokenStream, TokenStream, TokenStream),
}

impl VariantInfo {
    pub fn new(
        var_type: VariantType,
        data_type: (TokenStream, TokenStream, TokenStream),
        type_def: (
            Option<TokenStream>,
            Option<TokenStream>,
            Option<TokenStream>,
        ),
        pattern: TokenStream,
        construction: (TokenStream, TokenStream, TokenStream),
        identifier: Ident,
        docs: Vec<Attribute>,
    ) -> VariantInfo {
        VariantInfo {
            var_type,
            snake_case: identifier.to_string().to_snake_case(),
            identifier,
            docs,
            data_type,
            type_def,
            pattern,
            construction,
        }
    }

    pub fn from_unit(identifier: Ident, docs: Vec<Attribute>, parent: &EnumInfo) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;

        Ok(VariantInfo::new(
            VariantType::Unit,
            (quote! { () }, quote! { () }, quote! { () }),
            (None, None, None),
            quote! { #enum_ident::#identifier },
            (quote! { () }, quote! { () }, quote! { () }),
            identifier,
            docs,
        ))
    }

    pub fn from_field(
        identifier: Ident,
        field: Field,
        docs: Vec<Attribute>,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;
        let single = &field.ty;

        Ok(VariantInfo::new(
            VariantType::Field,
            (
                quote! { #single },
                quote! { & #single },
                quote! { &mut #single },
            ),
            (None, None, None),
            quote! { #enum_ident::#identifier( f0 ) },
            (quote! { f0 }, quote! { f0 }, quote! { f0 }),
            identifier,
            docs,
        ))
    }

    pub fn from_unnamed(
        identifier: Ident,
        tuple: FieldsUnnamed,
        docs: Vec<Attribute>,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;
        let fields: Vec<_> = tuple
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, _)| format_ident!("f{i}"))
            .collect();
        let construction = quote! { ( #(#fields),* ) };

        let mut ref_tuple = tuple.clone();
        for field in ref_tuple.unnamed.iter_mut() {
            let ftype = &field.ty;
            field.ty = syn::parse2(quote! { & #ftype })?;
        }

        let mut mut_tuple = tuple.clone();
        for field in mut_tuple.unnamed.iter_mut() {
            let ftype = &field.ty;
            field.ty = syn::parse2(quote! { &mut #ftype })?;
        }

        Ok(VariantInfo::new(
            VariantType::Unnamed,
            (
                quote! { #tuple },
                quote! { #ref_tuple },
                quote! { #mut_tuple },
            ),
            (None, None, None),
            quote! { #enum_ident::#identifier #construction },
            (construction.clone(), construction.clone(), construction),
            identifier,
            docs,
        ))
    }

    pub fn from_named(
        identifier: Ident,
        fields: FieldsNamed,
        docs: Vec<Attribute>,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;
        let visibility = &parent.visibility;
        let (field_idents, field_types): (Vec<_>, Vec<_>) = fields
            .named
            .into_iter()
            .map(|field| (field.ident.unwrap(), field.ty))
            .unzip();
        let constructor = quote! { { #(#field_idents),* } };

        // Prepare self struct
        let type_ident = format_ident!("{enum_ident}{identifier}");
        let fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: #t })
            .collect();

        // Build the struct without generics definition to find out which generics are actually
        // used
        let without_gen = quote! {
            #visibility struct #type_ident { #(#fields),* }
        };
        let ast = syn::parse2::<ItemStruct>(without_gen)?;
        let mut generics = parent.generics.filter_unused(&ast)?;
        let (_, gen_short, gen_where) = generics.split_for_impl();
        let gen_short = gen_short.to_token_stream();
        let gen_where = gen_where.map(|w| w.to_token_stream());

        // Build self struct
        let self_type = quote! { #type_ident #gen_short };
        let self_def = quote! {
            #(#docs)* #visibility struct #type_ident #generics #gen_where { #(#fields),* }
        };
        let data_constr = quote! { #type_ident #constructor };

        // Build generics for ref and mut with additional lifetime for the refs
        let lifetime_name = format!("'{}", type_ident.to_string().to_snake_case());
        let lifetime = Lifetime::new(&lifetime_name, Span::call_site());
        let mut gen_params = generics.params.clone();
        gen_params.push(GenericParam::Lifetime(LifetimeDef::new(lifetime.clone())));
        generics.params = gen_params;
        // We do NOT regenerate the short and where clause, since we just added a lifetime. If we
        // leave it out, the compiler will infer it. If we take it in, we have to sprinkle it
        // everywhere...
        //let (_, gen_short, gen_where) = generics.split_for_impl();

        // Build ref struct
        let ref_ident = format_ident!("{type_ident}Ref");
        let ref_fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: &#lifetime #t })
            .collect();
        let ref_type = quote! { #ref_ident #gen_short };
        let ref_def = quote! {
            #(#docs)* #visibility struct #ref_ident #generics #gen_where { #(#ref_fields),* }
        };
        let ref_constr = quote! { #ref_ident #constructor };

        // Build mut struct
        let mut_ident = format_ident!("{type_ident}Mut");
        let mut_fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: &#lifetime mut #t })
            .collect();
        let mut_type = quote! { #mut_ident #gen_short };
        let mut_def = quote! {
            #(#docs)* #visibility struct #mut_ident #generics #gen_where { #(#mut_fields),* }
        };
        let mut_constr = quote! { #mut_ident #constructor };

        Ok(VariantInfo::new(
            VariantType::Named,
            (self_type, ref_type, mut_type),
            (Some(self_def), Some(ref_def), Some(mut_def)),
            quote! { #enum_ident::#identifier #constructor },
            (data_constr, ref_constr, mut_constr),
            identifier,
            docs,
        ))
    }

    pub fn build_method_types(&self, parent: &EnumInfo, types: &[MethodType]) -> Vec<TokenStream> {
        let mut methods = Vec::new();
        for t in types {
            match t {
                MethodType::All => {
                    methods.push(self.build_variant());
                    methods.push(self.build_is());
                    methods.push(self.build_is_and());
                    methods.push(self.build_as_ref());
                    methods.push(self.build_as_mut());
                    methods.push(self.build_unwrap(parent));
                    methods.push(self.build_unwrap_as_ref(parent));
                    methods.push(self.build_unwrap_as_mut(parent));
                    methods.push(self.build_unwrap_or());
                    methods.push(self.build_unwrap_or_else());
                    methods.push(self.build_expect());
                    methods.push(self.build_expect_as_ref());
                    methods.push(self.build_expect_as_mut());
                }
                MethodType::Variant => {
                    methods.push(self.build_variant());
                }
                MethodType::IsVariant => {
                    methods.push(self.build_is());
                    methods.push(self.build_is_and());
                }
                MethodType::VariantAsRef => {
                    methods.push(self.build_as_ref());
                    methods.push(self.build_as_mut());
                }
                MethodType::UnwrapVariant => {
                    methods.push(self.build_unwrap(parent));
                    methods.push(self.build_unwrap_as_ref(parent));
                    methods.push(self.build_unwrap_as_mut(parent));
                    methods.push(self.build_unwrap_or());
                    methods.push(self.build_unwrap_or_else());
                }
                MethodType::ExpectVariant => {
                    methods.push(self.build_expect());
                    methods.push(self.build_expect_as_ref());
                    methods.push(self.build_expect_as_mut());
                }
            }
        }
        methods
    }

    pub fn build_variant(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

        let fn_ident = Ident::new(&snake_case, Span::call_site());

        quote! {
            fn #fn_ident(self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_as_ref(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.1;
        let pattern = &self.pattern;
        let construction = &self.construction.1;

        let fn_ident = format_ident!("{snake_case}_as_ref");

        quote! {
            fn #fn_ident(&self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_as_mut(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.2;
        let pattern = &self.pattern;
        let construction = &self.construction.2;

        let fn_ident = format_ident!("{snake_case}_as_mut");

        quote! {
            fn #fn_ident(&mut self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_is(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let pattern = &self.pattern;

        let fn_ident = format_ident!("is_{snake_case}");

        quote! {
            fn #fn_ident(&self) -> bool {
                match self {
                    #pattern => true,
                    _ => false,
                }
            }
        }
    }

    pub fn build_is_and(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.1;
        let pattern = &self.pattern;
        let construction = &self.construction.1;

        let fn_ident = format_ident!("is_{snake_case}_and");

        quote! {
            fn #fn_ident(&self, f: impl FnOnce(#data_type) -> bool) -> bool {
                match self {
                    #pattern => f(#construction),
                    _ => false,
                }
            }
        }
    }

    pub fn build_unwrap(&self, parent: &EnumInfo) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

        let fn_ident = format_ident!("unwrap_{snake_case}");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            fn #fn_ident(self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_unwrap_as_ref(&self, parent: &EnumInfo) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.1;
        let pattern = &self.pattern;
        let construction = &self.construction.1;

        let fn_ident = format_ident!("unwrap_{snake_case}_as_ref");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            fn #fn_ident(&self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_unwrap_as_mut(&self, parent: &EnumInfo) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.2;
        let pattern = &self.pattern;
        let construction = &self.construction.2;

        let fn_ident = format_ident!("unwrap_{snake_case}_as_mut");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            fn #fn_ident(&mut self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_unwrap_or(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

        let fn_ident = format_ident!("unwrap_{snake_case}_or");

        quote! {
            fn #fn_ident(self, default: #data_type) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => default,
                }
            }
        }
    }

    pub fn build_unwrap_or_else(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

        let fn_ident = format_ident!("unwrap_{snake_case}_or_else");

        quote! {
            fn #fn_ident(self, f: impl FnOnce(Self) -> #data_type) -> #data_type {
                match self {
                    #pattern => #construction,
                    some => f(some),
                }
            }
        }
    }

    pub fn build_expect(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

        let fn_ident = format_ident!("expect_{snake_case}");

        quote! {
            fn #fn_ident(self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{}", msg),
                }
            }
        }
    }

    pub fn build_expect_as_ref(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.1;
        let pattern = &self.pattern;
        let construction = &self.construction.1;

        let fn_ident = format_ident!("expect_{snake_case}_as_ref");

        quote! {
            fn #fn_ident(&self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{}", msg),
                }
            }
        }
    }

    pub fn build_expect_as_mut(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.2;
        let pattern = &self.pattern;
        let construction = &self.construction.2;

        let fn_ident = format_ident!("expect_{snake_case}_as_mut");

        quote! {
            fn #fn_ident(&mut self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{}", msg),
                }
            }
        }
    }
}

pub trait ExtractVariantInfo {
    fn extract_info(self, parent: &EnumInfo) -> Result<VariantInfo, Error>;
}

impl ExtractVariantInfo for Variant {
    fn extract_info(self, parent: &EnumInfo) -> Result<VariantInfo, Error> {
        let identifier = self.ident;

        // Get all doc comments
        let docs: Vec<_> = self.attrs.into_iter()
            .filter(|attr| {
                match attr.path.get_ident() {
                    Some(ident) => ident.to_string() == "doc",
                    None => false,
                }
            })
            .collect();

        match self.fields {
            // Variant without data
            Fields::Unit => VariantInfo::from_unit(identifier, docs, parent),

            // Variant with unnamed fields
            Fields::Unnamed(tuple) => {
                if tuple.unnamed.len() == 1 {
                    let field = tuple.unnamed[0].clone();
                    VariantInfo::from_field(identifier, field, docs, parent)
                } else {
                    VariantInfo::from_unnamed(identifier, tuple, docs, parent)
                }
            }

            // Variant with named fields
            Fields::Named(fields) => VariantInfo::from_named(identifier, fields, docs, parent),
        }
    }
}

pub trait SnakeCase {
    fn to_snake_case(self) -> String;
}

impl<T: AsRef<str>> SnakeCase for T {
    fn to_snake_case(self) -> String {
        // We probably wont need much more than two underscores...
        let mut output = String::with_capacity(self.as_ref().len() + 2);

        // Turn the input into snake case char by char
        let mut last_uppercase = true;
        let mut last_underscore = false;
        for char in self.as_ref().chars() {
            if char.is_uppercase() {
                // If there is an uppercase char that is not preceeded by another
                // uppercase char or an underscore, add an underscore in front of it
                if !last_uppercase && !last_underscore {
                    output.push('_');
                }

                // Add the lower case version of it to the output
                for char in char.to_lowercase() {
                    output.push(char);
                }

                last_uppercase = true;
            } else {
                output.push(char);

                last_uppercase = false;
            }

            last_underscore = char == '_';
        }

        output
    }
}

pub struct UsageMonitor {
    pub lifetimes: HashSet<String>,
    pub type_paths: HashSet<String>,
}

impl UsageMonitor {
    pub fn inspect_struct(input: &ItemStruct) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_item_struct(&input);
        monitor
    }

    pub fn inspect_lt_def(input: &LifetimeDef) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_lifetime_def(&input);
        monitor
    }

    pub fn inspect_type_param(input: &TypeParam) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_type_param(&input);
        monitor
    }

    pub fn inspect_where_pred(input: &WherePredicate) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_where_predicate(&input);
        monitor
    }
}

impl<'ast> Visit<'ast> for UsageMonitor {
    fn visit_lifetime(&mut self, lt: &'ast Lifetime) {
        self.lifetimes.insert(lt.to_string());

        syn::visit::visit_lifetime(self, lt)
    }

    fn visit_type_path(&mut self, tp: &'ast TypePath) {
        self.type_paths.insert(tp.to_token_stream().to_string());

        syn::visit::visit_type_path(self, tp)
    }
}

pub trait UsageMonitorAdapter {
    fn inspect(&self) -> UsageMonitor;
}

impl UsageMonitorAdapter for ItemStruct {
    fn inspect(&self) -> UsageMonitor {
        UsageMonitor::inspect_struct(self)
    }
}

impl UsageMonitorAdapter for LifetimeDef {
    fn inspect(&self) -> UsageMonitor {
        UsageMonitor::inspect_lt_def(self)
    }
}

impl UsageMonitorAdapter for TypeParam {
    fn inspect(&self) -> UsageMonitor {
        UsageMonitor::inspect_type_param(self)
    }
}

impl UsageMonitorAdapter for WherePredicate {
    fn inspect(&self) -> UsageMonitor {
        UsageMonitor::inspect_where_pred(self)
    }
}

pub trait GenericsFilter {
    fn filter_unused(&self, ast: &impl UsageMonitorAdapter) -> Result<Generics, Error>;
}

impl GenericsFilter for Generics {
    fn filter_unused(&self, ast: &impl UsageMonitorAdapter) -> Result<Generics, Error> {
        // Extract the generics
        let mut ltdefs: Vec<_> = self.lifetimes().into_iter().cloned().collect();
        let mut tparams: Vec<_> = self.type_params().into_iter().cloned().collect();
        let mut preds: Option<Vec<_>> = self
            .where_clause
            .as_ref()
            .map(|w| w.predicates.iter().collect());

        // Get the string representation of every generic
        let lifetimes: HashSet<_> = ltdefs
            .iter()
            .map(|ltdef| ltdef.lifetime.to_string())
            .collect();
        let type_idents: HashSet<_> = tparams
            .iter()
            .map(|tparam| tparam.ident.to_string())
            .collect();

        // Find all lifetimes and type paths used in the given ast
        let used = ast.inspect();

        // Find the unused generics
        let mut unused_lifetimes: Vec<_> = lifetimes.difference(&used.lifetimes).collect();
        let mut unused_tparams: Vec<_> = type_idents.difference(&used.type_paths).collect();

        // Again, go through the generics to find indirect usages
        let mut rem_unused_lifetimes = Vec::new();
        for (i, lt) in unused_lifetimes.iter().enumerate() {
            let mut used = false;

            // Check all lifetime defs
            for ltdef in ltdefs.iter() {
                if &ltdef.lifetime.to_string() != *lt {
                    let usage = UsageMonitor::inspect_lt_def(ltdef);
                    if usage.lifetimes.contains(*lt) {
                        used = true;
                        break;
                    }
                }
            }

            // Check all type params
            for tparam in tparams.iter() {
                let usage = UsageMonitor::inspect_type_param(tparam);
                if usage.lifetimes.contains(*lt) {
                    used = true;
                    break;
                }
            }

            // Finally, check the predicates
            if let Some(preds) = &preds {
                for pred in preds.iter() {
                    if let WherePredicate::Lifetime(llt) = pred {
                        if &llt.lifetime.to_string() == *lt {
                            continue;
                        }
                    }
                    let usage = UsageMonitor::inspect_where_pred(pred);
                    if usage.lifetimes.contains(*lt) {
                        used = true;
                        break;
                    }
                }
            }

            // If the lifetime is used in other generics, mark it for removal from the unused list
            if used {
                rem_unused_lifetimes.push(i);
            }
        }

        let mut rem_unused_tparams = Vec::new();
        for (i, tp) in unused_tparams.iter().enumerate() {
            let mut used = false;

            // Check all lifetime defs
            for ltdef in ltdefs.iter() {
                let usage = UsageMonitor::inspect_lt_def(ltdef);
                if usage.type_paths.contains(*tp) {
                    used = true;
                    break;
                }
            }

            // Check all type params
            for tparam in tparams.iter() {
                if &tparam.ident.to_string() != *tp {
                    let usage = UsageMonitor::inspect_type_param(tparam);
                    if usage.type_paths.contains(*tp) {
                        used = true;
                        break;
                    }
                }
            }

            // Finally, check the predicates
            if let Some(preds) = &preds {
                for pred in preds.iter() {
                    if let WherePredicate::Type(ttp) = pred {
                        if &ttp.bounded_ty.to_token_stream().to_string() == *tp {
                            continue;
                        }
                    }
                    let usage = UsageMonitor::inspect_where_pred(pred);
                    if usage.type_paths.contains(*tp) {
                        used = true;
                        break;
                    }
                }
            }

            // If the tparam is used in other generics, mark it for removal from the unused list
            if used {
                rem_unused_tparams.push(i);
            }
        }

        // Clean the unused lists
        for (ith, remove) in rem_unused_lifetimes.into_iter().enumerate() {
            unused_lifetimes.remove(remove - ith);
        }
        for (ith, remove) in rem_unused_tparams.into_iter().enumerate() {
            unused_tparams.remove(remove - ith);
        }

        // Remove unused lifetimes
        for lt in unused_lifetimes {
            // Remove the unused lifetime from the definition
            let index = ltdefs
                .iter()
                .position(|ltdef| &ltdef.lifetime.to_string() == lt)
                .unwrap();
            ltdefs.remove(index);

            // Look in the where clause predicates if there is a match to remove
            if let Some(preds) = &mut preds {
                let index = preds.iter().position(|pred| {
                    if let WherePredicate::Lifetime(ltpred) = pred {
                        &ltpred.lifetime.to_string() == lt
                    } else {
                        false
                    }
                });
                if let Some(index) = index {
                    preds.remove(index);
                }
            }
        }

        // Remove unused type params
        for tp in unused_tparams {
            // Remove the unused type param from the definition
            let index = tparams
                .iter()
                .position(|tparam| &tparam.ident.to_token_stream().to_string() == tp)
                .unwrap();
            tparams.remove(index);

            // Look in the where clause predicates if there is a match to remove
            if let Some(preds) = &mut preds {
                let index = preds.iter().position(|pred| match pred {
                    WherePredicate::Type(tpred) => {
                        &tpred.bounded_ty.to_token_stream().to_string() == tp
                    }
                    WherePredicate::Eq(eqpred) => {
                        &eqpred.lhs_ty.to_token_stream().to_string() == tp
                    }
                    _ => false,
                });
                if let Some(index) = index {
                    preds.remove(index);
                }
            }
        }

        // Build the new generics
        let inbetween = if ltdefs.is_empty() || tparams.is_empty() {
            quote! {}
        } else {
            quote! { , }
        };
        let tokens = match preds {
            Some(preds) if !preds.is_empty() => {
                quote! { struct T< #(#ltdefs),* #inbetween #(#tparams),* > where #(#preds),*; }
            }
            _ => {
                quote! { struct T< #(#ltdefs),* #inbetween #(#tparams),* >; }
            }
        };
        let ast: ItemStruct = syn::parse2(tokens)?;
        Ok(ast.generics)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Tests
///////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::helper::{GenericsFilter, SnakeCase};
    use quote::{quote, ToTokens};
    use syn::ItemStruct;

    #[test]
    fn to_snake_case() {
        assert_eq!("IpAddress".to_snake_case(), "ip_address");
        assert_eq!("TCP".to_snake_case(), "tcp");
        assert_eq!("snake_case".to_snake_case(), "snake_case");
        assert_eq!("HOME_IP".to_snake_case(), "home_ip");
    }

    #[test]
    fn struct_generic_usage() {
        let tokens = quote! {
            struct Test<'a, 'b, T, I: 'b + ToString> where T: Iterator<Item=I> {
                field: I,
            }
        };
        let mut ast: ItemStruct = syn::parse2(tokens).unwrap();
        let generics = std::mem::take(&mut ast.generics);
        ast.generics = generics.filter_unused(&ast).unwrap();

        let result = ast.to_token_stream().to_string();
        assert_eq!(
            result,
            "struct Test < 'b , I : 'b + ToString > { field : I , }"
        );
    }
}
