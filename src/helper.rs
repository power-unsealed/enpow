use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{collections::HashSet, str::FromStr};
use syn::{
    parenthesized, parse::Parse, parse::ParseStream, punctuated::Punctuated, spanned::Spanned,
    visit::Visit, Attribute, Data, DataEnum, DeriveInput, Error, Fields, GenericParam, Generics,
    Ident, ItemStruct, Lifetime, LifetimeDef, LitStr, Path, Token, Type, TypeParam, TypePath,
    TypeTuple, Variant, Visibility, WherePredicate,
};

macro_rules! cache_access {
    ($cache:expr, $builder:expr) => {{
        if let None = ($cache).as_ref() {
            $cache = Some($builder);
        }

        ($cache).as_ref().unwrap()
    }};
}

/// Unified configuration attribute
pub struct InnerAttributeInfo {
    pub span: Span,
    pub derives: Vec<Path>,
    pub type_name: Option<TypeNaming>,
    pub method_name: Option<Ident>,
}

impl InnerAttributeInfo {
    fn save_into_or_redundant_err<T: Spanned>(
        new: T,
        option: &mut Option<T>,
        what: &str,
    ) -> Result<(), Error> {
        if let Some(old) = option {
            Error::new(old.span(), format!("First {what} defined here")).to_compile_error();
            Err(Error::new(new.span(), format!("Redundant {what} detected")))
        } else {
            *option = Some(new);
            Ok(())
        }
    }
}

trait InnerAttributeCheck {
    fn is_inner_config(&self) -> bool;
}

impl InnerAttributeCheck for Attribute {
    fn is_inner_config(&self) -> bool {
        self.path
            .get_ident()
            .map_or(false, |ident| &ident.to_string() == "inner")
    }
}

impl Parse for InnerAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        // Parse the arguments to the attribute
        let content;
        parenthesized!(content in input);
        let span = content.span();
        let args: Punctuated<_, Token![,]> = content.parse_terminated(InnerArgument::parse)?;

        // Extract the information from the arguments
        let mut derives = Vec::new();
        let mut type_name = None;
        let mut method_name = None;
        for arg in args {
            match arg {
                InnerArgument::Derive(paths) => {
                    derives.extend(paths);
                }
                InnerArgument::TypeName(ident) => {
                    InnerAttributeInfo::save_into_or_redundant_err(
                        ident,
                        &mut type_name,
                        "type name",
                    )?;
                }
                InnerArgument::MethodName(ident) => {
                    InnerAttributeInfo::save_into_or_redundant_err(
                        ident,
                        &mut method_name,
                        "method name",
                    )?;
                }
            }
        }

        Ok(InnerAttributeInfo {
            span,
            derives,
            type_name,
            method_name,
        })
    }
}

#[derive(Clone)]
pub enum TypeNaming {
    Variant(Span),
    EnumVariant(Span),
    VariantEnum(Span),
    Custom(String, Span),
}

impl TypeNaming {
    pub fn format(&self, enum_id: &Ident, var_id: &Ident) -> Ident {
        match self {
            TypeNaming::Variant(_) => Ident::new(&var_id.to_string(), Span::call_site()),
            TypeNaming::EnumVariant(_) => format_ident!("{enum_id}{var_id}"),
            TypeNaming::VariantEnum(_) => format_ident!("{var_id}{enum_id}"),
            TypeNaming::Custom(format, _) => {
                // Replace all {enum} and {var}
                let output = format.replace("{enum}", &enum_id.to_string());
                let output = output.replace("{var}", &var_id.to_string());

                Ident::new(&output, Span::call_site())
            }
        }
    }
}

impl Spanned for TypeNaming {
    fn span(&self) -> Span {
        match self {
            TypeNaming::Variant(span) => *span,
            TypeNaming::EnumVariant(span) => *span,
            TypeNaming::VariantEnum(span) => *span,
            TypeNaming::Custom(_, span) => *span,
        }
    }
}

impl Parse for TypeNaming {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(LitStr) {
            let format: LitStr = input.parse()?;

            // Check whether it is generating valid idents
            let output = format.value().replace("{enum}", "A");
            let output = output.replace("{var}", "B");
            let tokens = match TokenStream::from_str(&output) {
                Ok(tokens) => tokens,
                Err(_) => {
                    return Err(Error::new(
                        format.span(),
                        "Does not format into valid identifiers",
                    ));
                }
            };
            if syn::parse2::<Ident>(tokens).is_err() {
                return Err(Error::new(
                    format.span(),
                    "Does not format into valid identifiers",
                ));
            }

            Ok(TypeNaming::Custom(format.value(), format.span()))
        } else {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "Var" => Ok(TypeNaming::Variant(ident.span())),
                "EnumVar" => Ok(TypeNaming::EnumVariant(ident.span())),
                "VarEnum" => Ok(TypeNaming::VariantEnum(ident.span())),
                _ => Err(Error::new(ident.span(), "Unexpected type naming scheme")),
            }
        }
    }
}

enum InnerArgument {
    Derive(Vec<Path>),
    TypeName(TypeNaming),
    MethodName(Ident),
}

impl Parse for InnerArgument {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "derive" => {
                let content;
                parenthesized!(content in input);
                let args: Punctuated<_, Token![,]> = content.parse_terminated(Path::parse)?;
                let paths = args.into_iter().collect();
                Ok(InnerArgument::Derive(paths))
            }
            "type_name" | "type_names" => {
                input.parse::<Token![=]>()?;
                let naming: TypeNaming = input.parse()?;
                Ok(InnerArgument::TypeName(naming))
            }
            "method_name" => {
                input.parse::<Token![=]>()?;
                let ident: LitStr = input.parse()?;
                Ok(InnerArgument::MethodName(ident.parse()?))
            }
            _ => Err(Error::new(ident.span(), "Unexpected argument")),
        }
    }
}

pub struct EnumInfo {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    /// The parsed `inner` attributes with their index in the attribute collection
    pub inners: Vec<(usize, InnerAttributeInfo)>,
    /// All `inner(derive())` directly applied to the enum itself
    pub derives: Vec<Path>,
    pub type_naming: TypeNaming,
    pub generics: Generics,
    pub identifier: Ident,
    pub visibility: Visibility,
    pub data: DataEnum,
    /// Whether there are other calls to `enpow` or `extract`, starting from the attribute at the
    /// given index
    pub other_calls_from: Option<usize>,
}

pub trait EnumInfoAdapter {
    fn extract_info(self) -> Result<EnumInfo, Error>;
}

impl EnumInfoAdapter for DeriveInput {
    fn extract_info(self) -> Result<EnumInfo, Error> {
        let span = self.span();

        // Extract the enum data
        let data = match self.data {
            Data::Enum(data) => data,
            _ => {
                return Err(Error::new(
                    Span::call_site(),
                    "Can only be applied to enums",
                ));
            }
        };

        // Parse all `inner` config attributes
        let mut inners = Vec::new();
        let mut derives = Vec::new();
        let mut type_naming = None;
        for (i, attr) in self.attrs.iter().enumerate() {
            if attr.is_inner_config() {
                let inner: InnerAttributeInfo = syn::parse2(attr.tokens.clone())?;

                if inner.method_name.is_some() {
                    return Err(Error::new(
                        inner.span,
                        "Method renaming only supported on variants, not on the enum itself.",
                    ));
                }

                if let Some(new) = &inner.type_name {
                    InnerAttributeInfo::save_into_or_redundant_err(
                        new.clone(),
                        &mut type_naming,
                        "type name",
                    )?;
                }

                derives.extend(inner.derives.iter().cloned());
                inners.push((i, inner));
            }
        }

        // If there is no type name set, default to `EnumVar`
        let type_naming = type_naming.unwrap_or_else(|| TypeNaming::EnumVariant(Span::call_site()));

        // If there is a custom type name, make sure that it contains at least a {var} to make each
        // variant distinctly named
        if let TypeNaming::Custom(format, _) = &type_naming {
            if !format.contains("{var}") {
                return Err(Error::new(
                    type_naming.span(),
                    "Expected type name to contain at least `{var}` to give each variants \
                    type a different name",
                ));
            }
        }

        // Check for additional calls to `enpow` or `extract`
        let mut other_calls_from = None;
        for (i, attr) in self.attrs.iter().enumerate() {
            let last = attr
                .path
                .segments
                .last()
                .map(|s| s.to_token_stream().to_string());
            if last.map_or(false, |str| str == "enpow" || str == "extract") {
                other_calls_from = Some(i);
                break;
            }
        }

        Ok(EnumInfo {
            span,
            attributes: self.attrs,
            generics: self.generics,
            derives,
            type_naming,
            inners,
            identifier: self.ident,
            visibility: self.vis,
            data,
            other_calls_from,
        })
    }
}

pub struct UnnamedFieldInfo {
    pub docs: Vec<Attribute>,
    pub data_type: Type,
}

pub struct NamedFieldInfo {
    pub docs: Vec<Attribute>,
    pub identifier: Ident,
    pub data_type: Type,
}

struct SelfRefMut<T> {
    vself: T,
    vref: T,
    vmut: T,
}

impl<T> SelfRefMut<T> {
    fn new(vself: T, vref: T, vmut: T) -> Self {
        Self { vself, vref, vmut }
    }
}

pub enum VariantType {
    Unit,
    Single(UnnamedFieldInfo),
    Unnamed(Vec<UnnamedFieldInfo>),
    Named(Vec<NamedFieldInfo>),
}

pub struct VariantInfo {
    pub docs: Vec<Attribute>,
    pub inners: Vec<(usize, InnerAttributeInfo)>,
    /// All `inner(derive())` applied to the enum or to this variant
    pub derives: Vec<Path>,
    pub visibility: Visibility,
    pub var_type: VariantType,
    /// Variant identifier
    pub identifier: Ident,
    /// Variant identifier in snake case
    pub method_name: String,
    /// `Enum::Variant`
    pub full_path: Path,
    pub generics: Generics,
    pub ref_generics: Generics,
    pub ref_lifetime: Lifetime,
    /// Identifiers of extracted types
    type_idents: SelfRefMut<Ident>,
    type_defs_cache: SelfRefMut<Option<TokenStream>>,
    data_types_cache: SelfRefMut<Option<TokenStream>>,
    pattern_cache: Option<TokenStream>,
    construction_cache: SelfRefMut<Option<TokenStream>>,
}

impl VariantInfo {
    pub fn new(
        var_type: VariantType,
        identifier: Ident,
        attributes: Vec<Attribute>,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;

        // Build path
        let full_path: Path = syn::parse2(quote! { #enum_ident :: #identifier })?;

        // Parse all `inner` config attributes
        let mut inners = Vec::new();
        let mut type_name = None;
        let mut method_name = None;
        let mut derives = parent.derives.clone();
        for (i, attr) in attributes.iter().enumerate() {
            if attr.is_inner_config() {
                let inner: InnerAttributeInfo = syn::parse2(attr.tokens.clone())?;

                if let Some(new) = &inner.type_name {
                    InnerAttributeInfo::save_into_or_redundant_err(
                        new.clone(),
                        &mut type_name,
                        "type name",
                    )?;
                }

                if let Some(new) = &inner.method_name {
                    InnerAttributeInfo::save_into_or_redundant_err(
                        new.clone(),
                        &mut method_name,
                        "method name",
                    )?;
                }

                derives.extend(inner.derives.iter().cloned());
                inners.push((i, inner));
            }
        }

        // Buid the identifiers depending on the given variant name and possible renames
        let method_name = match method_name {
            Some(method_name) => method_name.to_string(),
            None => identifier.to_string().to_snake_case(),
        };
        let self_ident = match type_name {
            Some(type_name) => type_name.format(enum_ident, &identifier),
            None => parent.type_naming.format(enum_ident, &identifier),
        };
        let ref_ident = format_ident!("{self_ident}Ref");
        let mut_ident = format_ident!("{self_ident}Mut");
        let type_idents = SelfRefMut::new(self_ident, ref_ident, mut_ident);

        // Extract doc comments
        let docs = attributes.extract_docs();

        // Build type idents

        // Build a dummy type tuple with all types contained in the fields.
        // Then parse it and find out which generics were actually used in it.
        // Use this info to filter all unused generics
        let generics = &parent.generics;
        let tokens = match &var_type {
            VariantType::Unit => quote! { () },
            VariantType::Single(field) => {
                let dtype = &field.data_type;
                quote! { ( #dtype, ) }
            }
            VariantType::Unnamed(fields) => {
                let types: Vec<_> = fields.iter().map(|field| &field.data_type).collect();
                quote! { ( #(#types),*, ) }
            }
            VariantType::Named(fields) => {
                let types: Vec<_> = fields.iter().map(|field| &field.data_type).collect();
                quote! { ( #(#types),*, ) }
            }
        };
        let ast: TypeTuple = syn::parse2(tokens)?;
        let generics = generics.filter_unused(&ast)?;

        // Add a lifetime to the generics
        let mut ref_generics = generics.clone();
        let lifetime_name = format!(
            "'__{}_{}",
            enum_ident.to_string().to_snake_case(),
            identifier.to_string().to_snake_case(),
        );
        let ref_lifetime = Lifetime::new(&lifetime_name, Span::call_site());
        ref_generics
            .params
            .push(GenericParam::Lifetime(LifetimeDef::new(
                ref_lifetime.clone(),
            )));

        Ok(VariantInfo {
            docs,
            inners,
            derives,
            visibility: parent.visibility.clone(),
            var_type,
            method_name,
            full_path,
            identifier,
            generics,
            ref_generics,
            ref_lifetime,
            type_idents,
            type_defs_cache: SelfRefMut::new(None, None, None),
            data_types_cache: SelfRefMut::new(None, None, None),
            pattern_cache: None,
            construction_cache: SelfRefMut::new(None, None, None),
        })
    }

    fn build_type_def(
        &self,
        ident: &Ident,
        generics: &Generics,
        type_prefix: TokenStream,
    ) -> TokenStream {
        let docs = &self.docs;
        let visibility = &self.visibility;
        let (gen_full, _, gen_where) = generics.split_for_impl();

        match &self.var_type {
            VariantType::Unit => {
                quote! { #(#docs)* #visibility struct #ident; }
            }
            VariantType::Single(field) => {
                let field_docs = &field.docs;
                let dtype = &field.data_type;
                let field = quote! { #(#field_docs)* pub #type_prefix #dtype };

                quote! {
                    #(#docs)*
                    #visibility struct #ident #gen_full ( #field ) #gen_where;
                }
            }
            VariantType::Unnamed(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        let docs = &field.docs;
                        let dtype = &field.data_type;
                        quote! { #(#docs)* pub #type_prefix #dtype }
                    })
                    .collect();

                quote! {
                    #(#docs)*
                    #visibility struct #ident #gen_full ( #(#fields),* ) #gen_where;
                }
            }
            VariantType::Named(fields) => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        let docs = &field.docs;
                        let ident = &field.identifier;
                        let dtype = &field.data_type;
                        quote! { #(#docs)* pub #ident: #type_prefix #dtype }
                    })
                    .collect();

                quote! {
                    #(#docs)*
                    #visibility struct #ident #gen_full #gen_where { #(#fields),* }
                }
            }
        }
    }

    pub fn build_self_type_def(&mut self) -> TokenStream {
        cache_access!(
            self.type_defs_cache.vself,
            self.build_type_def(&self.type_idents.vself, &self.generics, quote! {})
        )
        .clone()
    }

    pub fn build_ref_type_def(&mut self) -> TokenStream {
        cache_access!(self.type_defs_cache.vref, {
            let generics = &self.ref_generics;
            let lifetime = &self.ref_lifetime;
            self.build_type_def(&self.type_idents.vref, generics, quote! { &#lifetime })
        })
        .clone()
    }

    pub fn build_mut_type_def(&mut self) -> TokenStream {
        cache_access!(self.type_defs_cache.vmut, {
            let generics = &self.ref_generics;
            let lifetime = &self.ref_lifetime;
            self.build_type_def(&self.type_idents.vmut, generics, quote! { &#lifetime mut })
        })
        .clone()
    }

    pub fn build_self_type(&mut self) -> TokenStream {
        cache_access!(self.data_types_cache.vself, {
            match &self.var_type {
                VariantType::Unit => quote! { () },
                VariantType::Single(field) => {
                    let dtype = &field.data_type;
                    quote! { #dtype }
                }
                VariantType::Unnamed(fields) => {
                    let types: Vec<_> = fields.iter().map(|field| &field.data_type).collect();
                    quote! { ( #(#types),*, ) }
                }
                VariantType::Named(_) => self.build_extracted_self_type(),
            }
        })
        .clone()
    }

    pub fn build_extracted_self_type(&self) -> TokenStream {
        let (_, gen_short, _) = self.generics.split_for_impl();
        let ident = &self.type_idents.vself;
        quote! { #ident #gen_short }
    }

    pub fn build_ref_type(&mut self) -> TokenStream {
        cache_access!(self.data_types_cache.vref, {
            let lifetime = &self.ref_lifetime;
            match &self.var_type {
                VariantType::Unit => quote! { () },
                VariantType::Single(field) => {
                    let dtype = &field.data_type;
                    quote! { &#lifetime #dtype }
                }
                VariantType::Unnamed(fields) => {
                    let types: Vec<_> = fields
                        .iter()
                        .map(|field| {
                            let dtype = &field.data_type;
                            quote! { &#lifetime #dtype }
                        })
                        .collect();
                    quote! { ( #(#types),*, ) }
                }
                VariantType::Named(_) => self.build_extracted_ref_type(),
            }
        })
        .clone()
    }

    pub fn build_extracted_ref_type(&self) -> TokenStream {
        let (_, gen_short, _) = self.ref_generics.split_for_impl();
        let ident = &self.type_idents.vref;
        quote! { #ident #gen_short }
    }

    pub fn build_mut_type(&mut self) -> TokenStream {
        cache_access!(self.data_types_cache.vmut, {
            let lifetime = &self.ref_lifetime;
            match &self.var_type {
                VariantType::Unit => quote! { () },
                VariantType::Single(field) => {
                    let dtype = &field.data_type;
                    quote! { &#lifetime mut #dtype }
                }
                VariantType::Unnamed(fields) => {
                    let types: Vec<_> = fields
                        .iter()
                        .map(|field| {
                            let dtype = &field.data_type;
                            quote! { &#lifetime mut #dtype }
                        })
                        .collect();
                    quote! { ( #(#types),*, ) }
                }
                VariantType::Named(_) => self.build_extracted_mut_type(),
            }
        })
        .clone()
    }

    pub fn build_extracted_mut_type(&self) -> TokenStream {
        let (_, gen_short, _) = self.ref_generics.split_for_impl();
        let ident = &self.type_idents.vmut;
        quote! { #ident #gen_short }
    }

    fn build_tuple_ident_seq(count: usize) -> Vec<Ident> {
        (0..count)
            .into_iter()
            .map(|i| format_ident!("f{i}"))
            .collect()
    }

    pub fn build_match_pattern(&mut self) -> TokenStream {
        cache_access!(self.pattern_cache, {
            let path = &self.full_path;

            match &self.var_type {
                VariantType::Unit => quote! { #path },
                VariantType::Single(_) => {
                    let var = &VariantInfo::build_tuple_ident_seq(1)[0];
                    quote! { #path( #var ) }
                }
                VariantType::Unnamed(fields) => {
                    let vars = VariantInfo::build_tuple_ident_seq(fields.len());
                    quote! { #path( #(#vars),*, ) }
                }
                VariantType::Named(fields) => {
                    let vars: Vec<_> = fields.iter().map(|field| &field.identifier).collect();

                    quote! { #path { #(#vars),* } }
                }
            }
        })
        .clone()
    }

    fn build_construction(&self, ident: &Ident) -> TokenStream {
        match &self.var_type {
            VariantType::Unit => quote! { () },
            VariantType::Single(_) => {
                let var = &VariantInfo::build_tuple_ident_seq(1)[0];
                quote! { #var }
            }
            VariantType::Unnamed(fields) => {
                let vars = VariantInfo::build_tuple_ident_seq(fields.len());
                quote! { ( #(#vars),*, ) }
            }
            VariantType::Named(fields) => {
                let vars: Vec<_> = fields.iter().map(|field| &field.identifier).collect();
                quote! { #ident { #(#vars),* } }
            }
        }
    }

    pub fn build_self_construction(&mut self) -> TokenStream {
        cache_access!(
            self.construction_cache.vself,
            self.build_construction(&self.type_idents.vself)
        )
        .clone()
    }

    pub fn build_ref_construction(&mut self) -> TokenStream {
        cache_access!(
            self.construction_cache.vref,
            self.build_construction(&self.type_idents.vref)
        )
        .clone()
    }

    pub fn build_mut_construction(&mut self) -> TokenStream {
        cache_access!(
            self.construction_cache.vmut,
            self.build_construction(&self.type_idents.vmut)
        )
        .clone()
    }

    /// Builds an implementation of the From trait for this variant. It is expected, that the
    /// variant will be of the form `Enum::Variant(ExtractedSelfType)` in the final code! That
    /// means, this only works for extracted variants.
    pub fn build_from_impl_after_extraction(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_extracted_self_type();
        let path = &self.full_path;
        let enum_ident = &parent.identifier;
        let (gen_main, gen_short, gen_where) = parent.generics.split_for_impl();

        quote! {
            impl #gen_main From<#data_type> for #enum_ident #gen_short #gen_where {
                fn from(inner: #data_type) -> Self {
                    #path(inner)
                }
            }
        }
    }

    pub fn build_from_impl_without_extraction(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_self_type();
        let path = &self.full_path;
        let enum_ident = &parent.identifier;
        let (gen_main, gen_short, gen_where) = parent.generics.split_for_impl();

        let builder = match &self.var_type {
            VariantType::Unit => quote! {
                #path
            },
            VariantType::Single(_) => quote! {
                #path ( inner )
            },
            _ => {
                let destruction = self.build_construction(&self.type_idents.vself);
                let construction = self.build_match_pattern();
                quote! {
                    let #destruction = inner;
                    #construction
                }
            }
        };

        quote! {
            impl #gen_main From<#data_type> for #enum_ident #gen_short #gen_where {
                fn from(inner: #data_type) -> Self {
                    #builder
                }
            }
        }
    }

    /////////////
    // Methods //
    /////////////

    pub fn build_variant(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = Ident::new_raw(method_name, Span::call_site());

        quote! {
            /// Returns the inner data, if the enum value is of the expected type, otherwise
            /// returns `None`.
            #visibility fn #fn_ident(self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_as_ref(&mut self) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("{method_name}_as_ref");

        quote! {
            /// Returns a reference to the inner data, if the enum value is of the expected type,
            /// otherwise returns `None`.
            #visibility fn #fn_ident<#lifetime>(&#lifetime self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_as_mut(&mut self) -> TokenStream {
        let data_type = self.build_mut_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_mut_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("{method_name}_as_mut");

        quote! {
            /// Returns a mutable reference to the inner data, if the enum value is of the expected
            /// type, otherwise returns `None`.
            #visibility fn #fn_ident<#lifetime>(&#lifetime mut self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_is(&mut self) -> TokenStream {
        let pattern = self.build_match_pattern();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("is_{method_name}");

        quote! {
            /// Returns `true`, if the enum value is of the expected type, otherwise returns
            /// `false`.
            #visibility fn #fn_ident(&self) -> bool {
                match self {
                    #pattern => true,
                    _ => false,
                }
            }
        }
    }

    pub fn build_is_and(&mut self) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("is_{method_name}_and");

        quote! {
            /// Returns `true`, if the enum value is of the expected type and the given closure
            /// evalutates to `true`, otherwise returns `false`.
            #visibility fn #fn_ident<#lifetime>(
                &#lifetime self,
                f: impl FnOnce(#data_type) -> bool
            ) -> bool {
                match self {
                    #pattern => f(#construction),
                    _ => false,
                }
            }
        }
    }

    pub fn build_unwrap(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("unwrap_{method_name}");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            /// Returns the inner data, if the enum value is of the expected type, otherwise
            /// panics.
            #visibility fn #fn_ident(self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_unwrap_as_ref(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("unwrap_{method_name}_as_ref");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            /// Returns a reference to the inner data, if the enum value is of the expected type,
            /// otherwise panics.
            #visibility fn #fn_ident<#lifetime>(&#lifetime self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_unwrap_as_mut(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_mut_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_mut_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("unwrap_{method_name}_as_mut");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            /// Returns a mutable reference to the inner data, if the enum value is of the expected
            /// type, otherwise panics.
            #visibility fn #fn_ident<#lifetime>(&#lifetime mut self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_unwrap_or(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("unwrap_{method_name}_or");

        quote! {
            /// Returns the inner data, if the enum value is of the expected type, otherwise
            /// returns the given default value.
            #visibility fn #fn_ident(self, default: #data_type) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => default,
                }
            }
        }
    }

    pub fn build_unwrap_or_else(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("unwrap_{method_name}_or_else");

        quote! {
            /// Returns the inner data, if the enum value is of the expected type, otherwise
            /// returns the value that the given closure evaluated to.
            #visibility fn #fn_ident(self, f: impl FnOnce(Self) -> #data_type) -> #data_type {
                match self {
                    #pattern => #construction,
                    some => f(some),
                }
            }
        }
    }

    pub fn build_expect(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("expect_{method_name}");

        quote! {
            /// Returns the inner data, if the enum is of the expected type, otherwise panics with
            /// the given error message.
            #visibility fn #fn_ident(self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{}", msg),
                }
            }
        }
    }

    pub fn build_expect_as_ref(&mut self) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("expect_{method_name}_as_ref");

        quote! {
            /// Returns a reference to the inner data, if the enum is of the expected type,
            /// otherwise panics with the given error message.
            #visibility fn #fn_ident<#lifetime>(&#lifetime self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{}", msg),
                }
            }
        }
    }

    pub fn build_expect_as_mut(&mut self) -> TokenStream {
        let data_type = self.build_mut_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_mut_construction();
        let lifetime = &self.ref_lifetime;
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("expect_{method_name}_as_mut");

        quote! {
            /// Returns a mutable reference to the inner data, if the enum is of the expected type,
            /// otherwise panics with the given error message.
            #visibility fn #fn_ident<#lifetime>(&#lifetime mut self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{}", msg),
                }
            }
        }
    }

    pub fn build_map_or(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("map_{method_name}_or");

        quote! {
            /// Applies the given operation to the inner data and returns its result, if the enum
            /// value is of the expected type, otherwise returns the given default value.
            #visibility fn #fn_ident<__T>(
                self,
                default: __T,
                op: impl FnOnce(#data_type) -> __T
            ) -> __T {
                match self {
                    #pattern => op(#construction),
                    _ => default,
                }
            }
        }
    }

    pub fn build_map_or_else(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let method_name = &self.method_name;
        let visibility = &self.visibility;

        let fn_ident = format_ident!("map_{method_name}_or_else");

        quote! {
            /// Applies the given operation to the inner data and returns its result, if the enum
            /// value is of the expected type, otherwise returns the value that the given default
            /// closure evaluates to.
            #visibility fn #fn_ident<__T>(
                self,
                default: impl FnOnce(Self) -> __T,
                op: impl FnOnce(#data_type) -> __T
            ) -> __T {
                match self {
                    #pattern => op(#construction),
                    other => default(other),
                }
            }
        }
    }
}

pub trait VariantInfoAdapter {
    fn extract_info(self, parent: &EnumInfo) -> Result<VariantInfo, Error>;
}

impl VariantInfoAdapter for Variant {
    fn extract_info(self, parent: &EnumInfo) -> Result<VariantInfo, Error> {
        let identifier = self.ident;
        let attributes = self.attrs;

        match self.fields {
            // Variant without data
            Fields::Unit => VariantInfo::new(VariantType::Unit, identifier, attributes, parent),

            // Variant with one unnamed fields
            Fields::Unnamed(mut tuple) if tuple.unnamed.len() == 1 => {
                let single = tuple.unnamed.pop().unwrap().into_value();
                let field = UnnamedFieldInfo {
                    docs: single.attrs.extract_docs(),
                    data_type: single.ty,
                };
                VariantInfo::new(VariantType::Single(field), identifier, attributes, parent)
            }

            // Variant with unnamed fields
            Fields::Unnamed(tuple) => {
                let fields = tuple
                    .unnamed
                    .into_iter()
                    .map(|field| UnnamedFieldInfo {
                        docs: field.attrs.extract_docs(),
                        data_type: field.ty,
                    })
                    .collect();
                VariantInfo::new(VariantType::Unnamed(fields), identifier, attributes, parent)
            }

            // Variant with named fields
            Fields::Named(fields) => {
                let fields = fields
                    .named
                    .into_iter()
                    .map(|field| NamedFieldInfo {
                        docs: field.attrs.extract_docs(),
                        identifier: field.ident.expect("Expected field identifier"),
                        data_type: field.ty,
                    })
                    .collect();
                VariantInfo::new(VariantType::Named(fields), identifier, attributes, parent)
            }
        }
    }
}

pub trait DocExtractor {
    fn extract_docs(self) -> Vec<Attribute>;
}

impl DocExtractor for Vec<Attribute> {
    fn extract_docs(self) -> Vec<Attribute> {
        self.into_iter()
            .filter(|attr| match attr.path.get_ident() {
                Some(ident) => *ident == "doc",
                None => false,
            })
            .collect()
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
        monitor.visit_item_struct(input);
        monitor
    }

    pub fn inspect_lt_def(input: &LifetimeDef) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_lifetime_def(input);
        monitor
    }

    pub fn inspect_type_param(input: &TypeParam) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_type_param(input);
        monitor
    }

    pub fn inspect_where_pred(input: &WherePredicate) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_where_predicate(input);
        monitor
    }

    pub fn inspect_type_tuple(input: &TypeTuple) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_type_tuple(input);
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

impl UsageMonitorAdapter for TypeTuple {
    fn inspect(&self) -> UsageMonitor {
        UsageMonitor::inspect_type_tuple(self)
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
