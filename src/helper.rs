use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::HashSet;
use syn::{
    parenthesized, parse::Parse, parse::ParseStream, punctuated::Punctuated, spanned::Spanned,
    visit::Visit, Attribute, Data, DataEnum, DeriveInput, Error, Fields, GenericParam, Generics,
    Ident, ItemStruct, Lifetime, LifetimeDef, Path, Token, Type, TypeParam, TypePath, TypeTuple,
    Variant, Visibility, WherePredicate,
};

macro_rules! cache_access {
    ($cache:expr, $builder:expr) => {{
        if let None = ($cache).as_ref() {
            $cache = Some($builder);
        }

        ($cache).as_ref().unwrap()
    }};
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtractType {
    Unit,
    Single,
    Unnamed,
    Named,
}

impl ExtractType {
    pub fn from_attribute(attribute: TokenStream) -> Result<HashSet<ExtractType>, Error> {
        let info: ExtractAttributeInfo = syn::parse2(attribute)?;
        Ok(info.types)
    }
}

pub struct ExtractAttributeInfo {
    types: HashSet<ExtractType>,
}

impl Parse for ExtractAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut items: Punctuated<_, Token![,]> = input.parse_terminated(Ident::parse)?;
        let mut types = HashSet::new();

        // If there are no arguments, insert a fake "All"
        if items.is_empty() {
            items.push(Ident::new("All", Span::call_site()));
        }

        for item in items {
            match item.to_string().as_str() {
                "All" => {
                    types.insert(ExtractType::Unit);
                    types.insert(ExtractType::Single);
                    types.insert(ExtractType::Unnamed);
                    types.insert(ExtractType::Named);
                }
                "Unit" => {
                    types.insert(ExtractType::Unit);
                }
                "Single" => {
                    types.insert(ExtractType::Single);
                }
                "Unnamed" => {
                    types.insert(ExtractType::Unnamed);
                }
                "Named" => {
                    types.insert(ExtractType::Named);
                }
                some => {
                    return Err(Error::new_spanned(
                        item,
                        format!("Unknown argument `{some}`"),
                    ));
                }
            }
        }

        Ok(ExtractAttributeInfo { types })
    }
}

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

pub struct UnnamedFieldInfo {
    pub docs: Vec<Attribute>,
    pub data_type: Type,
}

pub struct NamedFieldInfo {
    pub docs: Vec<Attribute>,
    pub identifier: Ident,
    pub data_type: Type,
}

struct SrmVariant<T> {
    vself: T,
    vref: T,
    vmut: T,
}

impl<T> SrmVariant<T> {
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

impl VariantType {
    pub fn get_extract_type(&self) -> ExtractType {
        match self {
            VariantType::Unit => ExtractType::Unit,
            VariantType::Single(_) => ExtractType::Single,
            VariantType::Unnamed(_) => ExtractType::Unnamed,
            VariantType::Named(_) => ExtractType::Named,
        }
    }
}

pub struct VariantInfo {
    pub visibility: Visibility,
    pub var_type: VariantType,
    /// Variant identifier
    pub identifier: Ident,
    /// Variant identifier in snake case
    pub snake_case: String,
    /// `Enum::Variant`
    pub full_path: Path,
    pub generics: Generics,
    pub docs: Vec<Attribute>,
    type_idents: SrmVariant<Ident>,
    type_defs_cache: SrmVariant<Option<TokenStream>>,
    data_types_cache: SrmVariant<Option<TokenStream>>,
    pattern_cache: Option<TokenStream>,
    construction_cache: SrmVariant<Option<TokenStream>>,
}

impl VariantInfo {
    pub fn new(
        var_type: VariantType,
        identifier: Ident,
        docs: Vec<Attribute>,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;

        // Build path
        let full_path: Path = syn::parse2(quote! { #enum_ident :: #identifier })?;

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

        // Build type idents
        let self_ident = format_ident!("{enum_ident}{identifier}");
        let ref_ident = format_ident!("{enum_ident}{identifier}Ref");
        let mut_ident = format_ident!("{enum_ident}{identifier}Mut");

        Ok(VariantInfo {
            visibility: parent.visibility.clone(),
            var_type,
            snake_case: identifier.to_string().to_snake_case(),
            full_path,
            identifier,
            generics,
            docs,
            type_idents: SrmVariant::new(self_ident, ref_ident, mut_ident),
            type_defs_cache: SrmVariant::new(None, None, None),
            data_types_cache: SrmVariant::new(None, None, None),
            pattern_cache: None,
            construction_cache: SrmVariant::new(None, None, None),
        })
    }

    fn is_extract_type(&self, t: ExtractType) -> bool {
        self.var_type.get_extract_type() == t
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
                let docs = &field.docs;
                let dtype = &field.data_type;
                let field = quote! { #(#docs)* pub #type_prefix #dtype };

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

    fn build_generics_with_ref_lt(&self) -> (Generics, Lifetime) {
        // Add a lifetime to the generics
        let mut generics = self.generics.clone();
        let lifetime_name = format!("'{}", self.type_idents.vself.to_string().to_snake_case());
        let lifetime = Lifetime::new(&lifetime_name, Span::call_site());
        generics
            .params
            .push(GenericParam::Lifetime(LifetimeDef::new(lifetime.clone())));

        (generics, lifetime)
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
            let (generics, lifetime) = self.build_generics_with_ref_lt();
            self.build_type_def(&self.type_idents.vref, &generics, quote! { &#lifetime })
        })
        .clone()
    }

    pub fn build_mut_type_def(&mut self) -> TokenStream {
        cache_access!(self.type_defs_cache.vmut, {
            let (generics, lifetime) = self.build_generics_with_ref_lt();
            self.build_type_def(&self.type_idents.vmut, &generics, quote! { &#lifetime mut })
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
            match &self.var_type {
                VariantType::Unit => quote! { () },
                VariantType::Single(field) => {
                    let dtype = &field.data_type;
                    quote! { & #dtype }
                }
                VariantType::Unnamed(fields) => {
                    let types: Vec<_> = fields
                        .iter()
                        .map(|field| {
                            let dtype = &field.data_type;
                            quote! { & #dtype }
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
        let (_, gen_short, _) = self.generics.split_for_impl();
        let ident = &self.type_idents.vref;
        quote! { #ident #gen_short }
    }

    pub fn build_mut_type(&mut self) -> TokenStream {
        cache_access!(self.data_types_cache.vmut, {
            match &self.var_type {
                VariantType::Unit => quote! { () },
                VariantType::Single(field) => {
                    let dtype = &field.data_type;
                    quote! { &mut #dtype }
                }
                VariantType::Unnamed(fields) => {
                    let types: Vec<_> = fields
                        .iter()
                        .map(|field| {
                            let dtype = &field.data_type;
                            quote! { &mut #dtype }
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
        let (_, gen_short, _) = self.generics.split_for_impl();
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

    pub fn build_method_types(
        &mut self,
        parent: &EnumInfo,
        types: &[MethodType],
    ) -> Vec<TokenStream> {
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

    pub fn build_variant(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_as_ref(&mut self) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_as_mut(&mut self) -> TokenStream {
        let data_type = self.build_mut_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_mut_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_is(&mut self) -> TokenStream {
        let pattern = self.build_match_pattern();
        let snake_case = &self.snake_case;

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

    pub fn build_is_and(&mut self) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_unwrap(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_unwrap_as_ref(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_unwrap_as_mut(&mut self, parent: &EnumInfo) -> TokenStream {
        let data_type = self.build_mut_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_mut_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_unwrap_or(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_unwrap_or_else(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_expect(&mut self) -> TokenStream {
        let data_type = self.build_self_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_self_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_expect_as_ref(&mut self) -> TokenStream {
        let data_type = self.build_ref_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_ref_construction();
        let snake_case = &self.snake_case;

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

    pub fn build_expect_as_mut(&mut self) -> TokenStream {
        let data_type = self.build_mut_type();
        let pattern = self.build_match_pattern();
        let construction = self.build_mut_construction();
        let snake_case = &self.snake_case;

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
        let docs = self.attrs.extract_docs();

        match self.fields {
            // Variant without data
            Fields::Unit => VariantInfo::new(VariantType::Unit, identifier, docs, parent),

            // Variant with one unnamed fields
            Fields::Unnamed(mut tuple) if tuple.unnamed.len() == 1 => {
                let single = tuple.unnamed.pop().unwrap().into_value();
                let field = UnnamedFieldInfo {
                    docs: single.attrs.extract_docs(),
                    data_type: single.ty,
                };
                VariantInfo::new(VariantType::Single(field), identifier, docs, parent)
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
                VariantInfo::new(VariantType::Unnamed(fields), identifier, docs, parent)
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
                VariantInfo::new(VariantType::Named(fields), identifier, docs, parent)
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
                Some(ident) => ident.to_string() == "doc",
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

    pub fn inspect_type_tuple(input: &TypeTuple) -> Self {
        let mut monitor = UsageMonitor {
            lifetimes: HashSet::new(),
            type_paths: HashSet::new(),
        };
        monitor.visit_type_tuple(&input);
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
