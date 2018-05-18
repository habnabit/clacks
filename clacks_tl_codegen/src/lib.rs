#![recursion_limit = "128"]

#[cfg(feature = "rustfmt-codegen")]
extern crate rustfmt;

#[macro_use] extern crate derivative;
#[macro_use] extern crate quote;
extern crate pom;
extern crate syn;

pub mod parser {
    use pom::char_class::{alphanum, digit, hex_digit};
    use pom::parser::*;
    use pom::{self, Parser};

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Type {
        Int,
        Flags,
        Named(Vec<String>),
        TypeParameter(String),
        Generic(Vec<String>, Box<Type>),
        Flagged(String, u32, Box<Type>),
        Repeated(Vec<Field>),
    }

    impl Type {
        pub fn names_vec(&self) -> Option<&Vec<String>> {
            use self::Type::*;
            match *self {
                Int |
                Flags |
                TypeParameter(..) |
                Flagged(..) |
                Repeated(..) => None,
                Named(ref v) |
                Generic(ref v, ..) => Some(v),
            }
        }

        pub fn names_vec_mut(&mut self) -> Option<&mut Vec<String>> {
            use self::Type::*;
            match *self {
                Int |
                Flags |
                TypeParameter(..) |
                Flagged(..) |
                Repeated(..) => None,
                Named(ref mut v) |
                Generic(ref mut v, ..) => Some(v),
            }
        }

        pub fn owned_names_vec(&self) -> Vec<String> {
            self.names_vec().cloned().unwrap_or_else(Vec::new)
        }

        pub fn namespaces(&self) -> &[String] {
            self.names_vec()
                .map(|v| &v[..(v.len() - 1).max(0)])
                .unwrap_or(&[])
        }

        pub fn name(&self) -> Option<&str> {
            self.names_vec().and_then(|v| v.last().map(String::as_str))
        }

        pub fn flag_field(&self) -> Option<(&str, u32)> {
            use self::Type::*;
            match self {
                &Flagged(ref f, b, _) => Some((f, b)),
                _ => None,
            }
        }

        pub fn is_type_parameter(&self) -> bool {
            use self::Type::*;
            match self {
                &TypeParameter(..) => true,
                _ => false,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Field {
        pub name: Option<String>,
        pub ty: Type,
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Constructor<Ty, Fi> {
        pub variant: Ty,
        pub tl_id: Option<u32>,
        pub type_parameters: Vec<Fi>,
        pub fields: Vec<Fi>,
        pub output: Ty,
        pub original_variant: String,
        pub original_output: String,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Delimiter {
        Types,
        Functions,
    }

    #[derive(Debug, Clone)]
    pub enum Item {
        Delimiter(Delimiter),
        Constructor(Constructor<Type, Field>),
        Layer(u32),
    }

    #[derive(Debug, Clone)]
    pub struct Matched<T>(pub T, pub String);

    fn utf8(v: Vec<u8>) -> String {
        String::from_utf8(v).unwrap()
    }

    fn ident() -> Parser<u8, String> {
        (is_a(alphanum) | sym(b'_')).repeat(1..).map(utf8)
    }

    fn dotted_ident() -> Parser<u8, Vec<String>> {
        ((ident() - sym(b'.')).repeat(0..) + ident())
            .map(|(mut v, i)| {
                v.push(i);
                v
            })
    }

    fn tl_id() -> Parser<u8, u32> {
        sym(b'#') * is_a(hex_digit).repeat(0..9).convert(|s| u32::from_str_radix(&utf8(s), 16))
    }

    fn decimal() -> Parser<u8, u32> {
        is_a(digit).repeat(0..).convert(|s| utf8(s).parse())
    }

    fn ty_flag() -> Parser<u8, Type> {
        (ident() - sym(b'.') + decimal() - sym(b'?') + call(ty))
            .map(|((name, bit), ty)| Type::Flagged(name, bit, Box::new(ty)))
    }

    fn ty_generic() -> Parser<u8, Type> {
        (dotted_ident() - sym(b'<') + call(ty) - sym(b'>'))
            .map(|(name, ty)| Type::Generic(name, Box::new(ty)))
    }

    fn ty() -> Parser<u8, Type> {
        ( sym(b'#').map(|_| Type::Int) |
          sym(b'!') * ident().map(Type::TypeParameter) |
          ty_flag() |
          ty_generic() |
          dotted_ident().map(Type::Named)
        )
    }

    fn ty_space_generic() -> Parser<u8, Type> {
        let space_generic = dotted_ident() - sym(b' ') + ty();
        (space_generic.map(|(name, ty)| Type::Generic(name, Box::new(ty))) |
         ty())
    }

    fn base_field() -> Parser<u8, Field> {
        (ident() - sym(b':') + ty())
            .map(|(name, ty)| Field { name: Some(name), ty: ty })
            .name("field")
    }

    fn repeated_field() -> Parser<u8, Field> {
        sym(b'[')
            * call(base_fields).map(|fv| Field { name: None, ty: Type::Repeated(fv) })
            - seq(b" ]")
    }

    fn base_field_anonymous_or_repeated() -> Parser<u8, Field> {
        ( repeated_field() |
          base_field() |
          ty().map(|ty| Field { name: None, ty: ty }))
    }

    fn base_fields() -> Parser<u8, Vec<Field>> {
        (sym(b' ') * base_field_anonymous_or_repeated()).repeat(0..)
    }

    fn ty_param_field() -> Parser<u8, Field> {
        sym(b'{') * base_field() - sym(b'}')
    }

    fn fields() -> Parser<u8, (Vec<Field>, Vec<Field>)> {
        (sym(b' ') * ty_param_field()).repeat(0..)
            + base_fields()
    }

    fn output_and_matched<T: 'static>(inner: Parser<u8, T>) -> Parser<u8, Matched<T>> {
        Parser::new(move |input| {
            let start = input.position();
            let output = inner.parse(input)?;
            let end = input.position();
            Ok(Matched(output, utf8(input.segment(start, end))))
        })
    }

    fn constructor() -> Parser<u8, Constructor<Type, Field>> {
        (output_and_matched(dotted_ident()) + tl_id().opt() + fields() - seq(b" = ") + output_and_matched(ty_space_generic()) - sym(b';'))
            .map(|(((variant, tl_id), (type_parameters, fields)), output)| Constructor {
                tl_id, type_parameters, fields,
                original_variant: variant.1,
                variant: Type::Named(variant.0),
                original_output: output.1,
                output: output.0,
            })
            .name("constructor")
    }

    fn delimiter() -> Parser<u8, Delimiter> {
        ( seq(b"---types---").map(|_| Delimiter::Types) |
          seq(b"---functions---").map(|_| Delimiter::Functions)
        )
    }

    fn layer() -> Parser<u8, u32> {
        seq(b"// LAYER ") * decimal()
    }

    fn space() -> Parser<u8, ()> {
        let end_comment = || seq(b"*/");
        ( one_of(b" \n").discard() |
          (seq(b"//") - !(seq(b" LAYER ")) - none_of(b"\n").repeat(0..)).discard() |
          (seq(b"/*") * (!end_comment() * take(1)).repeat(0..) * end_comment()).discard()
        ).repeat(0..).discard()
    }

    fn item() -> Parser<u8, Matched<Item>> {
        output_and_matched({
            delimiter().map(Item::Delimiter) |
            constructor().map(Item::Constructor) |
            layer().map(Item::Layer)
        }) - space()
    }

    fn lines() -> Parser<u8, Vec<Matched<Item>>> {
        space() * item().repeat(0..) - end()
    }

    pub fn parse_string(input: &str) -> Result<Vec<Matched<Item>>, pom::Error> {
        let mut input = pom::DataInput::new(input.as_bytes());
        lines().parse(&mut input)
    }

    fn lowercase() -> Parser<u8, String> {
        is_a(|c: u8| c.is_ascii_lowercase() || c.is_ascii_digit()).repeat(1..).map(utf8)
    }

    fn uppercase() -> Parser<u8, String> {
        is_a(|c: u8| c.is_ascii_uppercase() || c.is_ascii_digit()).repeat(1..).map(utf8)
    }

    fn underscore() -> Parser<u8, ()> {
        sym(b'_').repeat(1..).discard()
    }

    fn mixed_case_ident() -> Parser<u8, Vec<String>> {
        let latter = (
            (underscore() * uppercase()) |
            (underscore() * lowercase()) |
            ((uppercase() + lowercase()) |
             (underscore() * uppercase() + lowercase()))
                .map(|(lhs, rhs)| lhs + &rhs) |
            uppercase()
        ).map(|mut s| {
            s.make_ascii_lowercase();
            s
        });
        (lowercase().opt() + latter.repeat(0..))
            .map(|(c, mut cs)| {
                cs.splice(..0, c);
                cs
            })
    }

    pub fn ident_to_snake_case(input: &str) -> Result<String, pom::Error> {
        let mut input = pom::DataInput::new(input.as_bytes());
        let v = mixed_case_ident().parse(&mut input)?;
        Ok(v.join("_"))
    }

    pub fn ident_to_upper_camel_case(input: &str) -> Result<String, pom::Error> {
        let mut input = pom::DataInput::new(input.as_bytes());
        let v = mixed_case_ident().parse(&mut input)?;
        let mut ret = String::new();
        for mut segment in v {
            ret.push(segment.as_bytes()[0].to_ascii_uppercase() as char);
            ret.push_str(&segment[1..]);
        }
        Ok(ret)
    }
}

pub use parser::{Constructor, Delimiter, Field, Item, Matched, Type};

use std::collections::BTreeMap;
use std::mem;

fn fail_hard() -> quote::Tokens {
    quote!(FAIL_LOUDLY_AT_COMPILE_TIME!())
}

#[derive(Derivative)]
#[derivative(Debug, Default(bound = ""))]
struct Constructors<Ty, Fi>(Vec<Matched<Constructor<Ty, Fi>>>);

type TypeResolutionMap = BTreeMap<Vec<String>, TypeIR>;

#[derive(Debug)]
enum NamespaceItem {
    AsEnum(Constructors<TypeIR, FieldIR>),
    AsVariant(Matched<Constructor<TypeIR, FieldIR>>),
    AsFunction(Matched<Constructor<TypeIR, FieldIR>>),
    AnotherNamespace(Namespace),
}

#[derive(Debug, Default)]
struct Namespace(BTreeMap<syn::Ident, NamespaceItem>);

impl NamespaceItem {
    fn as_tokens(&self, name: &syn::Ident) -> quote::Tokens {
        use self::NamespaceItem::*;
        match *self {
            AsEnum(ref cs) => cs.as_enum(),
            AsVariant(ref cm) => cm.0.as_variant_type_struct(&cm.1),
            AsFunction(ref cm) => cm.0.as_function_struct(&cm.1),
            AnotherNamespace(ref ns) => {
                let inner = ns.as_tokens();
                quote! { pub mod #name { #inner } }
            },
        }
    }
}

impl Namespace {
    fn descend_tree<'this>(&'this mut self, names: &[syn::Ident]) -> &'this mut Self {
        use self::NamespaceItem::*;
        names.iter()
            .fold(self, |ns, name| {
                match {
                    ns.0.entry(name.clone())
                        .or_insert_with(|| AnotherNamespace(Default::default()))
                } {
                    &mut AnotherNamespace(ref mut ns) => ns,
                    _ => panic!("duplicate namespace item {:?} {:?}", names, name),
                }
            })
    }

    fn insert(&mut self, mut names: Vec<syn::Ident>, item: NamespaceItem) {
        let leaf = names.pop().unwrap();
        match self.descend_tree(&names).0.insert(leaf, item) {
            None => (),
            Some(_) => panic!("duplicate namespace item {:?}", names),
        }
    }

    fn as_tokens(&self) -> quote::Tokens {
        let items = self.0.iter().map(|(name, item)| item.as_tokens(name));
        quote!(#( #items )*)
    }

    fn populate_all_constructors<'this>(&'this self, to_populate: &mut Vec<&'this Constructors<TypeIR, FieldIR>>) {
        use self::NamespaceItem::*;
        for item in self.0.values() {
            match *item {
                AsEnum(ref cs) => {
                    to_populate.push(cs);
                },
                AnotherNamespace(ref ns) => {
                    ns.populate_all_constructors(to_populate);
                },
                _ => (),
            };
        }
    }
}

#[derive(Debug)]
struct AllConstructors {
    items: Namespace,
    layer: u32,
}

fn filter_items(iv: &mut Vec<Matched<Item>>) {
    iv.retain(|&Matched(ref i, _)| {
        let c = match i {
            &Item::Constructor(ref c) => c,
            _ => return true,
        };
        // Blacklist some annoying inconsistencies.
        match c.variant.name() {
            Some("true") |
            Some("vector") => false,
            _ => true,
        }
    });
}

impl AllConstructors {
    fn from_matched_items(iv: Vec<Matched<Item>>) -> Self {
        use self::NamespaceItem::*;
        let mut current = Delimiter::Types;
        let mut ret = AllConstructors {
            items: Default::default(),
            layer: 0,
        };
        let mut constructorses: BTreeMap<Vec<String>, Constructors<Type, Field>> = BTreeMap::new();
        let mut functions: Vec<Matched<Constructor<Type, Field>>> = Vec::new();
        for Matched(i, m) in iv {
            match i {
                Item::Delimiter(d) => current = d,
                Item::Constructor(mut c) => {
                    match current {
                        Delimiter::Types => {
                            constructorses.entry(c.output.owned_names_vec())
                                .or_insert_with(Default::default)
                                .0.push(Matched(c, m));
                        },
                        Delimiter::Functions => functions.push(Matched(c, m)),
                    }
                },
                Item::Layer(i) => ret.layer = i,
            }
        }
        let mut resolve_map: TypeResolutionMap = Default::default();
        for (_, mut cs) in constructorses {
            let base_ns = cs.first_constructor().output.namespaces().to_vec();
            cs.trim_common_prefixes(&base_ns, &mut resolve_map);
            let mut cs = cs.resolve(&resolve_map);
            for &Matched(ref c, ref m) in &cs.0 {
                ret.items.insert(
                    c.variant.owned_names_vec(),
                    AsVariant(Matched(c.clone(), m.clone())));
            }
            ret.items.insert(cs.first_constructor().output.owned_names_vec(), AsEnum(cs));
        }
        for Matched(c, m) in functions {
            let c = c.resolve(Delimiter::Functions, &resolve_map);
            let mut ns = c.variant.owned_names_vec();
            ns.insert(0, no_conflict_ident("rpc"));
            ret.items.insert(ns, AsFunction(Matched(c, m)));
        }
        ret
    }

    fn as_dynamic_deserializers(&self) -> quote::Tokens {
        let mut all_constructors = Default::default();
        self.items.populate_all_constructors(&mut all_constructors);
        let dynamic_deserializer = all_constructors.iter()
            .map(|cs| cs.as_dynamic_deserializer());

        quote! {

            lazy_static! {
                pub static ref DYNAMIC_DESERIALIZERS:
                    ::std::collections::BTreeMap<::ConstructorNumber, ::DynamicDeserializer> =
                {
                    let mut ret = ::std::collections::BTreeMap::new();
                    #( #dynamic_deserializer )*
                    ret
                };
            }

        }
    }

    fn as_tokens(&self) -> quote::Tokens {
        let ns_tokens = self.items.as_tokens();
        let dynamic_deserializers = self.as_dynamic_deserializers();
        quote! {

            #ns_tokens
            #dynamic_deserializers

        }
    }
}

fn no_conflict_ident(s: &str) -> syn::Ident {
    let mut candidate: String = s.into();
    loop {
        match syn::parse_str(&candidate) {
            Ok(i) => return i,
            Err(_) => candidate.push('_'),
        }
    }
}

fn no_conflict_local_ident(s: &str) -> Option<syn::Ident> {
    match s {
        "bytes" => Some("bytes_".into()),
        _ => None,
    }
}

fn wrap_option_type(wrap: bool, ty: quote::Tokens) -> quote::Tokens {
    if wrap {
        quote! { Option<#ty> }
    } else {
        ty
    }
}

fn wrap_option_value(wrap: bool, ty: quote::Tokens) -> quote::Tokens {
    if wrap {
        quote! { Some(#ty) }
    } else {
        ty
    }
}

fn lift_option_value(value: &Option<quote::Tokens>) -> quote::Tokens {
    match value {
        &Some(ref t) => quote!(Some(#t)),
        &None => quote!(None),
    }
}

#[derive(Debug, Clone)]
struct TypeName {
    tokens: quote::Tokens,
    idents: Option<Vec<syn::Ident>>,
}

impl TypeName {
    fn transformed_tokens<F>(&self, func: F) -> quote::Tokens
        where F: FnOnce(&quote::Tokens) -> quote::Tokens,
    {
        func(&self.tokens)
    }

    fn transformed<F>(&self, func: F) -> Self
        where F: FnOnce(&quote::Tokens) -> quote::Tokens,
    {
        let tokens = self.transformed_tokens(func);
        TypeName { tokens, idents: None }
    }
}

impl<S> ::std::iter::FromIterator<S> for TypeName
    where S: AsRef<str>
{
    fn from_iter<T>(iter: T) -> Self
        where T: IntoIterator<Item = S>,
    {
        let mut tokens = quote!(::mtproto);
        let mut idents = vec![];
        for segment in iter {
            let segment = no_conflict_ident(segment.as_ref());
            tokens = quote!(#tokens::#segment);
            idents.push(segment);
        }
        TypeName { tokens, idents: Some(idents) }
    }
}

#[derive(Debug, Clone)]
enum WireKind {
    Bare(TypeName),
    Boxed(TypeName),
    TypeParameter(syn::Ident),
    FlaggedTrue,
    Flags,
}

fn is_first_char_lowercase(s: &str) -> bool {
    s.chars().next()
        .map(char::is_lowercase)
        .unwrap_or(false)
}

impl WireKind {
    fn from_names_and_hint(names: &[String], force_bare: bool) -> Self {
        use self::WireKind::*;
        match names.last().map(String::as_str) {
            Some("true") => FlaggedTrue,
            Some(s) if force_bare || is_first_char_lowercase(s) =>
                Bare(names.iter().map(String::as_str).collect()),
            Some(_) =>
                Boxed(names.iter().map(String::as_str).collect()),
            None => unimplemented!(),
        }
    }

    fn type_parameter(id: syn::Ident) -> Self {
        WireKind::TypeParameter(id)
    }

    fn become_container_for(&mut self, include_determiner: bool, contained: Self) {
        use self::WireKind::*;
        let ty_loc = match *self {
            Bare(ref mut t) |
            Boxed(ref mut t) => t,
            _ => unimplemented!(),
        };
        let contained = if include_determiner {
            match contained {
                Bare(ty) => ty.transformed_tokens(|t| quote!(::mtproto::Bare, #t)),
                Boxed(ty) => ty.transformed_tokens(|t| quote!(::mtproto::Boxed, #t)),
                TypeParameter(t) => quote!(::mtproto::Boxed, #t),
                _ => unimplemented!(),
            }
        } else {
            match contained {
                Bare(t) | Boxed(t) => t.tokens,
                TypeParameter(t) => quote!(#t),
                _ => unimplemented!(),
            }
        };
        *ty_loc = ty_loc.transformed(|ty| quote!(#ty<#contained>));
    }

    fn as_read_method(&self) -> quote::Tokens {
        use self::WireKind::*;
        match *self {
            Bare(..) | Flags => quote!(read_bare),
            Boxed(..) | TypeParameter(..) => quote!(read_boxed),
            FlaggedTrue => fail_hard(),
        }
    }

    fn as_write_method(&self) -> quote::Tokens {
        use self::WireKind::*;
        match *self {
            Bare(..) | Flags => quote!(write_bare),
            Boxed(..) | TypeParameter(..) => quote!(write_boxed),
            FlaggedTrue => fail_hard(),
        }
    }

    fn is_unit(&self) -> bool {
        use self::WireKind::*;
        match *self {
            FlaggedTrue => true,
            _ => false,
        }
    }

    fn is_flags(&self) -> bool {
        use self::WireKind::*;
        match *self {
            Flags => true,
            _ => false,
        }
    }

    fn is_type_parameter(&self) -> bool {
        use self::WireKind::*;
        match *self {
            TypeParameter(..) => true,
            _ => false,
        }
    }

    fn opt_names_slice(&self) -> Option<&[syn::Ident]> {
        use self::WireKind::*;
        match *self {
            Bare(ref t) |
            Boxed(ref t) => t.idents.as_ref().map(|v| v.as_slice()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct TypeIR {
    wire_kind: WireKind,
    needs_box: bool,
    needs_determiner: bool,
    with_option: bool,
}

impl TypeIR {
    fn from_names(names: &[String]) -> Self {
        Self::from_names_and_hint(names, false)
    }

    fn from_names_and_hint(names: &[String], force_bare: bool) -> Self {
        let wire_kind = WireKind::from_names_and_hint(names, force_bare);
        TypeIR {
            wire_kind, with_option: false,
            needs_box: match names.last().map(String::as_str) {
                // Special case two recursive types.
                Some("PageBlock") |
                Some("RichText") => true,
                _ => false,
            },
            needs_determiner: match names.last().map(String::as_str) {
                Some("vector") |
                Some("Vector") => true,
                _ => false,
            },
        }
    }

    fn bytes() -> Self {
        TypeIR {
            wire_kind: WireKind::Bare(["bytes"].iter().collect()),
            needs_box: false,
            needs_determiner: false,
            with_option: false,
        }
    }

    fn int() -> Self {
        TypeIR {
            wire_kind: WireKind::Bare(["int"].iter().collect()),
            needs_box: false,
            needs_determiner: false,
            with_option: false,
        }
    }

    fn type_parameter(id: syn::Ident) -> Self {
        TypeIR {
            wire_kind: WireKind::type_parameter(id),
            needs_box: false,
            needs_determiner: false,
            with_option: false,
        }
    }

    fn flags() -> Self {
        TypeIR {
            wire_kind: WireKind::Flags,
            needs_box: false,
            needs_determiner: false,
            with_option: false,
        }
    }

    fn with_container(self, mut container: TypeIR) -> Self {
        container.wire_kind.become_container_for(container.needs_determiner, self.wire_kind);
        container
    }

    fn with_option_wrapper(mut self) -> Self {
        self.with_option = true;
        self
    }

    fn io_turbofish(&self) -> quote::Tokens {
        use self::WireKind::*;
        let mut ty = match self.wire_kind {
            Flags => quote!(::mtproto::Flags),
            _ => self.non_field_type(),
        };
        if self.needs_box {
            ty = quote!(Box<#ty>);
        }
        quote!(::<#ty>)
    }

    fn assemble_method(&self, method: quote::Tokens) -> quote::Tokens {
        let turbofish = self.io_turbofish();
        quote!(#method #turbofish)
    }

    fn as_read_method(&self) -> quote::Tokens {
        self.assemble_method(self.wire_kind.as_read_method())
    }

    fn as_write_method(&self) -> quote::Tokens {
        self.assemble_method(self.wire_kind.as_write_method())
    }

    fn non_field_type(&self) -> quote::Tokens {
        use self::WireKind::*;
        match self.wire_kind {
            Bare(ref t) | Boxed(ref t) => t.tokens.clone(),
            TypeParameter(ref t) => quote!(#t),
            _ => fail_hard(),
        }
    }

    fn unboxed(&self) -> quote::Tokens {
        wrap_option_type(self.with_option, self.non_field_type())
    }

    fn boxed(&self) -> quote::Tokens {
        let mut ty = self.non_field_type();
        if self.needs_box {
            ty = quote!(Box<#ty>);
        }
        wrap_option_type(self.with_option, ty)
    }

    fn field_type(&self) -> quote::Tokens {
        use self::WireKind::*;
        match self.wire_kind {
            FlaggedTrue if self.with_option => quote!(bool),
            _ => self.boxed(),
        }
    }

    fn ref_prefix(&self) -> quote::Tokens {
        if self.is_unit() {quote!()} else {quote!(ref)}
    }

    fn reference_prefix(&self) -> quote::Tokens {
        if self.is_unit() {quote!()} else {quote!(&)}
    }

    fn local_reference_prefix(&self) -> quote::Tokens {
        if self.is_unit() {quote!(&)} else {quote!()}
    }

    fn is_defined_trailer(&self) -> quote::Tokens {
        use self::WireKind::*;
        match self.wire_kind {
            _ if !self.with_option => quote!(FAIL_LOUDLY_AT_COMPILE_TIME),
            FlaggedTrue => quote!(),
            _ => quote!(.is_some()),
        }
    }

    fn is_unit(&self) -> bool {
        self.wire_kind.is_unit()
    }

    fn is_flags(&self) -> bool {
        self.wire_kind.is_flags()
    }

    fn is_type_parameter(&self) -> bool {
        self.wire_kind.is_type_parameter()
    }

    fn owned_names_vec(&self) -> Vec<syn::Ident> {
        self.wire_kind.opt_names_slice()
            .unwrap()
            .iter()
            .cloned()
            .collect()
    }

    fn name(&self) -> syn::Ident {
        self.wire_kind.opt_names_slice()
            .and_then(|s| s.last())
            .unwrap()
            .clone()
    }
}

#[derive(Debug, Clone)]
struct FieldIR {
    name: String,
    ty: TypeIR,
    flag_bit: Option<u32>,
}

impl Field {
    fn resolved(&self, resolve_map: &TypeResolutionMap, replace_string_with_bytes: bool) -> FieldIR {
        let name = self.name.clone().unwrap();
        let ty = if replace_string_with_bytes && self.ty.name() == Some("string") {
            TypeIR::bytes()
        } else {
            self.ty.resolved(resolve_map, &name == "flags")
        };
        let flag_bit = self.ty.flag_field().map(|(_, b)| b);
        FieldIR { ty, name, flag_bit }
    }
}

impl FieldIR {
    fn name(&self) -> syn::Ident {
        no_conflict_ident(&self.name)
    }

    fn local_name(&self) -> Option<syn::Ident> {
        no_conflict_local_ident(&self.name)
    }

    fn as_field(&self) -> quote::Tokens {
        let name = self.name();
        let ty = self.ty.field_type();

        quote! {
            #name: #ty
        }
    }
}

impl Type {
    fn resolved(&self, resolve_map: &TypeResolutionMap, is_flag_field: bool) -> TypeIR {
        use Type::*;
        match *self {
            Named(ref names) => {
                match resolve_map.get(names) {
                    Some(ir) => return ir.clone(),
                    None => TypeIR::from_names(names),
                }
            },
            TypeParameter(ref name) => TypeIR::type_parameter(no_conflict_ident(name)),
            Generic(ref container, ref ty) => {
                let ty = ty.resolved(resolve_map, false);
                let container = match resolve_map.get(container) {
                    Some(ir) => ir.clone(),
                    None => TypeIR::from_names(container),
                };
                ty.with_container(container)
            },
            Flagged(_, _, ref ty) => {
                ty.resolved(resolve_map, false).with_option_wrapper()
            },
            Flags => TypeIR::flags(),
            Int if is_flag_field => TypeIR::flags(),
            Int => TypeIR::int(),
            Repeated(..) => unimplemented!(),
        }
    }
}

impl Constructor<Type, Field> {
    fn resolve(self, which: Delimiter, resolve_map: &TypeResolutionMap) -> Constructor<TypeIR, FieldIR> {
        Constructor {
            variant: self.variant.resolved(resolve_map, false),
            fields: self.resolved_fields(resolve_map),
            output: self.resolved_output(which, resolve_map),
            type_parameters: self.type_parameters.iter()
                .map(|t| t.resolved(resolve_map, false))
                .collect(),
            tl_id: self.tl_id,
            original_variant: self.original_variant,
            original_output: self.original_output,
        }
    }

    fn resolved_output(&self, which: Delimiter, resolve_map: &TypeResolutionMap) -> TypeIR {
        if which == Delimiter::Functions && self.is_output_a_type_parameter() {
            TypeIR::type_parameter(no_conflict_ident(self.output.name().unwrap()))
        } else {
            self.output.resolved(resolve_map, false)
        }
    }

    fn is_output_a_type_parameter(&self) -> bool {
        let output_name = match &self.output {
            &Type::Named(ref v) if v.len() == 1 => v[0].as_str(),
            _ => return false,
        };
        for p in &self.type_parameters {
            if p.name.as_ref().map(String::as_str) == Some(output_name) {
                return true;
            }
        }
        false
    }

    fn resolved_fields(&self, resolve_map: &TypeResolutionMap) -> Vec<FieldIR> {
        let replace_string_with_bytes = match self.original_variant.as_str() {
            // types
            "resPQ" |
            "p_q_inner_data" |
            "p_q_inner_data_temp" |
            "server_DH_params_ok" |
            "server_DH_inner_data" |
            "client_DH_inner_data" |
            // functions
            "req_DH_params" |
            "set_client_DH_params" => true,
            _ => false,
        };

        self.fields.iter()
            .map(|f| f.resolved(resolve_map, replace_string_with_bytes))
            .collect()
    }
}

impl Constructor<TypeIR, FieldIR> {
    fn fields_tokens(&self, pub_: quote::Tokens, trailer: quote::Tokens) -> quote::Tokens {
        let pub_ = std::iter::repeat(pub_);
        if self.fields.is_empty() {
            quote! { #trailer }
        } else {
            let fields = self.fields.iter()
                .filter(|f| !f.ty.is_flags())
                .map(FieldIR::as_field);
            quote! {
                { #( #pub_ #fields , )* }
            }
        }
    }

    fn as_variant_empty_pattern(&self) -> quote::Tokens {
        let name = self.variant_name();
        if self.fields.is_empty() {
            quote!(#name)
        } else {
            quote!(#name(..))
        }
    }

    fn generics(&self) -> quote::Tokens {
        if self.type_parameters.is_empty() {
            return quote!();
        }
        let tys = self.type_parameters.iter().map(FieldIR::name);
        quote! { <#(#tys),*> }
    }

    fn rpc_generics(&self) -> quote::Tokens {
        self.type_generics(&quote!(::Function))
    }

    fn type_generics(&self, trait_: &quote::Tokens) -> quote::Tokens {
        if self.type_parameters.is_empty() {
            return quote!();
        }
        let tys = self.type_parameters.iter().map(FieldIR::name);
        let traits = std::iter::repeat(trait_);
        quote! { <#(#tys: #traits),*> }
    }

    fn serialize_generics(&self) -> quote::Tokens {
        if self.type_parameters.is_empty() {
            return quote!();
        }
        let tys = self.type_parameters.iter().map(FieldIR::name);
        quote! { <#(#tys: ::AnyBoxedSerialize),*> }
    }

    fn as_struct_determine_flags(&self, field_prefix: quote::Tokens) -> Option<quote::Tokens> {
        match self.fields.iter().filter(|f| f.ty.is_flags()).count() {
            0 => return None,
            1 => (),
            n => panic!("{} flags fields found on {:?}", n, self),
        }
        let determination = {
            let fields = self.fields.iter()
                .filter_map(|f| {
                    let name = f.name();
                    f.flag_bit.map(|bit| {
                        let is_defined = f.ty.is_defined_trailer();
                        quote! {
                            if #field_prefix #name #is_defined {
                                _flags |= 1 << #bit;
                            }
                        }
                    })
                });
            quote! {
                let mut _flags = 0i32;
                #( #fields )*
            }
        };
        Some(determination)
    }

    fn as_struct_doc(&self, matched: &str) -> String {
        format!("TL-derived from `{}`\n\n```text\n{}\n```\n", self.original_variant, matched)
    }

    fn as_struct_base(&self, name: &syn::Ident, matched: &str) -> quote::Tokens {
        let doc = self.as_struct_doc(matched);
        let generics = self.generics();
        let fields = self.fields_tokens(quote! {pub}, quote! {;});
        quote! {
            #[derive(Debug, Clone, Serialize)]
            #[doc = #doc]
            pub struct #name #generics #fields
        }
    }

    fn as_struct_deserialize(&self) -> (bool, quote::Tokens) {
        if self.fields.is_empty() {
            return (false, quote!());
        }
        let mut flags_to_read = vec![];
        let mut has_flags = false;
        let constructor = {
            let fields = self.fields.iter()
                .filter_map(|f| {
                    let name = f.name();
                    let read_method = f.ty.as_read_method();
                    let mut expr = if f.ty.is_flags() {
                        has_flags = true;
                        flags_to_read.push(quote!(_flags = _de. #read_method ()?;));
                        return None;
                    } else if let Some(bit) = f.flag_bit {
                        let predicate = quote!(_flags & (1 << #bit) != 0);
                        if f.ty.is_unit() {
                            predicate
                        } else {
                            quote! {
                                if #predicate {
                                    Some(_de. #read_method ()?)
                                } else {
                                    None
                                }
                            }
                        }
                    } else {
                        quote!(_de. #read_method ()?)
                    };
                    if !flags_to_read.is_empty() {
                        let read_flags = mem::replace(&mut flags_to_read, vec![]);
                        expr = quote!({
                            #( #read_flags )*
                            #expr
                        })
                    }
                    Some(quote!(#name: #expr))
                });
            quote!({ #( #fields, )* })
        };
        (has_flags, constructor)
    }

    fn as_into_boxed(&self, name: &syn::Ident) -> Option<quote::Tokens> {
        if self.tl_id().is_none() {
            return None;
        }
        let constructor = self.output.unboxed();
        let variant_name = self.variant_name();
        Some(quote! {
            impl ::IntoBoxed for #name {
                type Boxed = #constructor;
                fn into_boxed(self) -> #constructor {
                    #constructor::#variant_name(self)
                }
            }
        })
    }

    fn as_type_struct_base(&self, name: syn::Ident, matched: &str) -> quote::Tokens {
        let serialize_destructure = self.as_variant_ref_destructure(&name)
            .map(|d| quote! { let &#d = self; })
            .unwrap_or_else(|| quote!());
        let serialize_stmts = self.as_variant_serialize();
        let (has_flags, deserialize) = self.as_struct_deserialize();
        let flag_init = if has_flags {
            quote! { let _flags: i32; }
        } else {
            quote!()
        };
        let type_impl = self.as_type_impl(
            &name,
            quote!(#serialize_destructure #serialize_stmts Ok(())),
            Some(quote! {
                #flag_init
                Ok(#name #deserialize)
            }));
        let struct_block = self.as_struct_base(&name, matched);
        let into_boxed = self.as_into_boxed(&name);
        quote! {
            #struct_block
            #type_impl
            #into_boxed
        }
    }

    fn variant_name(&self) -> syn::Ident {
        self.variant.name()
    }

    fn as_variant_type_struct(&self, matched: &str) -> quote::Tokens {
        if self.fields.is_empty() {
            quote!()
        } else {
            self.as_type_struct_base(self.variant_name(), matched)
        }
    }

    fn as_variant_ref_destructure(&self, name: &syn::Ident) -> Option<quote::Tokens> {
        if self.fields.is_empty() {
            return None;
        }
        let fields = self.fields.iter()
            .filter(|f| !f.ty.is_flags())
            .map(|f| {
                let prefix = f.ty.ref_prefix();
                let field_name = f.name();
                if let Some(local_name) = f.local_name() {
                    quote! { #field_name: #prefix #local_name }
                } else {
                    quote! { #prefix #field_name }
                }
            });
        Some(quote! {
            #name { #( #fields ),* }
        })
    }

    fn as_variant_serialize(&self) -> quote::Tokens {
        let determine_flags = self.as_struct_determine_flags(quote!())
            .unwrap_or_else(|| quote!());
        let fields = self.fields.iter()
            .map(|f| {
                if f.ty.is_unit() {
                    return quote!();
                }
                let field_name = f.name();
                let local_name = f.local_name().unwrap_or_else(|| field_name.clone());
                let write_method = f.ty.as_write_method();
                if f.ty.is_flags() {
                    quote! { _ser. #write_method (&_flags)?; }
                } else if f.flag_bit.is_some() {
                    let outer_ref = f.ty.reference_prefix();
                    let inner_ref = f.ty.ref_prefix();
                    let local_ref = f.ty.local_reference_prefix();
                    quote! {
                        if let #outer_ref Some(#inner_ref inner) = #local_name {
                            _ser. #write_method (#local_ref inner)?;
                        }
                    }
                } else {
                    let prefix = f.ty.local_reference_prefix();
                    quote! { _ser. #write_method (#prefix #local_name)?; }
                }
            });
        quote! {
            #determine_flags
            #( #fields )*
        }
    }

    fn as_function_struct(&self, matched: &str) -> quote::Tokens {
        let name = self.variant_name();
        let tl_id = self.tl_id().unwrap();
        let rpc_generics = self.rpc_generics();
        let serialize_generics = self.serialize_generics();
        let generics = self.generics();
        let struct_block = self.as_struct_base(&name, matched);
        let mut output_ty = self.output.boxed();
        if self.output.is_type_parameter() {
            output_ty = quote! {#output_ty::Reply};
        }
        let serialize_destructure = self.as_variant_ref_destructure(&name)
            .map(|d| quote! { let &#d = self; })
            .unwrap_or_else(|| quote!());
        let serialize_stmts = self.as_variant_serialize();
        let type_impl = self.as_type_impl(
            &name,
            quote!(#serialize_destructure #serialize_stmts Ok(())),
            None);

        quote! {
            #struct_block

            impl #serialize_generics ::BoxedSerialize for #name #generics {
                fn serialize_boxed<'this>(&'this self) -> (::ConstructorNumber, &'this ::BareSerialize) {
                    (#tl_id, self)
                }
            }

            impl #rpc_generics ::Function for #name #generics {
                type Reply = #output_ty;
            }

            #type_impl
        }
    }

    fn as_variant(&self) -> quote::Tokens {
        let name = self.variant_name();
        if self.fields.is_empty() {
            quote!(#name)
        } else {
            let type_name = self.variant.unboxed();
            quote!(#name(#type_name))
        }
    }

    fn as_variant_serialize_arm(&self) -> quote::Tokens {
        let tl_id = self.tl_id().unwrap();
        if self.fields.is_empty() {
            quote!(=> (#tl_id, &()))
        } else {
            quote!((ref x) => (#tl_id, x))
        }
    }

    fn as_variant_deserialize(&self) -> quote::Tokens {
        if self.fields.is_empty() {
            quote!()
        } else {
            let read_method = self.variant.as_read_method();
            quote!((_de. #read_method ()?))
        }
    }

    fn as_variant_generic_type(&self) -> TypeIR {
        let variant = self.variant.clone();
        match self.type_parameters.first() {
            Some(param) => {
                assert!(self.type_parameters.len() == 1);
                TypeIR::type_parameter(param.name()).with_container(variant)
            },
            None => variant,
        }
    }

    fn tl_id(&self) -> Option<quote::Tokens> {
        self.tl_id.as_ref().map(|tl_id| {
            let tl_id: syn::LitInt = syn::parse_str(&format!("0x{:08x}", tl_id)).unwrap();
            quote!(::ConstructorNumber(#tl_id))
        })
    }

    fn as_type_impl(&self, name: &syn::Ident, serialize: quote::Tokens, deserialize: Option<quote::Tokens>) -> quote::Tokens {
        let serialize_generics = self.serialize_generics();
        let ty = self.as_variant_generic_type();
        let generics = self.generics();

        let deserialize = deserialize.map(|body| {
            let bare_deserialize_generics = self.type_generics(&quote!(::BareDeserialize));
            quote! {
                impl #bare_deserialize_generics ::BareDeserialize for #name #generics {
                    fn deserialize_bare(_de: &mut ::Deserializer) -> ::Result<Self> {
                        #body
                    }
                }
            }
        }).unwrap_or_else(|| quote!());

        quote! {

            impl #serialize_generics ::BareSerialize for #name #generics {
                fn serialize_bare(&self, _ser: &mut ::Serializer) -> ::Result<()> {
                    #serialize
                }
            }

            #deserialize
        }
    }
}

fn common_prefix_of<'a>(a: Option<&'a str>, b: &'a str) -> &'a str {
    let a = match a {
        None => return b,
        Some(s) => s,
    };
    let ret = a.char_indices()
        .zip(b.char_indices())
        .skip_while(|&((_, c_a), (_, c_b))| c_a == c_b)
        .next()
        .map(|((e_a, _), (e_b, _))| {
            assert_eq!(e_a, e_b);
            &b[..e_a]
        })
        .unwrap_or(b);
    let min_len = a.len().min(b.len()).min(ret.len());
    &ret[..min_len]
}

impl<Ty, Fi> Constructors<Ty, Fi> {
    fn first_constructor(&self) -> &Constructor<Ty, Fi> {
        &self.0[0].0
    }
}

impl Constructors<Type, Field> {
    fn resolve(self, resolve_map: &TypeResolutionMap) -> Constructors<TypeIR, FieldIR> {
        Constructors({
            self.0.into_iter()
                .map(|Matched(c, m)| Matched(c.resolve(Delimiter::Types, resolve_map), m))
                .collect()
        })
    }

    fn trim_common_prefixes(&mut self, base_ns: &[String], resolve_map: &mut TypeResolutionMap) {
        let common_prefix = match {
            self.0.iter()
                .filter_map(|m| m.0.variant.name())
                .fold(None, |a, b| Some(common_prefix_of(a, b)))
        } {
            None | Some("") => return,
            Some(s) => s.to_string(),
        };
        let common_module = &common_prefix;
        for &mut Matched(ref mut c, _) in &mut self.0 {

            if let Some(variant) = c.variant.names_vec_mut() {
                if !variant.starts_with(&base_ns) {
                    variant.splice(..0, base_ns.iter().cloned());
                }
            }

            let names = match c.variant.names_vec_mut() {
                None => continue,
                Some(v) => v,
            };
            let fixup_key = names.clone();
            if !names.starts_with(base_ns) {
                names.splice(..0, base_ns.iter().cloned());
            }
            let last = names.pop().unwrap();
            let was_bare = is_first_char_lowercase(&last);
            let last = match &last[common_prefix.len()..] {
                "" => c.output.name().unwrap(),
                s => s,
            }.to_string();
            names.push(common_module.to_string());
            names.push(last);
            let type_ir = TypeIR::from_names_and_hint(&names, was_bare);
            resolve_map.insert(fixup_key, type_ir.clone());
            resolve_map.insert(names.clone(), type_ir);
        }
    }

}

impl Constructors<TypeIR, FieldIR> {
    // fn coalesce_methods(&self) -> BTreeMap<&str, BTreeMap<&Type, BTreeSet<&Constructor<TypeIR>>>> {
    //     let mut map: BTreeMap<&str, BTreeMap<&Type, BTreeSet<&Constructor<TypeIR>>>> = BTreeMap::new();
    //     for cons in &self.0 {
    //         for field in cons.non_flag_fields() {
    //             let name = match field.name.as_ref() {
    //                 Some(s) => s.as_str(),
    //                 None => continue,
    //             };
    //             map.entry(name)
    //                 .or_insert_with(Default::default)
    //                 .entry(&field.ty)
    //                 .or_insert_with(Default::default)
    //                 .insert(cons);
    //         }
    //     }
    //     map
    // }

    fn determine_methods(&self, enum_name: &syn::Ident) -> quote::Tokens {
        quote!()

        // let all_constructors = self.0.len();
        // let mut methods = vec![];
        // for (name, typemap) in self.coalesce_methods() {
        //     if typemap.len() != 1 {
        //         continue;
        //     }
        //     let (output_ty, constructors) = typemap.into_iter().next().unwrap();
        //     if constructors.len() <= 1 {
        //         continue;
        //     }
        //     let name = no_conflict_ident(name);
        //     let mut ty_ir = output_ty.as_type();
        //     let field_is_option = ty_ir.needs_option();
        //     let exhaustive = constructors.len() == all_constructors;
        //     if !exhaustive {
        //         ty_ir.with_option = true;
        //     }
        //     let force_option = !exhaustive && ty_ir.is_unit();
        //     let value = if field_is_option && !ty_ir.is_copy {
        //         quote!(x.#name.as_ref())
        //     } else {
        //         let ref_ = ty_ir.reference_prefix();
        //         wrap_option_value((ty_ir.needs_option() && !field_is_option) || force_option, quote!(#ref_ x.#name))
        //     };
        //     let constructors = constructors.into_iter()
        //         .map(|c| {
        //             let cons_name = c.variant_name();
        //             quote!(&#enum_name::#cons_name(ref x) => #value)
        //         });
        //     let trailer = if exhaustive {
        //         quote!()
        //     } else {
        //         quote!(_ => None)
        //     };
        //     let ty = wrap_option_type(force_option, ty_ir.ref_type());
        //     methods.push(quote! {
        //         pub fn #name(&self) -> #ty {
        //             match self {
        //                 #( #constructors, )*
        //                 #trailer
        //             }
        //         }
        //     });
        // }

        // if methods.is_empty() {
        //     quote!()
        // } else {
        //     quote! {
        //         impl #enum_name {
        //             #( #methods )*
        //         }
        //     }
        // }
    }

    fn as_type_impl(&self, name: &syn::Ident, type_id: quote::Tokens, serialize: quote::Tokens, deserialize: quote::Tokens) -> quote::Tokens {
        let tl_ids = self.as_tl_ids();

        quote! {

            impl ::BoxedSerialize for #name {
                fn serialize_boxed<'this>(&'this self) -> (::ConstructorNumber, &'this ::BareSerialize) {
                    #serialize
                }
            }

            impl ::BoxedDeserialize for #name {
                fn possible_constructors() -> Vec<::ConstructorNumber> { vec![#tl_ids] }
                fn deserialize_boxed(_id: ::ConstructorNumber, _de: &mut ::Deserializer) -> ::Result<Self> {
                    #deserialize
                }
            }

        }
    }

    fn constructors_and_tl_ids<'this>(&'this self) -> Box<'this + Iterator<Item = (quote::Tokens, &'this Constructor<TypeIR, FieldIR>)>> {
        Box::new(self.0.iter().filter_map(|cm| {
            cm.0.tl_id().map(|id| (id, &cm.0))
        }))
    }

    fn as_type_id_match(&self, enum_name: &syn::Ident) -> quote::Tokens {
        let constructors = self.constructors_and_tl_ids()
            .map(|(tl_id, c)| {
                let pat = c.as_variant_empty_pattern();
                quote!(&#enum_name::#pat => #tl_id)
            });
        quote! {
            match self {
                #( #constructors, )*
            }
        }
    }

    fn as_serialize_match(&self, enum_name: &syn::Ident) -> quote::Tokens {
        let constructors = self.0.iter()
            .map(|&Matched(ref c, _)| {
                let variant_name = c.variant_name();
                let serialize = c.as_variant_serialize_arm();
                quote!(&#enum_name::#variant_name #serialize)
            });
        quote! {
            match self {
                #( #constructors, )*
            }
        }
    }

    fn as_tl_ids(&self) -> quote::Tokens {
        let tl_ids = self.0.iter()
            .filter_map(|cm| cm.0.tl_id());
        quote!(#( #tl_ids, )*)
    }

    fn as_dynamic_deserializer(&self) -> quote::Tokens {
        let constructors = self.constructors_and_tl_ids()
            .map(|(tl_id, c)| {
                let ty = c.output.unboxed();
                quote!(ret.insert(#tl_id, <#ty as ::BoxedDeserializeDynamic>::boxed_deserialize_to_box as ::DynamicDeserializer))
            });
        quote! {
            #( #constructors; )*
        }
    }

    fn as_deserialize_match(&self, enum_name: &syn::Ident) -> quote::Tokens {
        let constructors = self.constructors_and_tl_ids()
            .map(|(tl_id, c)| {
                let variant_name = c.variant_name();
                let deserialize = c.as_variant_deserialize();
                quote!(#tl_id => Ok(#enum_name::#variant_name #deserialize))
            });
        quote! {
            match _id {
                #( #constructors, )*
                id => Err(::error::ErrorKind::InvalidType(Self::possible_constructors(), id).into()),
            }
        }
    }

    fn as_enum_doc(&self) -> String {
        use std::fmt::Write;
        let mut ret = format!("TL-derived from `{}`\n\n```text\n", &self.first_constructor().original_output);
        for (e, cm) in self.0.iter().enumerate() {
            if e != 0 {
                ret.write_str("\n\n").unwrap();
            }
            ret.write_str(&cm.1).unwrap();
        }
        write!(ret, "\n```\n").unwrap();
        ret
    }

    fn as_enum(&self) -> quote::Tokens {
        if self.0.iter().all(|cm| cm.0.tl_id().is_none()) {
            return quote!();
        }
        let name = self.first_constructor().output.name();
        let doc = self.as_enum_doc();
        let variants = self.0.iter()
            .map(|cm| cm.0.as_variant());
        let methods = self.determine_methods(&name);
        let type_impl = self.as_type_impl(
            &name,
            self.as_type_id_match(&name),
            self.as_serialize_match(&name),
            self.as_deserialize_match(&name));

        quote! {
            #[derive(Debug, Clone, Serialize)]
            #[doc = #doc]
            pub enum #name {
                #( #variants , )*
            }
            #methods
            #type_impl
        }
    }
}

#[cfg(feature = "rustfmt-codegen")]
fn reformat(source: String) -> std::io::Result<String> {
    let mut config: rustfmt::config::Config = Default::default();
    {
        let mut set = config.set();
        set.error_on_line_overflow(false);
        set.array_width(200); 
        set.fn_call_width(200);
        set.max_width(200);
        set.struct_lit_width(200);
        set.struct_variant_width(200);
    }
    let outputs = match rustfmt::format_input::<std::io::Sink>(rustfmt::Input::Text(source), &config, None) {
        Ok((_, outputs, _)) => outputs,
        Err((e, _)) => return Err(e),
    };
    let (_, string_buf) = outputs.into_iter()
        .filter(|&(ref name, _)| name == "stdin")
        .next()
        .unwrap();
    Ok(format!("{}", string_buf))
}

#[cfg(not(feature = "rustfmt-codegen"))]
fn reformat(source: String) -> std::io::Result<String> {
    Ok(source)
}

pub fn generate_code_for(input: &str) -> std::io::Result<String> {
    let constructors = {
        let mut items = parser::parse_string(input).unwrap();
        filter_items(&mut items);
        AllConstructors::from_matched_items(items)
    };

    let layer = constructors.layer as i32;
    let prelude = quote! {
        #![allow(non_camel_case_types, non_snake_case)]
        pub use mtproto_prelude::*;

        pub const LAYER: i32 = #layer;
    };

    let items = constructors.as_tokens();

    reformat(quote!(
        #prelude
        #items
    ).to_string())
}
