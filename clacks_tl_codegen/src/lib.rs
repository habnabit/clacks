#![recursion_limit = "128"]

extern crate pom;
#[macro_use]
extern crate quote;
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
            match self {
                &Int |
                &Flags |
                &TypeParameter(..) |
                &Flagged(..) |
                &Repeated(..) => None,
                &Named(ref v) |
                &Generic(ref v, ..) => Some(v),
            }
        }

        pub fn namespace(&self) -> Option<&str> {
            self.names_vec().and_then(|v| {
                match v.len() {
                    1 => None,
                    2 => v.first().map(String::as_str),
                    _ => unimplemented!(),
                }
            })
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
    pub struct Constructor {
        pub variant: Type,
        pub tl_id: Option<u32>,
        pub type_parameters: Vec<Field>,
        pub fields: Vec<Field>,
        pub output: Type,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Delimiter {
        Types,
        Functions,
    }

    #[derive(Debug, Clone)]
    pub enum Item {
        Delimiter(Delimiter),
        Constructor(Constructor),
        Layer(u32),
    }

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

    fn constructor() -> Parser<u8, Constructor> {
        (dotted_ident() + tl_id().opt() + fields() - seq(b" = ") + ty_space_generic() - sym(b';'))
            .map(|(((variant, tl_id), (type_parameters, fields)), output)| Constructor {
                variant: Type::Named(variant),
                tl_id: tl_id,
                type_parameters: type_parameters,
                fields: fields,
                output: output,
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

    fn item() -> Parser<u8, Item> {
        ( delimiter().map(Item::Delimiter) |
          constructor().map(Item::Constructor) |
          layer().map(Item::Layer)
        ) - space()
    }

    fn lines() -> Parser<u8, Vec<Item>> {
        space() * item().repeat(0..) - end()
    }

    pub fn parse_string(input: &str) -> Result<Vec<Item>, pom::Error> {
        let mut input = pom::DataInput::new(input.as_bytes());
        lines().parse(&mut input)
    }
}

pub use parser::{Constructor, Delimiter, Field, Item, Type};

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::mem;

#[derive(Debug, Default)]
struct Constructors(Vec<Constructor>);

#[derive(Debug)]
struct AllConstructors {
    types: BTreeMap<Option<String>, BTreeMap<String, Constructors>>,
    functions: BTreeMap<Option<String>, Vec<Constructor>>,
    layer: u32,
}

fn filter_items(iv: &mut Vec<Item>) {
    iv.retain(|i| {
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

fn partition_by_delimiter_and_namespace(iv: Vec<Item>) -> AllConstructors {
    let mut current = Delimiter::Types;
    let mut ret = AllConstructors {
        types: BTreeMap::new(),
        functions: BTreeMap::new(),
        layer: 0,
    };
    for i in iv {
        match i {
            Item::Delimiter(d) => current = d,
            Item::Constructor(c) => {
                match current {
                    Delimiter::Types => {
                        ret.types.entry(c.output.namespace().map(Into::into))
                            .or_insert_with(Default::default)
                            .entry(c.output.name().map(Into::into).unwrap())
                            .or_insert_with(Default::default)
                            .0.push(c);
                    },
                    Delimiter::Functions => {
                        ret.functions.entry(c.variant.namespace().map(Into::into))
                            .or_insert_with(Default::default)
                            .push(c);
                    },
                }
            },
            Item::Layer(i) => ret.layer = i,
        }
    }
    ret
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
enum WireKind {
    Bare(quote::Tokens),
    Boxed(quote::Tokens),
    TypeParameter(quote::Tokens),
    FlaggedTrue,
    Flags,
}

fn names_to_path_tokens(names: &[String]) -> quote::Tokens {
    let mut ret = quote!(::mtproto);
    for segment in names {
        let segment = no_conflict_ident(segment);
        ret = quote!(#ret::#segment);
    }
    ret
}

fn is_first_char_lowercase(s: &str) -> bool {
    s.chars().next()
        .map(char::is_lowercase)
        .unwrap_or(false)
}

impl WireKind {
    fn from_names(names: &[String]) -> Self {
        use self::WireKind::*;
        match names.last().map(String::as_str) {
            Some("true") => FlaggedTrue,
            Some(s) if is_first_char_lowercase(s) =>
                Bare(names_to_path_tokens(names)),
            Some(s) => Boxed(names_to_path_tokens(names)),
            None => unimplemented!(),
        }
    }

    fn type_parameter(name: &str) -> Self {
        let id = no_conflict_ident(name);
        WireKind::TypeParameter(quote! { #id })
    }

    fn become_deserialize_output(&mut self) {
        use self::WireKind::*;
        let ty_loc = match *self {
            Bare(ref mut t) |
            Boxed(ref mut t) => t,
            _ => return,
        };
        let ty = ty_loc.clone();
        *ty_loc = quote!(<#ty as ::Deserialize>::Output);
    }

    fn become_container_for(&mut self, contained: Self) {
        use self::WireKind::*;
        let ty_loc = match *self {
            Bare(ref mut t) |
            Boxed(ref mut t) => t,
            _ => unimplemented!(),
        };
        let contained = match contained {
            Bare(t) | Boxed(t) | TypeParameter(t) => t,
            _ => unimplemented!(),
        };
        let ty = ty_loc.clone();
        *ty_loc = quote!(#ty<#contained>);
    }
}

#[derive(Debug, Clone)]
struct TypeIR {
    wire_kind: WireKind,
    needs_box: bool,
    with_option: bool,
}

impl TypeIR {
    fn from_names(names: &[String]) -> Self {
        let wire_kind = WireKind::from_names(names);
        TypeIR {
            wire_kind, with_option: false,
            needs_box: match names.last().map(String::as_str) {
                // Special case two recursive types.
                Some("PageBlock") |
                Some("RichText") => true,
                _ => false,
            },
        }
    }

    fn type_parameter(name: &str) -> Self {
        TypeIR {
            wire_kind: WireKind::type_parameter(name),
            needs_box: false,
            with_option: false,
        }
    }

    fn flags() -> Self {
        TypeIR {
            wire_kind: WireKind::Flags,
            needs_box: false,
            with_option: false,
        }
    }

    fn with_container(self, mut container: TypeIR) -> Self {
        container.wire_kind.become_container_for(self.wire_kind);
        container
    }

    fn with_deserialize_output(mut self) -> Self {
        self.wire_kind.become_deserialize_output();
        self
    }

    fn with_option_wrapper(mut self) -> Self {
        self.with_option = true;
        self
    }

    fn io_turbofish(&self) -> quote::Tokens {
        use self::WireKind::*;
        println!("turbofish for: {:?}", self);
        match self.wire_kind {
            Bare(ref t) | Boxed(ref t) | TypeParameter(ref t) => quote!(::<#t>),
            Flags => quote!(::<::mtproto::Flags>),
            _ => quote!(FAIL_LOUDLY_AT_COMPILE_TIME),
        }
    }

    fn non_field_type(&self) -> quote::Tokens {
        use self::WireKind::*;
        match self.wire_kind {
            Bare(ref t) | Boxed(ref t) | TypeParameter(ref t) => t.clone(),
            _ => quote!(FAIL_LOUDLY_AT_COMPILE_TIME),
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
            _ => self.clone().with_deserialize_output().unboxed(),
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
        use self::WireKind::*;
        match self.wire_kind {
            FlaggedTrue => true,
            _ => false,
        }
    }
}

type TypeFixupMap = BTreeMap<Vec<String>, Vec<String>>;

impl Field {
    fn fixup(&mut self, fixup_map: &TypeFixupMap) {
        let is_flag_field = self.name.as_ref().map(String::as_str) == Some("flags");
        self.ty.fixup(fixup_map, is_flag_field);
    }
}

impl Type {
    fn fixup(&mut self, fixup_map: &TypeFixupMap, is_flag_field: bool) {
        use Type::*;
        let loc = match *self {
            Named(ref mut names) => names,
            Generic(ref mut container, ref mut ty) => {
                ty.fixup(fixup_map, false);
                container
            },
            Flagged(_, _, ref mut ty) => {
                ty.fixup(fixup_map, false);
                return;
            },
            Int if is_flag_field => {
                *self = Flags;
                return;
            },
            Int => {
                *self = Named(vec!["int".into()]);
                return;
            },
            _ => return,
        };
        match fixup_map.get(loc) {
            Some(replacement) => loc.clone_from(replacement),
            None => (),
        }
    }

    fn as_type(&self) -> TypeIR {
        use Type::*;
        match self {
            &Named(ref v) => TypeIR::from_names(v),
            &TypeParameter(ref s) => TypeIR::type_parameter(s),
            &Generic(ref container_name, ref ty) => {
                let container = TypeIR::from_names(container_name);
                ty.as_type().with_container(container)
            },
            &Flagged(_, _, ref ty) => {
                ty.as_type().with_option_wrapper()
            },
            &Flags => TypeIR::flags(),
            &Int |
            &Repeated(..) => unimplemented!(),
        }
    }
}

impl Field {
    fn as_field(&self) -> quote::Tokens {
        let name = self.name.as_ref().map(|n| no_conflict_ident(n)).unwrap();
        let ty = self.ty.as_type().field_type();

        quote! {
            #name: #ty
        }
    }
}

impl Constructor {
    fn fixup(&mut self, which: Delimiter, fixup_map: &TypeFixupMap) {
        if which == Delimiter::Functions {
            self.fixup_output();
        }
        self.fixup_fields(fixup_map);
        self.fixup_variant(fixup_map);
        //println!("fixed up {:#?}", self);
    }

    fn fixup_output(&mut self) {
        if self.is_output_a_type_parameter() {
            self.output = Type::TypeParameter(self.output.name().unwrap().into());
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

    fn fixup_fields(&mut self, fixup_map: &TypeFixupMap) {
        for f in &mut self.fields {
            f.fixup(fixup_map);
        }

        match self.variant.name() {
            Some("resPQ") |
            Some("p_q_inner_data") |
            Some("p_q_inner_data_temp") |
            Some("server_DH_params_ok") |
            Some("server_DH_inner_data") |
            Some("client_DH_inner_data") |
            Some("req_DH_params") |
            Some("set_client_DH_params") => (),
            _ => return,
        }
        for f in &mut self.fields {
            match f.ty {
                Type::Named(ref mut v) if v.len() == 1 && v[0] == "string" => {
                    v[0] = "bytes".into();
                },
                _ => (),
            }
        }
    }

    fn fixup_variant(&mut self, fixup_map: &TypeFixupMap) {
        match self.variant.name() {
            // The 'updates' variant struct conflicts with the module.
            Some("updates") => {
                self.variant = Type::Named(vec!["updates_".into()]);
            },
            _ => self.variant.fixup(fixup_map, false),
        }
    }

    fn flag_field_names(&self) -> HashSet<&str> {
        let mut ret = HashSet::new();
        for f in &self.fields {
            match f.name {
                Some(ref name) if f.ty == Type::Flags => {
                    ret.insert(name.as_str());
                }
                _ => (),
            }
            if let Some((flag, _)) = f.ty.flag_field() {
                ret.insert(flag);
            }
        }
        ret
    }

    fn flag_field_idents(&self) -> HashSet<syn::Ident> {
        self.flag_field_names()
            .into_iter()
            .map(no_conflict_ident)
            .collect()
    }

    fn non_flag_fields<'a>(&'a self) -> Box<Iterator<Item = &'a Field> + 'a> {
        let flag_fields = self.flag_field_names();
        Box::new({
            self.fields.iter()
                .filter(move |f| {
                    f.name.as_ref()
                        .map(|s| !flag_fields.contains(s.as_str()))
                        .unwrap_or(true)
                })
        })
    }

    fn fields_tokens(&self, pub_: quote::Tokens, trailer: quote::Tokens) -> quote::Tokens {
        let pub_ = std::iter::repeat(pub_);
        if self.fields.is_empty() {
            quote! { #trailer }
        } else {
            let fields = self.non_flag_fields()
                .map(Field::as_field);
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
        let tys = self.type_parameters.iter()
            .map(|f| no_conflict_ident(f.name.as_ref().unwrap()));
        quote! { <#(#tys),*> }
    }

    fn rpc_generics(&self) -> quote::Tokens {
        self.type_generics(&quote!(::Function))
    }

    fn type_generics(&self, trait_: &quote::Tokens) -> quote::Tokens {
        if self.type_parameters.is_empty() {
            return quote!();
        }
        let tys = self.type_parameters.iter()
            .map(|f| no_conflict_ident(f.name.as_ref().unwrap()));
        let traits = std::iter::repeat(trait_);
        quote! { <#(#tys: #traits),*> }
    }

    fn serialize_generics(&self) -> quote::Tokens {
        if self.type_parameters.is_empty() {
            return quote!();
        }
        let tys1 = self.type_parameters.iter()
            .map(|f| no_conflict_ident(f.name.as_ref().unwrap()))
            .collect::<Vec<_>>();
        let tys2 = tys1.clone();
        quote! { <#(#tys1: ::Serialize<Input = #tys2>),*> }
    }

    fn as_struct_determine_flags(&self, field_prefix: quote::Tokens) -> Option<(HashSet<syn::Ident>, quote::Tokens)> {
        let flag_fields = self.flag_field_idents();
        if flag_fields.is_empty() {
            return None;
        }
        let determination = {
            let fields = self.fields.iter()
                .filter_map(|f| {
                    let name = no_conflict_ident(f.name.as_ref().unwrap());
                    f.ty.flag_field().map(|(flag_field, bit)| {
                        let flag_field = no_conflict_ident(flag_field);
                        let is_defined = f.ty.as_type().is_defined_trailer();
                        quote! {
                            if #field_prefix #name #is_defined {
                                #flag_field |= 1 << #bit;
                            }
                        }
                    })
                });
            let flag_fields = &flag_fields;
            quote! {
                #( let mut #flag_fields = 0i32; )*
                #( #fields )*
            }
        };
        Some((flag_fields, determination))
    }

    fn as_struct_base(&self, name: &syn::Ident) -> quote::Tokens {
        let generics = self.generics();
        let fields = self.fields_tokens(quote! {pub}, quote! {;});
        quote! {
            #[derive(Debug, Clone)]
            pub struct #name #generics #fields
        }
    }

    fn as_struct_deserialize(&self) -> (HashSet<syn::Ident>, quote::Tokens) {
        if self.fields.is_empty() {
            return (HashSet::new(), quote!());
        }
        let flag_fields = self.flag_field_idents();
        let mut flags_to_read = vec![];
        let constructor = {
            let fields = self.fields.iter()
                .filter_map(|f| {
                    let name = no_conflict_ident(f.name.as_ref().unwrap());
                    let read_fish = f.ty.as_type().io_turbofish();
                    let mut expr = if flag_fields.contains(&name) {
                        flags_to_read.push(quote!(#name = _de.read_generic #read_fish ()?;));
                        return None;
                    } else if let Some((flag_field, bit)) = f.ty.flag_field() {
                        let flag_field = no_conflict_ident(flag_field);
                        let predicate = quote!(#flag_field & (1 << #bit) != 0);
                        if f.ty.as_type().is_unit() {
                            predicate
                        } else {
                            quote! {
                                if #predicate {
                                    Some(_de.read_generic #read_fish ()?)
                                } else {
                                    None
                                }
                            }
                        }
                    } else {
                        quote!(_de.read_generic #read_fish ()?)
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
        (flag_fields, constructor)
    }

    fn as_type_struct_base(&self, name: syn::Ident) -> quote::Tokens {
        let serialize_destructure = self.as_variant_ref_destructure(&name)
            .map(|d| quote! { let &#d = self; })
            .unwrap_or_else(|| quote!());
        let serialize_stmts = self.as_variant_serialize();
        let (flag_fields_, deserialize) = self.as_struct_deserialize();
        let flag_fields = &flag_fields_;
        let type_impl = self.as_type_impl(
            &name,
            self.tl_id().unwrap_or_else(|| quote!(unimplemented!())),
            quote!(#serialize_destructure #serialize_stmts Ok(())),
            Some(quote! {
                #( let #flag_fields: i32; )*
                Ok(#name #deserialize)
            }));
        let struct_block = self.as_struct_base(&name);
        quote! {
            #struct_block
            #type_impl
        }
    }

    fn as_single_type_struct(&self) -> quote::Tokens {
        let name = self.variant_name();
        let output_name = self.output.name().map(|n| no_conflict_ident(n)).unwrap();
        let type_struct = self.as_type_struct_base(name);
        quote! {
            #type_struct
            type #output_name = #name;
        }
    }

    fn variant_name(&self) -> syn::Ident {
        self.variant.name().map(|n| no_conflict_ident(n)).unwrap()
    }

    fn as_variant_type_struct(&self) -> quote::Tokens {
        if self.fields.is_empty() {
            quote!()
        } else {
            self.as_type_struct_base(self.variant_name())
        }
    }

    fn as_variant_ref_destructure(&self, name: &syn::Ident) -> Option<quote::Tokens> {
        if self.fields.is_empty() {
            return None;
        }
        let fields = self.non_flag_fields()
            .map(|f| {
                let prefix = f.ty.as_type().ref_prefix();
                let name = f.name.as_ref().unwrap();
                let field_name = no_conflict_ident(name);
                if let Some(local_name) = no_conflict_local_ident(&name) {
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
        println!("struct {:#?}", self);
        let (flag_fields, determine_flags) = self.as_struct_determine_flags(quote!())
            .unwrap_or_else(|| (HashSet::new(), quote!()));
        let fields = self.fields.iter()
            .map(|f| {
                println!("variant {:#?}", f);
                let name = f.name.as_ref().unwrap();
                let field_name = no_conflict_ident(name);
                let local_name = no_conflict_local_ident(name).unwrap_or_else(|| field_name.clone());
                let ty = f.ty.as_type();
                if ty.is_unit() {
                    return quote!();
                }
                let write_fish = ty.io_turbofish();
                if flag_fields.contains(&field_name) {
                    quote! { _ser.write_generic #write_fish (&#local_name)?; }
                } else if f.ty.flag_field().is_some() {
                    let outer_ref = ty.reference_prefix();
                    let inner_ref = ty.ref_prefix();
                    let local_ref = ty.local_reference_prefix();
                    quote! {
                        if let #outer_ref Some(#inner_ref inner) = #local_name {
                            _ser.write_generic #write_fish (#local_ref inner)?;
                        }
                    }
                } else {
                    let prefix = ty.local_reference_prefix();
                    quote! { _ser.write_generic #write_fish (#prefix #local_name)?; }
                }
            });
        quote! {
            #determine_flags
            #( #fields )*
        }
    }

    fn as_function_struct(&self) -> quote::Tokens {
        let name = self.variant_name();
        let rpc_generics = self.rpc_generics();
        let generics = self.generics();
        let struct_block = self.as_struct_base(&name);
        let mut output_ty = self.output.as_type().boxed();
        if self.output.is_type_parameter() {
            output_ty = quote! {#output_ty::Reply};
        }
        let serialize_destructure = self.as_variant_ref_destructure(&name)
            .map(|d| quote! { let &#d = self; })
            .unwrap_or_else(|| quote!());
        let serialize_stmts = self.as_variant_serialize();
        let type_impl = self.as_type_impl(
            &name,
            self.tl_id().unwrap_or_else(|| quote!(unimplemented!())),
            quote!(#serialize_destructure #serialize_stmts Ok(())),
            None);
        quote! {
            #struct_block
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
            quote!(#name(#name))
        }
    }

    fn as_variant_serialize_arm(&self) -> quote::Tokens {
        if self.fields.is_empty() {
            quote!(=> Ok(()))
        } else {
            let write_fish = self.variant.as_type().io_turbofish();
            quote!((ref x) => _ser.write_generic #write_fish (x))
        }
    }

    fn as_variant_deserialize(&self) -> quote::Tokens {
        if self.fields.is_empty() {
            quote!()
        } else {
            let read_fish = self.variant.as_type().io_turbofish();
            quote!((_de.read_generic #read_fish ()?))
        }
    }

    fn as_variant_generic_type(&self) -> TypeIR {
        let variant = self.variant.as_type();
        match self.type_parameters.first().and_then(|f| f.name.as_ref()) {
            Some(param) => {
                assert!(self.type_parameters.len() == 1);
                TypeIR::type_parameter(param).with_container(variant)
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

    fn as_type_impl(&self, name: &syn::Ident, type_id: quote::Tokens, serialize: quote::Tokens, deserialize: Option<quote::Tokens>) -> quote::Tokens {
        let serialize_generics = self.serialize_generics();
        let ty = self.as_variant_generic_type();
        let io_fish = ty.io_turbofish();
        let generics = self.generics();

        let deserialize = deserialize.map(|body| {
            let boxed_deserialize_generics = self.type_generics(&quote!(::BoxedDeserialize));
            let deserialize_generics = self.type_generics(&quote!(::Deserialize));
            quote! {
                impl #boxed_deserialize_generics ::BoxedDeserialize for #name #generics {
                    fn deserialize_boxed(_id: ::ConstructorNumber, _de: &mut ::Deserializer) -> ::Result<Self> {
                        #body
                    }
                }

                impl #deserialize_generics ::Deserialize for #name #generics {
                    type Output = Self;
                    fn deserialize(de: &mut ::Deserializer) -> ::Result<Self> {
                        de.read_generic #io_fish ()
                    }
                }
            }
        }).unwrap_or_else(|| quote!());

        quote! {

            impl #serialize_generics ::BoxedSerialize for #name #generics {
                fn type_id(&self) -> ::ConstructorNumber {
                    #type_id
                }

                fn serialize_boxed(&self, _ser: &mut ::Serializer) -> ::Result<()> {
                    #serialize
                }
            }

            impl #serialize_generics ::Serialize for #name #generics {
                type Input = Self;
                fn serialize(obj: &Self::Input, ser: &mut ::Serializer) -> ::Result<()> {
                    ser.write_generic #io_fish (obj)
                }
            }

            #deserialize
        }
    }
}

impl Constructors {
    fn fixup(&mut self, delim: Delimiter, fixup_map: &TypeFixupMap) {
        for c in &mut self.0 {
            c.fixup(delim, fixup_map);
        }
    }

    fn coalesce_methods(&self) -> BTreeMap<&str, BTreeMap<&Type, BTreeSet<&Constructor>>> {
        let mut map: BTreeMap<&str, BTreeMap<&Type, BTreeSet<&Constructor>>> = BTreeMap::new();
        for cons in &self.0 {
            for field in cons.non_flag_fields() {
                let name = match field.name.as_ref() {
                    Some(s) => s.as_str(),
                    None => continue,
                };
                map.entry(name)
                    .or_insert_with(Default::default)
                    .entry(&field.ty)
                    .or_insert_with(Default::default)
                    .insert(cons);
            }
        }
        map
    }

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
        let self_type = self.0[0].output.as_type().boxed();
        quote! {

            impl ::BoxedSerialize for #name {
                fn type_id(&self) -> ::ConstructorNumber {
                    #type_id
                }

                fn serialize_boxed(&self, _ser: &mut ::Serializer) -> ::Result<()> {
                    #serialize
                }
            }

            impl ::Serialize for #name {
                type Input = #self_type;
                fn serialize(obj: &Self::Input, ser: &mut ::Serializer) -> ::Result<()> {
                    unimplemented!()
                }
            }

            impl ::BoxedDeserialize for #name {
                fn deserialize_boxed(_id: ::ConstructorNumber, _de: &mut ::Deserializer) -> ::Result<Self> {
                    #deserialize
                }
            }

            impl ::Deserialize for #name {
                type Output = #self_type;
                fn deserialize(de: &mut ::Deserializer) -> ::Result<Self::Output> {
                    unimplemented!()
                }
            }
        }
    }

    fn as_type_id_match(&self, enum_name: &syn::Ident) -> quote::Tokens {
        let constructors = self.0.iter()
            .filter(|c| c.tl_id().is_some())
            .map(|c| {
                let pat = c.as_variant_empty_pattern();
                let tl_id = c.tl_id();
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
            .map(|c| {
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

    fn as_deserialize_match(&self, enum_name: &syn::Ident) -> quote::Tokens {
        let tl_ids = self.0.iter()
            .map(|c| c.tl_id());
        let constructors = self.0.iter()
            .filter(|c| c.tl_id().is_some())
            .map(|c| {
                let variant_name = c.variant_name();
                let tl_id = c.tl_id();
                let deserialize = c.as_variant_deserialize();
                quote!(#tl_id => Ok(#enum_name::#variant_name #deserialize))
            });
        quote! {
            match _id {
                #( #constructors, )*
                id => Err(::error::ErrorKind::InvalidType(vec![#( #tl_ids ),*], id).into()),
            }
        }
    }

    fn as_structs(&self) -> quote::Tokens {
        // if self.0.len() == 1 {
        //     return self.0[0].as_single_type_struct();
        // }

        let structs = self.0.iter()
            .map(Constructor::as_variant_type_struct);

        if !self.0.iter().any(|c| c.tl_id().is_some()) {
            return quote! {
                #( #structs )*
            };
        }

        let name = self.0[0].output.name().map(|n| no_conflict_ident(n)).unwrap();
        let variants = self.0.iter()
            .map(Constructor::as_variant);
        let methods = self.determine_methods(&name);
        let type_impl = self.as_type_impl(
            &name,
            self.as_type_id_match(&name),
            self.as_serialize_match(&name),
            self.as_deserialize_match(&name));

        quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #( #variants , )*
            }
            #methods
            #type_impl
            #( #structs )*
        }
    }

    fn as_dynamic_ctors(&self) -> Vec<(Option<Vec<String>>, u32, quote::Tokens)> {
        let ty = self.0[0].output.as_type().unboxed();
        let ty_name = self.0[0].output.names_vec();
        self.0.iter()
            .filter_map(|c| {
                c.tl_id().map(|tl_id| {
                    (ty_name.cloned(), c.tl_id.unwrap(), quote!(cstore.add::<#ty>(#tl_id)))
                })
            })
            .collect()
    }
}

fn make_fixup_map(constructors: &AllConstructors) -> TypeFixupMap {
    let mut variants_to_outputs: TypeFixupMap = Default::default();

    let iter = constructors.types.iter()
        .flat_map(|(ns, constructor_map)| {
            constructor_map.iter().flat_map(move |(output, constructors)| {
                constructors.0.iter().filter_map(move |constructor| {
                    //println!("cons {:#?}", constructor);
                    let variant_name = match constructor.variant {
                        Type::Named(ref n) => n,
                        _ => return None,
                    };
                    let mut full_output: Vec<String> = ns.iter().cloned().collect();
                    full_output.extend(variant_name.last().cloned());
                    Some((variant_name.clone(), full_output))
                })
            })
        });

    let iter = constructors.functions.iter()
        .flat_map(|(ns, constructors)| {
            constructors.iter().filter_map(move |constructor| {
                let variant_name = match constructor.variant {
                    Type::Named(ref n) => n,
                    _ => return None,
                };
                let mut full_output: Vec<String> = vec!["rpc".into()];
                full_output.extend(ns.iter().cloned());
                full_output.extend(variant_name.last().cloned());
                Some((variant_name.clone(), full_output))
            })
        })
        .chain(iter);

    for (k, v) in iter {
        if let Some(old_value) = variants_to_outputs.insert(k, v) {
            panic!("duplicated key/value (value: {:?})", old_value);
        }
    }

    variants_to_outputs
}

pub fn generate_code_for(input: &str) -> String {
    let constructors = {
        let mut items = parser::parse_string(input).unwrap();
        filter_items(&mut items);
        partition_by_delimiter_and_namespace(items)
    };

    let layer = constructors.layer as i32;
    let mut items = vec![quote! {
        #![allow(non_camel_case_types)]
        pub use mtproto_prelude::*;

        pub const LAYER: i32 = #layer;
    }];

    let variants_to_outputs = make_fixup_map(&constructors);

    let mut dynamic_ctors = vec![];
    for (ns, mut constructor_map) in constructors.types {
        dynamic_ctors.extend(
            constructor_map.values().flat_map(Constructors::as_dynamic_ctors));
        let substructs = constructor_map.values_mut()
            .map(|c| {
                c.fixup(Delimiter::Functions, &variants_to_outputs);
                c.as_structs()
            });
        match ns {
            None => items.extend(substructs),
            Some(name) => {
                let name = no_conflict_ident(name.as_str());
                items.push(quote! {
                    pub mod #name {
                        #( #substructs )*
                    }
                });
            },
        }
    }

    dynamic_ctors.sort_by(|t1, t2| (&t1.0, t1.1).cmp(&(&t2.0, t2.1)));
    let dynamic_ctors = dynamic_ctors.into_iter()
        .map(|t| t.2);
    // items.push(quote! {
    //     pub fn register_ctors<R: ::tl::parsing::Reader>(cstore: &mut ::tl::dynamic::TLCtorMap<R>) {
    //         cstore.add::<Vec<Object>>(::tl::VEC_TYPE_ID);
    //         #( #dynamic_ctors; )*
    //     }
    // });

    let mut rpc_items = vec![];
    for (ns, mut substructs) in constructors.functions {
        substructs.sort_by(|c1, c2| c1.variant.cmp(&c2.variant));
        let substructs = substructs.into_iter()
            .map(|mut c| {
                c.fixup(Delimiter::Functions, &variants_to_outputs);
                c.as_function_struct()
            });
        match ns {
            None => rpc_items.extend(substructs),
            Some(name) => {
                let name = no_conflict_ident(name.as_str());
                rpc_items.push(quote! {
                    pub mod #name {
                        #( #substructs )*
                    }
                });
            },
        }
    }
    items.push(quote! {
        pub mod rpc {
            #( #rpc_items )*
        }
    });

    (quote! { #(#items)* }).to_string()
}
