# EnPow

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-informational?style=flat-square)](COPYRIGHT.md)

EnPow is a procedural macro crate used to en**Pow**er user defined **En**ums with many methods usually known from the standard library's `Result<T, E>` and `Option<T>`. It can generate methods like `fn is_<variant>(&self) -> bool` or `fn unwrap_<variant>(self) -> <inner>`, supporting variants with named or unnamed fields (or none), as well as generics. See the `enpow` macro documentation for details on the specific methods supported.

Additionally, this crate allows to extract the data associated with each enum variant into separate structs, allowing for more compact code e.g. when designing an Abstract Syntax Tree. See the `extract` macro documentation for more details.

It is also possible to combine both macros when keeping them in the right order: first `extract` and then `enpow`. Combining both macros avoids generating separate structs for `Ref` or `Mut` struct variants as demonstrated in one of the following examples.

# Usage Examples

The following examples demonstrate the separate usage of the macros `enpow` and `extract`, as well as their combined usage. See the macro's documentation for more details.

## Using just `enpow`

```rust
use enpow::enpow;

#[enpow(Var, VarAsRef)]
#[enpow_derive(Debug, PartialEq)]
#[derive(Clone, Debug, PartialEq)]
pub enum Token<Span> {
    /// `+`
    Plus(
        /// Source span
        Span
    ),
    /// Unsigned integer literal
    Number {
        /// Source span
        span: Span,
        /// Value
        value: u64,
    }
}

// Use the auto implementations
assert_eq!(Token::Plus(3).plus(), Some(3));
assert_eq!(Token::Plus(7).number(), None);
assert_eq!(Token::Number { span: 0, value: 42 }.number().unwrap().span, 0);

let mut num = Token::Number { span: 10, value: 7 };
*num.number_as_mut().unwrap().span = 20;
assert_eq!(num.number(), Some(TokenNumber { span: 20, value: 7 }))
```

<details>
<summary>See generated code</summary>

```rust
#[derive(Clone, Debug, PartialEq)]
pub enum Token<Span> {
    /// `+`
    Plus(
        /// Source span
        Span
    ),
    /// Unsigned integer literal
    Number {
        /// Source span
        span: Span,
        /// Value
        value: u64,
    }
}

#[allow(unused)]
#[derive(Debug, PartialEq)]
/// Unsigned integer literal
pub struct TokenNumber<Span> {
    /// Source span
    pub span: Span,
    /// Value
    pub value: u64,
}

#[allow(unused)]
#[derive(Debug, PartialEq, Clone, Copy)]
/// Unsigned integer literal
pub struct TokenNumberRef<'token_number, Span> {
    /// Source span
    pub span: &'token_number Span,
    /// Value
    pub value: &'token_number u64,
}

#[allow(unused)]
#[derive(Debug, PartialEq)]
/// Unsigned integer literal
pub struct TokenNumberMut<'token_number, Span> {
    /// Source span
    pub span: &'token_number mut Span,
    /// Value
    pub value: &'token_number mut u64,
}

#[automatically_derived]
#[allow(unused)]
impl<Span> Token<Span> {
    pub fn plus(self) -> Option<Span> {
        match self {
            Token::Plus(f0) => Some(f0),
            _ => None,
        }
    }

    pub fn plus_as_ref(&self) -> Option<&Span> {
        match self {
            Token::Plus(f0) => Some(f0),
            _ => None,
        }
    }

    pub fn plus_as_mut(&mut self) -> Option<&mut Span> {
        match self {
            Token::Plus(f0) => Some(f0),
            _ => None,
        }
    }

    pub fn number(self) -> Option<TokenNumber<Span>> {
        match self {
            Token::Number { span, value } => Some(TokenNumber { span, value }),
            _ => None,
        }
    }

    pub fn number_as_ref(&self) -> Option<TokenNumberRef<Span>> {
        match self {
            Token::Number { span, value } => Some(TokenNumberRef { span, value }),
            _ => None,
        }
    }

    pub fn number_as_mut(&mut self) -> Option<TokenNumberMut<Span>> {
        match self {
            Token::Number { span, value } => Some(TokenNumberMut { span, value }),
            _ => None,
        }
    }
}

// Use the auto implementations
assert_eq!(Token::Plus(3).plus(), Some(3));
assert_eq!(Token::Plus(7).number(), None);
assert_eq!(Token::Number { span: 0, value: 42 }.number().unwrap().span, 0);

let mut num = Token::Number { span: 10, value: 7 };
*num.number_as_mut().unwrap().span = 20;
assert_eq!(num.number(), Some(TokenNumber { span: 20, value: 7 }))
```
</details>

## Using just `extract`

```rust
use enpow::extract;

#[extract(All)]
#[extract_derive(Clone, Debug, PartialEq)]
#[derive(Clone, Debug, PartialEq)]
pub enum Token<Span> {
    /// `+`
    Plus(
        /// Source span
        Span
    ),
    /// Unsigned integer literal
    Number {
        /// Source span
        span: Span,
        /// Value
        value: u64,
    }
}

// Use Debug and PartialEq
assert_eq!(TokenPlus((2,3)), TokenPlus((2,3)));
assert_eq!(
    TokenNumber { span: (0, 4), value: 1024 },
    TokenNumber { span: (0, 4), value: 1024 }
);
```

<details>
<summary>See generated code</summary>

```rust
#[derive(Clone, Debug, PartialEq)]
pub enum Token<Span> {
    /// `+`
    Plus(TokenPlus<Span>),
    /// Unsigned integer literal
    Number(TokenNumber<Span>),
}

#[derive(Clone, Debug, PartialEq)]
/// `+`
pub struct TokenPlus<Span>(
    /// Source span
    pub Span
);

#[derive(Clone, Debug, PartialEq)]
/// Unsigned integer literal
pub struct TokenNumber<Span> {
    /// Source span
    pub span: Span,
    /// Value
    pub value: u64,
}

// Use Debug and PartialEq
assert_eq!(TokenPlus((2,3)), TokenPlus((2,3)));
assert_eq!(
    TokenNumber { span: (0, 4), value: 1024 },
    TokenNumber { span: (0, 4), value: 1024 }
);
```
</details>

## Combining `extract` and `enpow`

```rust
use enpow::{enpow, extract};

#[extract(Named)]
#[extract_derive(Clone, Debug, PartialEq)]
#[enpow(IsVar)]
#[derive(Clone, Debug, PartialEq)]
pub enum Token<Span> {
    /// `+`
    Plus(
        /// Source span
        Span
    ),
    /// Unsigned integer literal
    Number {
        /// Source span
        span: Span,
        /// Value
        value: u64,
    }
}

// Use the auto implementations
let token = Token::Number(TokenNumber { span: (0, 3), value: 1024 });
assert!(token.is_number_and(|num: &TokenNumber<_>| num.value == 1024));
```

<details>
<summary>See generated code</summary>

```rust
#[derive(Clone, Debug, PartialEq)]
pub enum Token<Span> {
    /// `+`
    Plus(
        /// Source span
        Span
    ),
    /// Unsigned integer literal
    Number(TokenNumber<Span>),
}

#[automatically_derived]
#[allow(unused)]
impl<Span> Token<Span> {
    pub fn is_plus(&self) -> bool {
        match self {
            Token::Plus(f0) => true,
            _ => false,
        }
    }

    pub fn is_plus_and(&self, f: impl FnOnce(&Span) -> bool) -> bool {
        match self {
            Token::Plus(f0) => f(f0),
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Token::Number(f0) => true,
            _ => false,
        }
    }

    pub fn is_number_and(&self, f: impl FnOnce(&TokenNumber<Span>) -> bool) -> bool {
        match self {
            Token::Number(f0) => f(f0),
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
/// Unsigned integer literal
pub struct TokenNumber<Span> {
    /// Source span
    pub span: Span,
    /// Value
    pub value: u64,
}

// Use the auto implementations
let token = Token::Number(TokenNumber { span: (0, 3), value: 1024 });
assert!(token.is_number_and(|num: &TokenNumber<_>| num.value == 1024));
```
</details>

# Inspiration

While the first plan for this crate was limited to simple `unwrap_as` methods and alike, the crate [`variantly`](https://crates.io/crates/variantly) was a great inspiration to take this idea way further. It can be seen as an alternative to this crate with partially different feature set.

# Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as below, without any additional terms or conditions.

# License

&copy; 2022 Florian Köhler.

This project is licensed at your option under either of

- [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) ([`LICENSE-APACHE`](LICENSE-APACHE))
- [MIT license](https://opensource.org/licenses/MIT) ([`LICENSE-MIT`](LICENSE-MIT))

The [SPDX](https://spdx.dev) license identifier for this project is `MIT OR Apache-2.0`.

---

Licensing derived from [arnavyc](https://github.com/arnavyc)/[dual-licensed-mit-apache](https://github.com/arnavyc/dual-licensed-mit-apache)