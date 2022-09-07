# EnPow

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-informational?style=flat)](COPYRIGHT.md)

EnPow is a procedural macro crate used to en**Pow**er user defined **En**ums with many methods usually known from the standard library's `Result<T, E>` and `Option<T>`. It can generate methods like `fn is_<variant>(&self) -> bool` or `fn unwrap_<variant>(self) -> <inner>`, supporting variants with named or unnamed fields (or none), as well as generics. See the `enpow` macro documentation for details on the specific methods supported.

Additionally, this crate allows to extract the data associated with each enum variant into separate structs, allowing for more compact code e.g. when designing an Abstract Syntax Tree. See the `extract` macro documentation for more details.

It is also possible to combine both macros when keeping them in the right order: first `extract` and then `enpow`. Combining both macros avoids generating separate structs for `Ref` or `Mut` struct variants as further explained in the following use case. Nevertheless, both macros can be used indenpendently from each other.

# Installation

Add the following to your `Cargo.toml` to include `enpow` as dependency to your project.
```toml
[dependencies]
enpow = "~2.0.0"
```

# Use Case

The following code describes a simple logging system with support for different log levels. We then create an example log and print all errors in it.

```rust
/// A log entry
#[derive(Clone)]
pub enum LogEntry<C: ToString + Clone> {
    /// A simple note without context
    Note(
        /// Note's message
        String
    ),
    /// A warning with a given context
    Warning(
        /// Warning's message
        String,
        /// Context of the warning
        C
    ),
    /// An error message with error code and context
    Error {
        /// Error message
        message: String,
        /// Context of the error
        context: C,
        /// Error code
        code: i16,
    },
}

/// Application log for a certain context type
pub struct Log<C: ToString + Clone> {
    /// Log entries
    entries: Vec<LogEntry<C>>,
}

impl<C: ToString + Clone> Log<C> {
    /// Collects all entries of type `LogEntry::Error` from the log
    pub fn get_errors(&self) -> Vec<LogEntry<C>> {
        self.entries.iter()
            .filter(|entry| match entry {
                LogEntry::Error { .. } => true,
                _ => false,
            })
            .cloned()
            .collect()
    }
}

/// Line number in source
type Line = usize;

// Create a sample log
let log = Log { entries: vec![
    LogEntry::Note("All fine 😊".into()),
    LogEntry::Warning("There might be an issue here 🤔".into(), 4),
    LogEntry::Error {
        message: "There _was_ an issue 😖".into(),
        context: 4,
        code: -1,
    },
    LogEntry::Error {
        message: "Follow up".into(),
        context: 12,
        code: -7,
    },
] };

// Get and print all errors
let errors = log.get_errors();
if !errors.is_empty() {
    eprintln!("Failed for the following reasons:");

    for error in errors {
        match error {
            LogEntry::Error { message, context: line, code } => {
                eprintln!("Error {code} at {line}: {message}");
            }
            _ => panic!("Expected to find a LogEntry::Error"),
        }
    }
}
```

This code works, but it is a bit wordy having to pattern match against the specific variant of each log entry every time.

## Using `enpow`

Here comes the `enpow` macro into play. It can generate some helper methods that should make our code more concise. We specifically make use of the `is_<variant>()` (keyword `IsVar`) and `unwrap_<variant>()` (keyword `UnwrapVar`) methods.

```rust
use enpow::enpow; // ℹ️

/// A log entry
#[enpow(IsVar, UnwrapVar)] // ℹ️
#[derive(Clone)]
pub enum LogEntry<C: ToString + Clone> {
    // ✂ unchanged
#   /// A simple note without context
#   Note(
#       /// Note's message
#       String
#   ),
#   /// A warning with a given context
#   Warning(
#       /// Warning's message
#       String,
#       /// Context of the warning
#       C
#   ),
#   /// An error message with error code and context
#   Error {
#       /// Error message
#       message: String,
#       /// Context of the error
#       context: C,
#       /// Error code
#       code: i16,
#   },
}

/// Application log for a certain context type
pub struct Log<C: ToString + Clone> {
    /// Log entries
    entries: Vec<LogEntry<C>>,
}

impl<C: ToString + Clone> Log<C> {
    /// Collects all entries of type `LogEntry::Error` from the log
    pub fn get_errors(&self) -> Vec<LogEntry<C>> {
        self.entries.iter()
            .filter(|entry| entry.is_error()) // ℹ️
            .cloned()
            .collect()
    }
}

/// Line number in source
type Line = usize;

// Create a sample log
let log = Log { entries: vec![
    // ✂ unchanged
#   LogEntry::Note("All fine 😊".into()),
#   LogEntry::Warning("There might be an issue here 🤔".into(), 4),
#   LogEntry::Error {
#       message: "There _was_ an issue 😖".into(),
#       context: 4,
#       code: -1,
#   },
#   LogEntry::Error {
#       message: "Follow up".into(),
#       context: 12,
#       code: -7,
#   },
] };

// Get and print all errors
let errors = log.get_errors();
if !errors.is_empty() {
    eprintln!("Failed for the following reasons:");

    for error in errors {
        let error = error.unwrap_error();  // ℹ️
        eprintln!("Error {} at {}: {}", error.code, error.context, error.message);
    }
}
```

Even though this code is already more concise, there is still a rough edge. When collecting all the errors, they still are returned as `Vec<LogEntry>` with no guarantee by the type system that this collection would actually contain only errors. We could solve this problem by returning from `get_errors()` the type `LogEntryError<C>` that was already constructed to aid the `unwrap_error()` method.

Currently, however, there are multiple (depending on the generated methods, up to four) parts in the code where the same data is defined: in the enum variant `LogEntry::Error { .. }`, and in the just mentioned, automatically generated struct `LogEntryError<C>`. It might be more desireable to extract the enum variant into this struct and let the variant just point to it, like so: `LogEntry::Error(LogEntryError<C>)`. This would especially come in handy when working with methods that need references to the variant's associated data, since up until now, special `LogEntryErrorRef<C>` and `LogEntryErrorMut<C>` structs need to be generated for that. With the data extracted, it would be possible to just use `&LogEntryError<C>` and `&mut LogEntryError<C>`.

## Using `extract`

Here, the `extract` macro comes into play, which does this automatically for us. We tell the macro to do the extraction for every variant with more than one unnamed field (keyword `Unnamed`) or with named fields (keyword `Named`). This will automatically change our two affected variants into `LogEntry::Warning(LogEntryWarning<C>)` and `LogEntry::Error(LogEntryError<C>)`. We can then use the generated type `LogEntryError<C>` to guarantee that actually only errors are returned by `get_errors()`. Note, how the construction of the log entries changes, even though the enum code was not changed by hand. `extract` also automatically implements the `From` trait to convert instances of the extracted types into the corresponding enum variants.

Additionally, we make use of the method `<variant>_as_ref()` (keyword `VarAsRef`) to make collecting all error entries and unwrapping them more concise. To make the cloning of the automatically generated `LogEntryError<C>` struct work, we add the `inner(derive(Clone))` attribute.

> ⚠️ Macro Order: When combining both macros, `enpow` must be placed _after_ `extract` to work correctly. If the helper attribute `inner` is placed between `extract` and `enpow`, it will only be effective for `extract`. If it is placed after both `extract` and `enpow`, it will be effective for both macros. Also, the normal `derive` macro must be placed _after_ `extract` to work correctly.

```rust
use enpow::{enpow, extract}; // ℹ️

/// A log entry
#[extract(Unnamed, Named)] // ℹ️
#[enpow(VarAsRef)]         //
#[inner(derive(Clone))]    //
#[derive(Clone)]
pub enum LogEntry<C: ToString + Clone> {
    // ✂ unchanged
#   /// A simple note without context
#   Note(
#       /// Note's message
#       String
#   ),
#   /// A warning with a given context
#   Warning(
#       /// Warning's message
#       String,
#       /// Context of the warning
#       C
#   ),
#   /// An error message with error code and context
#   Error {
#       /// Error message
#       message: String,
#       /// Context of the error
#       context: C,
#       /// Error code
#       code: i16,
#   },
}

/// Application log for a certain context type
pub struct Log<C: ToString + Clone> {
    /// Log entries
    entries: Vec<LogEntry<C>>,
}

impl<C: ToString + Clone> Log<C> {
    /// Collects all entries of type `LogEntry::Error` from the log
    pub fn get_errors(&self) -> Vec<LogEntryError<C>> { // ℹ️
        self.entries.iter()
            .filter_map(|entry| entry.error_as_ref())   // ℹ️
            .cloned()
            .collect()
    }
}

/// Line number in source
type Line = usize;

// Create a sample log
let log = Log { entries: vec![
    LogEntry::Note("All fine 😊".into()),
    LogEntry::from(LogEntryWarning( // ℹ️
        "There might be an issue here 🤔".into(),
        4,
    )),
    LogEntry::from(LogEntryError { // ℹ️
        message: "There _was_ an issue 😖".into(),
        context: 4,
        code: -1,
    }),
    LogEntry::from(LogEntryError { // ℹ️
        message: "Follow up".into(),
        context: 12,
        code: -7,
    }),
] };

// Get and print all errors
let errors = log.get_errors();
if !errors.is_empty() {
    eprintln!("Failed for the following reasons:");

    for error in errors {
        // ℹ️
        eprintln!("Error {} at {}: {}", error.code, error.context, error.message);
    }
}
```

At last, we get rid of the long names of the generated structs by giving defining another pattern naming pattern. For this, the same `inner()` helper attribute with the argument `type_name` or `type_names` is used. Besides, with `inner()` it is also possible to change how the variant's name appears in its methods `method_name="..."`, and it is possible to add auto derives to a single variant.

```rust
use enpow::{enpow, extract};

/// A log entry
#[extract(Unnamed, Named)]
#[enpow(VarAsRef)]
#[inner(type_names=Var, derive(Clone))] // ℹ️
#[derive(Clone)]
pub enum LogEntry<C: ToString + Clone> {
    // ✂ unchanged
#   /// A simple note without context
#   Note(
#       /// Note's message
#       String
#   ),
#   /// A warning with a given context
#   Warning(
#       /// Warning's message
#       String,
#       /// Context of the warning
#       C
#   ),
#   /// An error message with error code and context
#   Error {
#       /// Error message
#       message: String,
#       /// Context of the error
#       context: C,
#       /// Error code
#       code: i16,
#   },
}

/// Application log for a certain context type
pub struct Log<C: ToString + Clone> {
    /// Log entries
    entries: Vec<LogEntry<C>>,
}

impl<C: ToString + Clone> Log<C> {
    /// Collects all entries of type `LogEntry::Error` from the log
    pub fn get_errors(&self) -> Vec<Error<C>> { // ℹ️
        self.entries.iter()
            .filter_map(|entry| entry.error_as_ref())
            .cloned()
            .collect()
    }
}

/// Line number in source
type Line = usize;

// Create a sample log
let log = Log { entries: vec![
    LogEntry::Note("All fine 😊".into()),
    LogEntry::from(Warning( // ℹ️
        "There might be an issue here 🤔".into(),
        4,
    )),
    LogEntry::from(Error { // ℹ️
        message: "There _was_ an issue 😖".into(),
        context: 4,
        code: -1,
    }),
    LogEntry::from(Error { // ℹ️
        message: "Follow up".into(),
        context: 12,
        code: -7,
    }),
] };

// Get and print all errors
let errors = log.get_errors();
if !errors.is_empty() {
    eprintln!("Failed for the following reasons:");

    for error in errors {
        eprintln!("Error {} at {}: {}", error.code, error.context, error.message);
    }
}
```

This was just a quick introductory example for understanding the use and usage of this crate. See the macros' documentation for more details.

# Inspiration and Alternatives

While the first plan for this crate was limited to simple `unwrap` methods and alike, the crate [`variantly`](https://crates.io/crates/variantly) was a great inspiration to take this idea way further. It can be seen as an alternative to this crate with partially different feature set.

Another alternative with partially different feature set would be [`enum_variant_type`](https://crates.io/crates/enum_variant_type).

<br/>

---

<br/>

#### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as below, without any additional terms or conditions.


#### License

&copy; 2022 Florian Köhler.

This project is licensed at your option under either of

- [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) ([`LICENSE-APACHE`](LICENSE-APACHE))
- [MIT license](https://opensource.org/licenses/MIT) ([`LICENSE-MIT`](LICENSE-MIT))

The [SPDX](https://spdx.dev) license identifier for this project is `MIT OR Apache-2.0`.

<br/>

Licensing derived from [arnavyc](https://github.com/arnavyc)/[dual-licensed-mit-apache](https://github.com/arnavyc/dual-licensed-mit-apache)