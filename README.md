# EnPow

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-informational?style=flat-square)](COPYRIGHT.md)

EnPow is a procedural macro crate used to en**Pow**er user defined **En**ums with many methods usually known from the standard library's `Result<T, E>` and `Option<T>`. It can generate methods like `fn is_<variant>(&self) -> bool` or `fn unwrap_<variant>(self) -> <inner>`, supporting variants with named or unnamed fields (or none), as well as generics. See the `enpow` macro documentation for details on the specific methods supported.

Additionally, this crate allows to extract the data associated with each enum variant into separate structs, allowing for more compact code e.g. when designing an Abstract Syntax Tree. See the `extract` macro documentation for more details.

It is also possible to combine both macros when keeping them in the right order: first `extract` and then `enpow`. Combining both macros avoids generating separate structs for `Ref` or `Mut` struct variants as demonstrated in the following use case.

# Installation

Add the following to your `Cargo.toml` to include `enpow` as dependency to your project.
```toml
[dependencies]
enpow = "~1.0.2"
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

<details>
<summary>ℹ️ Click to reveal generated code</summary>

```rust
#[derive(Clone)]
pub enum LogEntry<C: ToString + Clone> {
    /// A simple note without context
    Note(
        /// Note's message
        String,
    ),
    /// A warning with a given context
    Warning(
        /// Warning's message
        String,
        /// Context of the warning
        C,
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

#[allow(unused)]
#[derive()]
/// An error message with error code and context
pub struct LogEntryError<C: ToString + Clone> {
    /// Error message
    pub message: String,
    /// Context of the error
    pub context: C,
    /// Error code
    pub code: i16,
}

#[allow(unused)]
#[derive(Clone, Copy)]
/// An error message with error code and context
pub struct LogEntryErrorRef<'log_entry_error, C: ToString + Clone> {
    /// Error message
    pub message: &'log_entry_error String,
    /// Context of the error
    pub context: &'log_entry_error C,
    /// Error code
    pub code: &'log_entry_error i16,
}

#[allow(unused)]
#[derive()]
/// An error message with error code and context
pub struct LogEntryErrorMut<'log_entry_error, C: ToString + Clone> {
    /// Error message
    pub message: &'log_entry_error mut String,
    /// Context of the error
    pub context: &'log_entry_error mut C,
    /// Error code
    pub code: &'log_entry_error mut i16,
}

#[automatically_derived]
#[allow(unused)]
impl<C: ToString + Clone> LogEntry<C> {
    /// Returns `true`, if the enum value is of the expected type, otherwise returns
    /// `false`.
    pub fn is_note(&self) -> bool {
        match self {
            LogEntry::Note(f0) => true,
            _ => false,
        }
    }
    
    /// Returns `true`, if the enum value is of the expected type and the given closure
    /// evalutates to `true`, otherwise returns `false`.
    pub fn is_note_and<'log_entry_note>(
        &'log_entry_note self,
        f: impl FnOnce(&'log_entry_note String) -> bool,
    ) -> bool {
        match self {
            LogEntry::Note(f0) => f(f0),
            _ => false,
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// panics.
    pub fn unwrap_note(self) -> String {
        match self {
            LogEntry::Note(f0) => f0,
            _ => panic!("Failed unwrapping to LogEntry::Note. Unexpected variant"),
        }
    }
    
    /// Returns a reference to the inner data, if the enum value is of the expected type,
    /// otherwise panics.
    pub fn unwrap_note_as_ref<'log_entry_note>(&'log_entry_note self) -> &'log_entry_note String {
        match self {
            LogEntry::Note(f0) => f0,
            _ => panic!("Failed unwrapping to LogEntry::Note. Unexpected variant"),
        }
    }
    
    /// Returns a mutable reference to the inner data, if the enum value is of the expected
    /// type, otherwise panics.
    pub fn unwrap_note_as_mut<'log_entry_note>(
        &'log_entry_note mut self,
    ) -> &'log_entry_note mut String {
        match self {
            LogEntry::Note(f0) => f0,
            _ => panic!("Failed unwrapping to LogEntry::Note. Unexpected variant"),
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// returns the given default value.
    pub fn unwrap_note_or(self, default: String) -> String {
        match self {
            LogEntry::Note(f0) => f0,
            _ => default,
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// returns the value that the given closure evaluated to.
    pub fn unwrap_note_or_else(self, f: impl FnOnce(Self) -> String) -> String {
        match self {
            LogEntry::Note(f0) => f0,
            some => f(some),
        }
    }
    
    /// Returns `true`, if the enum value is of the expected type, otherwise returns
    /// `false`.
    pub fn is_warning(&self) -> bool {
        match self {
            LogEntry::Warning(f0, f1) => true,
            _ => false,
        }
    }
    
    /// Returns `true`, if the enum value is of the expected type and the given closure
    /// evalutates to `true`, otherwise returns `false`.
    pub fn is_warning_and<'log_entry_warning>(
        &'log_entry_warning self,
        f: impl FnOnce((&'log_entry_warning String, &'log_entry_warning C)) -> bool,
    ) -> bool {
        match self {
            LogEntry::Warning(f0, f1) => f((f0, f1)),
            _ => false,
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// panics.
    pub fn unwrap_warning(self) -> (String, C) {
        match self {
            LogEntry::Warning(f0, f1) => (f0, f1),
            _ => panic!("Failed unwrapping to LogEntry::Warning. Unexpected variant"),
        }
    }
    
    /// Returns a reference to the inner data, if the enum value is of the expected type,
    /// otherwise panics.
    pub fn unwrap_warning_as_ref<'log_entry_warning>(
        &'log_entry_warning self,
    ) -> (&'log_entry_warning String, &'log_entry_warning C) {
        match self {
            LogEntry::Warning(f0, f1) => (f0, f1),
            _ => panic!("Failed unwrapping to LogEntry::Warning. Unexpected variant"),
        }
    }
    
    /// Returns a mutable reference to the inner data, if the enum value is of the expected
    /// type, otherwise panics.
    pub fn unwrap_warning_as_mut<'log_entry_warning>(
        &'log_entry_warning mut self,
    ) -> (&'log_entry_warning mut String, &'log_entry_warning mut C) {
        match self {
            LogEntry::Warning(f0, f1) => (f0, f1),
            _ => panic!("Failed unwrapping to LogEntry::Warning. Unexpected variant"),
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// returns the given default value.
    pub fn unwrap_warning_or(self, default: (String, C)) -> (String, C) {
        match self {
            LogEntry::Warning(f0, f1) => (f0, f1),
            _ => default,
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// returns the value that the given closure evaluated to.
    pub fn unwrap_warning_or_else(self, f: impl FnOnce(Self) -> (String, C)) -> (String, C) {
        match self {
            LogEntry::Warning(f0, f1) => (f0, f1),
            some => f(some),
        }
    }
    
    /// Returns `true`, if the enum value is of the expected type, otherwise returns
    /// `false`.
    pub fn is_error(&self) -> bool {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => true,
            _ => false,
        }
    }
    
    /// Returns `true`, if the enum value is of the expected type and the given closure
    /// evalutates to `true`, otherwise returns `false`.
    pub fn is_error_and<'log_entry_error>(
        &'log_entry_error self,
        f: impl FnOnce(LogEntryErrorRef<'log_entry_error, C>) -> bool,
    ) -> bool {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => f(LogEntryErrorRef {
                message,
                context,
                code,
            }),
            _ => false,
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// panics.
    pub fn unwrap_error(self) -> LogEntryError<C> {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => LogEntryError {
                message,
                context,
                code,
            },
            _ => panic!("Failed unwrapping to LogEntry::Error. Unexpected variant"),
        }
    }
    
    /// Returns a reference to the inner data, if the enum value is of the expected type,
    /// otherwise panics.
    pub fn unwrap_error_as_ref<'log_entry_error>(
        &'log_entry_error self,
    ) -> LogEntryErrorRef<'log_entry_error, C> {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => LogEntryErrorRef {
                message,
                context,
                code,
            },
            _ => panic!("Failed unwrapping to LogEntry::Error. Unexpected variant"),
        }
    }
    
    /// Returns a mutable reference to the inner data, if the enum value is of the expected
    /// type, otherwise panics.
    pub fn unwrap_error_as_mut<'log_entry_error>(
        &'log_entry_error mut self,
    ) -> LogEntryErrorMut<'log_entry_error, C> {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => LogEntryErrorMut {
                message,
                context,
                code,
            },
            _ => panic!("Failed unwrapping to LogEntry::Error. Unexpected variant"),
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// returns the given default value.
    pub fn unwrap_error_or(self, default: LogEntryError<C>) -> LogEntryError<C> {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => LogEntryError {
                message,
                context,
                code,
            },
            _ => default,
        }
    }
    
    /// Returns the inner data, if the enum value is of the expected type, otherwise
    /// returns the value that the given closure evaluated to.
    pub fn unwrap_error_or_else(
        self,
        f: impl FnOnce(Self) -> LogEntryError<C>,
    ) -> LogEntryError<C> {
        match self {
            LogEntry::Error {
                message,
                context,
                code,
            } => LogEntryError {
                message,
                context,
                code,
            },
            some => f(some),
        }
    }
}
```
</details>

Even though this code is already more concise, there is still a rough edge. When collecting all the errors, they still are returned as `Vec<LogEntry>` with no guarantee by the type system that this collection would actually contain only errors. We can solve this problem by extracting the associated data of the `LogEntry::Error` variant into a separate struct.

## Using `extract`

Here, the `extract` macro comes into play, which does this automatically for us. We tell the macro to do the extraction for every variant with more than one unnamed field (keyword `Unnamed`) or with named fields (keyword `Named`). This will automatically change our two affected variants into `LogEntry::Warning(LogEntryWarning<C>)` and `LogEntry::Error(LogEntryError<C>)`. We can then use the newly generated type `LogEntryError<C>` to guarantee that actually only errors are returned by `get_errors()`. Note, how the construction of the log entries changes, even though the enum code was not changed by hand.

Additionally, we make use of the method `<variant>_as_ref()` (keyword `VarAsRef`) to make collecting all error entries and unwrapping them more concise. To make the cloning of the automatically generated `LogEntryError<C>` struct work, we add the `extract_derive(Clone)` attribute.

> ⚠️ When combining both macros, `enpow` must be placed _after_ `extract` to work correctly. Also, the normal `derive` must be placed _after_ `extract`;

```rust
use enpow::{enpow, extract}; // ℹ️

/// A log entry
#[extract(Unnamed, Named)] // ℹ️
#[extract_derive(Clone)]   //
#[enpow(VarAsRef)]         //
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
    LogEntry::Warning(LogEntryWarning( // ℹ️
        "There might be an issue here 🤔".into(),
        4,
    )),
    LogEntry::Error(LogEntryError { // ℹ️
        message: "There _was_ an issue 😖".into(),
        context: 4,
        code: -1,
    }),
    LogEntry::Error(LogEntryError { // ℹ️
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

<details>
<summary>ℹ️ Click to reveal generated code</summary>

```rust
#[derive(Clone)]
pub enum LogEntry<C: ToString + Clone> {
    /// A simple note without context
    Note(
        /// Note's message
        String,
    ),
    /// A warning with a given context
    Warning(LogEntryWarning<C>),
    /// An error message with error code and context
    Error(LogEntryError<C>),
}

#[automatically_derived]
#[allow(unused)]
impl<C: ToString + Clone> LogEntry<C> {
    /// Returns a reference to the inner data, if the enum value is of the expected type,
    /// otherwise returns `None`.
    pub fn note_as_ref<'log_entry_note>(&'log_entry_note self) -> Option<&'log_entry_note String> {
        match self {
            LogEntry::Note(f0) => Some(f0),
            _ => None,
        }
    }
    
    /// Returns a mutable reference to the inner data, if the enum value is of the expected
    /// type, otherwise returns `None`.
    pub fn note_as_mut<'log_entry_note>(
        &'log_entry_note mut self,
    ) -> Option<&'log_entry_note mut String> {
        match self {
            LogEntry::Note(f0) => Some(f0),
            _ => None,
        }
    }
    
    /// Returns a reference to the inner data, if the enum value is of the expected type,
    /// otherwise returns `None`.
    pub fn warning_as_ref<'log_entry_warning>(
        &'log_entry_warning self,
    ) -> Option<&'log_entry_warning LogEntryWarning<C>> {
        match self {
            LogEntry::Warning(f0) => Some(f0),
            _ => None,
        }
    }
    
    /// Returns a mutable reference to the inner data, if the enum value is of the expected
    /// type, otherwise returns `None`.
    pub fn warning_as_mut<'log_entry_warning>(
        &'log_entry_warning mut self,
    ) -> Option<&'log_entry_warning mut LogEntryWarning<C>> {
        match self {
            LogEntry::Warning(f0) => Some(f0),
            _ => None,
        }
    }
    
    /// Returns a reference to the inner data, if the enum value is of the expected type,
    /// otherwise returns `None`.
    pub fn error_as_ref<'log_entry_error>(
        &'log_entry_error self,
    ) -> Option<&'log_entry_error LogEntryError<C>> {
        match self {
            LogEntry::Error(f0) => Some(f0),
            _ => None,
        }
    }
    
    /// Returns a mutable reference to the inner data, if the enum value is of the expected
    /// type, otherwise returns `None`.
    pub fn error_as_mut<'log_entry_error>(
        &'log_entry_error mut self,
    ) -> Option<&'log_entry_error mut LogEntryError<C>> {
        match self {
            LogEntry::Error(f0) => Some(f0),
            _ => None,
        }
    }
}

#[derive(Clone)]
/// A warning with a given context
pub struct LogEntryWarning<C: ToString + Clone>(
    /// Warning's message
    pub String,
    /// Context of the warning
    pub C,
);

#[derive(Clone)]
/// An error message with error code and context
pub struct LogEntryError<C: ToString + Clone> {
    /// Error message
    pub message: String,
    /// Context of the error
    pub context: C,
    /// Error code
    pub code: i16,
}
```
</details>

This was just a quick introductory example for understanding the use and usage of this crate. See the macros' documentation for more details.

# Inspiration

While the first plan for this crate was limited to simple `unwrap` methods and alike, the crate [`variantly`](https://crates.io/crates/variantly) was a great inspiration to take this idea way further. It can be seen as an alternative to this crate with partially different feature set.

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