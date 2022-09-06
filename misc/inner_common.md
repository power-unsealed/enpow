### Configuration

The helper attribute `inner` is used to configure the main macros `expand` and `enpow`. For this, `inner` can be attached to both the enum itself and to individual variants. However, it has to be placed after each main macro it should be effective for. An `inner` macro placed after multiple main macros will be effective for each one of them.

#### Derives

The argument `derive()` enables to add auto trait derives to the automatically generated types. The position of the attribute decides on whether the given traits are implemented for all variant types or just for the selected ones.

> ℹ️ `Ref` structs always automatically derive `Clone` and `Copy`, while `Mut` structs are prohibited from deriving these traits. This exclusion will be handled automatically by the macro.

```rust
# use enpow::{enpow, extract};
#
#[extract(Unit, Single, Unnamed)]
#[enpow(UnwrapVar, VarAsRef)]
#[inner(derive(Debug, PartialEq))]
enum IpAddress {
    None,
    V4(u8, u8, u8, u8),
    V6(String),
    #[inner(derive(Clone))]
    Multi {
        v4: (u8, u8, u8, u8),
        v6: String,
    },
}

// Using PartialEq, Debug, and Clone derive
assert_eq!(
    IpAddress::Multi { v4: (0, 0, 0, 0), v6: "::".into() }.unwrap_multi(),
    IpAddressMulti { v4: (0, 0, 0, 0), v6: "::".into() }.clone()
);

// Using automatic Copy derive on Ref struct
let ip = IpAddress::Multi { v4: (0, 0, 0, 0), v6: "::".into() };
let copy = ip.unwrap_multi_as_ref();
let another_copy = copy;
assert_eq!(copy, IpAddressMultiRef { v4: &(0, 0, 0, 0), v6: &"::".into() });
assert_eq!(another_copy, IpAddressMultiRef { v4: &(0, 0, 0, 0), v6: &"::".into() });
```

#### Type Names

With the argument `type_name` or `type_names`, the standard way of naming the generated structs can be changed. There are multiple naming schemes possible, depending on the keyword or string literal provided:

* `type_name=EnumVar` [default] will give each type the name of the enum followed by the name of the corresponding variant, e.g. `IpAddressMulti`
* `type_name=Var` will let each type be named just like its corresponding variant, e.g. `Multi`
* `type_name=VarEnum` will give each type the name of the corresponding variant followed by the name of the enum, e.g. `MultiIpAddress`
* `type_name="My{enum}Var{var}"` will give each type the specified name with occurences of `{enum}` and `{var}` replaced by the name of the enum and the name of the corresponding variant, e.g. `MyIpAddressMulti`

This example generates the types `IpNone`, `IpV4`, `IpV6`, and `IpV4_6`.

```rust
# use enpow::{enpow, extract};

#[extract(All)]
#[enpow(UnwrapVar)]
#[inner(type_names="Ip{var}", derive(Debug, PartialEq))]
#[derive(Debug, PartialEq)]
enum IpAddress {
    None,
    V4(u8, u8, u8, u8),
    V6(String),
    #[inner(type_name="IpV4_6")]
    Multi {
        v4: (u8, u8, u8, u8),
        v6: String,
    },
}

assert_eq!(IpAddress::from(IpNone).unwrap_none(), IpNone);
assert_eq!(IpAddress::from(IpV4(192, 168, 0, 1)).unwrap_v4(), IpV4(192, 168, 0, 1));
assert_eq!(IpAddress::from(IpV6("::1".into())).unwrap_v6(), IpV6("::1".into()));
assert_eq!(
    IpAddress::from(IpV4_6 { v4: (0, 0, 0, 0), v6: "::1".into() }).unwrap_multi(),
    IpV4_6 { v4: (0, 0, 0, 0), v6: "::1".into() }
);
```