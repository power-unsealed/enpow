#### Method Names

With the argument `method_name`, the standard way of naming the generated structs can be changed. The usage is `method_name="my_new_name"`. The given name will be used instead of the variant's name turned into snake_case. Note, that this argument can only be applied to variants directly, not to the enum itself.

```rust
# use enpow::{enpow, extract};

#[extract(All)]
#[enpow(UnwrapVar)]
#[inner(type_names="Ip{var}", derive(Debug, PartialEq))]
#[derive(Debug, PartialEq)]
enum IpAddress {
    None,
    #[inner(method_name="ipv4")]
    V4(u8, u8, u8, u8),
    #[inner(method_name="ipv6")]
    V6(String),
    Multi {
        v4: (u8, u8, u8, u8),
        v6: String,
    },
}

assert_eq!(IpAddress::from(IpV4(192, 168, 0, 1)).unwrap_ipv4(), IpV4(192, 168, 0, 1));
assert_eq!(IpAddress::from(IpV6("::1".into())).unwrap_ipv6(), IpV6("::1".into()));
```