pub fn to_snake_case(input: &str) -> String {
    // We probably wont need much more than two underscores...
    let mut output = String::with_capacity(input.len() + 2);

    // Turn the input into snake case char by char
    let mut last_uppercase = true;
    let mut last_underscore = false;
    for char in input.chars() {
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

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Tests
///////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    #[test]
    fn to_snake_case() {
        assert_eq!(super::to_snake_case("IpAddress"), "ip_address");
        assert_eq!(super::to_snake_case("TCP"), "tcp");
        assert_eq!(super::to_snake_case("snake_case"), "snake_case");
        assert_eq!(super::to_snake_case("HOME_IP"), "home_ip");
    }
}