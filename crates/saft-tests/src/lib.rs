#[cfg(test)]
mod test {
    use assert_cmd::Command;
    use pretty_assertions::assert_eq;
    use saft_macro::discover_tests;

    #[discover_tests(path = "./crates/saft-tests/res/tests/**/*.saf.out")]
    fn test(file_name: &str, out_file: &str) {
        let mut cmd = Command::cargo_bin("saft").unwrap();
        cmd.arg(file_name);

        let output = cmd.unwrap();

        let expected_output = std::fs::read_to_string(out_file).unwrap();

        assert_eq!(String::from_utf8(output.stdout).unwrap(), expected_output);
    }
}
