#[cfg(test)]
mod test {
    use assert_cmd::Command;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_simple() {
        let mut cmd = Command::cargo_bin("saft").unwrap();
        cmd.arg("res/tests/test.saf");

        let output = cmd.unwrap();
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "asd\n");
    }
}
