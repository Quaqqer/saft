#[cfg(test)]
mod test {
    use assert_cmd::Command;
    use pretty_assertions::assert_eq;
    use saft_macro::discover_tests;

    #[discover_tests(root = "./crates/saft-tests/res/tests", glob = "**/[!_]*.saf")]
    fn test(file_name: &str) {
        let mut cmd = Command::cargo_bin("saft").unwrap();
        cmd.arg(file_name);

        let got = String::from_utf8(cmd.unwrap().stdout)
            .unwrap()
            .lines()
            .map(|line| line.trim())
            .collect::<Vec<_>>()
            .join("\n");

        let mut expected = Vec::new();

        let mut got_output = false;
        for line in std::fs::read_to_string(file_name).unwrap().lines() {
            if let Some(comment) = line.strip_prefix('#').map(|l| l.trim()) {
                if got_output {
                    expected.push(comment.to_string());
                } else if comment == "output:" {
                    got_output = true;
                }
            }
        }

        assert_eq!(got, expected.join("\n"));
    }
}
