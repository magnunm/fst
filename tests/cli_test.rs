use std::fs::File;
use std::io::Write;
use assert_cmd::Command;

#[test]
fn cli_test() {
    let test_dir = tempfile::tempdir().unwrap();
    let test_file_path = test_dir.path().join("temp-test-file.txt");
    let mut test_file = File::create(&test_file_path).unwrap();

    write!(test_file, "app.example.com
example.com/index.html
example.com/assets/image.png
example.com/api?some=arg
example.com.somesite.xyz
somesite.xyz/example.com").unwrap();

    // Basic search for substring
    Command::cargo_bin("fst").unwrap()
        .arg("api")
        .arg(&test_file_path)
        .assert()
        .success()
        .stdout("example.com/api?some=arg\n");

    // Regex search
    Command::cargo_bin("fst").unwrap()
        .arg(r"^[^/]*example\.com(/.*)*$")
        .arg(&test_file_path)
        .assert()
        .success()
        .stdout("app.example.com
example.com/index.html
example.com/assets/image.png
example.com/api?some=arg
");

    // Check that the different operations work as expected.
    Command::cargo_bin("fst").unwrap()
        .arg("-o")
        .arg("m")
        .arg("api")
        .arg(&test_file_path)
        .assert()
        .success()
        .stdout("api\n");

    Command::cargo_bin("fst").unwrap()
        .arg("-o")
        .arg("im")
        .arg("api")
        .arg(&test_file_path)
        .assert()
        .success()
        .stdout("example.com/?some=arg\n");

    Command::cargo_bin("fst").unwrap()
        .arg("-o")
        .arg("ip")
        .arg(r"^[^/]*example\.com(/.*)*$")
        .arg(&test_file_path)
        .assert()
        .success()
        .stdout("example.com.somesite.xyz
somesite.xyz/example.co
");  // FIXME: Why is the last "m" removed? Can not reproduce.

    Command::cargo_bin("fst").unwrap()
        .arg("-o")
        .arg("c")
        .arg(r"^[^/]*example\.com(/.*)*$")
        .arg(&test_file_path)
        .assert()
        .success()
        .stdout("4\n");

    drop(test_file);
    test_dir.close().unwrap();
}

#[test]
fn recursive() {
    // Search for a random string, so the only match is in this file
    let find_me = "yLZLdG67QJAuVycOuwXo";
    Command::cargo_bin("fst").unwrap()
        .arg("-r")
        .arg(find_me)
        .arg("tests")
        .assert()
        .success()
        .stdout(format!("tests/cli_test.rs:    let find_me = \"{}\";\n", find_me));
}

#[test]
fn grouping_vs_alteration_precedence() {
    let command_with_grouping = Command::cargo_bin("fst").unwrap()
        .arg("(version|checksum) =")
        .arg("Cargo.lock")
        .output()
        .unwrap();
    let command_without_grouping = Command::cargo_bin("fst").unwrap()
        .arg("version|checksum =")
        .arg("Cargo.lock")
        .output()
        .unwrap();

    assert_eq!(command_with_grouping.stdout, command_without_grouping.stdout);
}
