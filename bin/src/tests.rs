use hestia_lib::*;
use std::fs;

fn main() {
    let contents: String = match fs::read_to_string("../tests.txt") {
        Ok(s) => s,
        Err(e) => panic!("could not open file `tests.txt`: {}", e),
    };
    // name the tests?
    for test in contents.split("---\n") {
        // println!("test: {:?}", test);
        let split_test: Vec<&str> = test.split("\n-").collect();

        let name = split_test.first().expect("invalid test ?: missing name");
        let got = split_test
            .get(1)
            .unwrap_or_else(|| panic!("invalid test `{}`: missing `got`", name));
        let mut got_evaluator = match evaluator::Evaluator::loaded() {
            Ok(evalr) => evalr,
            Err(e) => panic!("failed to load stdlib: {}", e),
        };
        let got_parsed = parser::parse(got.to_string())
            .unwrap_or_else(|e| panic!("parsing error in `{}`: {}", name, e));
        let mut got_result = None;
        for expr in got_parsed {
            got_result = Some(got_evaluator.eval_top(expr).unwrap());
        }
        let mut expected_evaluator = match evaluator::Evaluator::loaded() {
            Ok(evalr) => evalr,
            Err(e) => panic!("failed to load stdlib: {}", e),
        };
        let expected = split_test
            .get(2)
            .unwrap_or_else(|| panic!("invalid test `{}`: missing `expected`", name));
        let expected_parsed = parser::parse(expected.to_string()).unwrap();
        let mut expected_result = None;
        for expr in expected_parsed {
            expected_result = Some(
                expected_evaluator
                    .eval_top(expr)
                    .unwrap_or_else(|e| panic!("eval error in `{}`: {}", name, e)),
            );
        }
        match (got_result, expected_result) {
            (Some(got), Some(expected)) => {
                if got != expected {
                    panic!("test `{}`: got `{}`, expected `{}`", name, got, expected);
                }
            }
            (None, _) => panic!("invalid test `{}`: `got` evaluated to nothing", name),
            (_, None) => panic!("invalid test `{}`: `expected` evaluated to nothing", name),
        }
    }
}
