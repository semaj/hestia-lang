export SHELL := bash
.SHELLFLAGS := -e -o pipefail -c

test:
	cargo test --verbose
	cargo fmt --all -- --check
	cargo clippy -- -D warnings
