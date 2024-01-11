export SHELL := bash
.SHELLFLAGS := -e -o pipefail -c

test:
	cd lib && cargo test --verbose
	cd lib && cargo fmt --all -- --check
	cd lib && cargo clippy --all-targets --all-features -- -D warnings

bin-tests:
	cd bin && cargo run --bin tests

run:
	cd bin && cargo run
