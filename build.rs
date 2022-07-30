fn main() {
    lalrpop::Configuration::default()
        .process_dir("src/parse")
        .unwrap()
}
