fn main() {
    cc::Build::new().file("src/external.c").compile("external");
}
