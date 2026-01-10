pub fn inherent_method_symbol(struct_name: &str, method_name: &str) -> String {
    format!("{}::{}", struct_name, method_name)
}

pub fn interface_method_symbol(struct_name: &str, interface: &str, method_name: &str) -> String {
    format!("{}::{}::{}", struct_name, interface, method_name)
}
