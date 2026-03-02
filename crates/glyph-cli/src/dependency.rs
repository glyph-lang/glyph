use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use glyph_core::ast::Module;

use crate::diagnostics::{SourceInfo, format_diagnostic};
use crate::module_loader::{discover_and_parse_modules, module_id_from_path};

use super::{
    DependencySpec, EXIT_COMPILER, GlyphError, GlyphResult, Manifest, resolve_dependency_root,
    usage_err,
};

#[derive(Debug)]
pub(crate) struct ResolvedDependency {
    pub(crate) name: String,
    pub(crate) lib_root: PathBuf,
    pub(crate) lib_module_id: String,
    pub(crate) dependency_names: HashSet<String>,
}

pub(crate) fn resolve_dependencies(
    root: &Path,
    manifest: &Manifest,
    visited: &mut HashSet<PathBuf>,
) -> GlyphResult<Vec<ResolvedDependency>> {
    let mut seen_names = HashMap::new();
    let mut visiting = Vec::new();
    let mut name_stack = Vec::new();
    resolve_dependencies_inner(
        root,
        manifest,
        visited,
        &mut seen_names,
        &mut visiting,
        &mut name_stack,
    )
}

fn resolve_dependencies_inner(
    root: &Path,
    manifest: &Manifest,
    visited: &mut HashSet<PathBuf>,
    seen_names: &mut HashMap<String, PathBuf>,
    visiting: &mut Vec<PathBuf>,
    name_stack: &mut Vec<String>,
) -> GlyphResult<Vec<ResolvedDependency>> {
    let mut resolved = Vec::new();

    let mut deps: Vec<_> = manifest.dependencies.iter().collect();
    deps.sort_by_key(|(name, _)| *name);

    for (dep_name, spec) in deps {
        let dep_path = match spec {
            DependencySpec::Path { path } => path,
        };
        let dep_root = resolve_dependency_root(root, dep_path);
        let canonical = fs::canonicalize(&dep_root).map_err(|e| {
            usage_err(format!(
                "failed to resolve dependency '{}' at {}: {}",
                dep_name,
                dep_root.display(),
                e
            ))
        })?;

        if let Some(existing) = seen_names.get(dep_name) {
            if existing != &canonical {
                return Err(usage_err(format!(
                    "dependency name '{}' refers to multiple paths ({} and {})",
                    dep_name,
                    existing.display(),
                    canonical.display()
                )));
            }
        } else {
            seen_names.insert(dep_name.clone(), canonical.clone());
        }

        if let Some(pos) = visiting.iter().position(|p| p == &canonical) {
            let mut cycle = name_stack[pos..].to_vec();
            cycle.push(dep_name.clone());
            return Err(usage_err(format!(
                "circular dependency detected: {}",
                cycle.join(" -> ")
            )));
        }

        if visited.contains(&canonical) {
            continue;
        }

        let manifest_path = canonical.join("glyph.toml");
        let dep_manifest = super::load_manifest(&manifest_path, &canonical)?;
        let lib = dep_manifest.lib.as_ref().ok_or_else(|| {
            usage_err(format!(
                "dependency '{}' does not define a [lib] target",
                dep_name
            ))
        })?;

        let lib_path = canonical.join(&lib.path);
        let lib_root = lib_path
            .parent()
            .unwrap_or(canonical.as_path())
            .to_path_buf();
        let lib_module_id = module_id_from_path(&lib_root, &lib_path).map_err(|e| GlyphError {
            code: EXIT_COMPILER,
            message: format!(
                "failed to determine lib module for dependency '{}': {}",
                dep_name, e
            ),
        })?;
        let dependency_names = dep_manifest.dependencies.keys().cloned().collect();

        visiting.push(canonical.clone());
        name_stack.push(dep_name.clone());
        let mut nested = resolve_dependencies_inner(
            &canonical,
            &dep_manifest,
            visited,
            seen_names,
            visiting,
            name_stack,
        )?;
        resolved.append(&mut nested);
        visiting.pop();
        name_stack.pop();

        if visited.insert(canonical.clone()) {
            resolved.push(ResolvedDependency {
                name: dep_name.clone(),
                lib_root,
                lib_module_id,
                dependency_names,
            });
        }
    }

    Ok(resolved)
}

pub(crate) fn load_dependency_modules(
    dep: &ResolvedDependency,
) -> GlyphResult<(HashMap<String, Module>, HashMap<String, SourceInfo>)> {
    let load = discover_and_parse_modules(&dep.lib_root).map_err(|e| GlyphError {
        code: EXIT_COMPILER,
        message: format!("failed to load dependency '{}' modules: {}", dep.name, e),
    })?;
    if !load.diagnostics.is_empty() {
        for diag in &load.diagnostics {
            eprintln!("{}", format_diagnostic(diag, &load.sources));
        }
        return Err(GlyphError {
            code: EXIT_COMPILER,
            message: "build failed due to compiler diagnostics".to_string(),
        });
    }

    let mut modules = HashMap::new();
    let mut sources = HashMap::new();
    let mut known_dependencies = dep.dependency_names.clone();
    known_dependencies.insert(dep.name.clone());

    for (module_id, mut module) in load.modules {
        rewrite_imports(&mut module, &dep.name, &known_dependencies);
        let prefixed_id = format!("{}/{}", dep.name, module_id);
        if let Some(source) = load.sources.get(&module_id) {
            sources.insert(
                prefixed_id.clone(),
                SourceInfo::new(source.path.clone(), source.source.clone()),
            );
        }
        modules.insert(prefixed_id, module);
    }

    let prefixed_lib_id = format!("{}/{}", dep.name, dep.lib_module_id);
    let lib_module = modules
        .get(&prefixed_lib_id)
        .cloned()
        .ok_or_else(|| GlyphError {
            code: EXIT_COMPILER,
            message: format!(
                "dependency '{}' missing lib module '{}'",
                dep.name, prefixed_lib_id
            ),
        })?;
    let lib_source = sources
        .get(&prefixed_lib_id)
        .map(|s| SourceInfo::new(s.path.clone(), s.source.clone()))
        .ok_or_else(|| GlyphError {
            code: EXIT_COMPILER,
            message: format!(
                "dependency '{}' missing source info for '{}'",
                dep.name, prefixed_lib_id
            ),
        })?;

    if modules.contains_key(&dep.name) {
        return Err(GlyphError {
            code: EXIT_COMPILER,
            message: format!("dependency '{}' aliases to an existing module id", dep.name),
        });
    }
    if sources.contains_key(&dep.name) {
        return Err(GlyphError {
            code: EXIT_COMPILER,
            message: format!("dependency '{}' aliases to an existing source id", dep.name),
        });
    }

    modules.insert(dep.name.clone(), lib_module);
    sources.insert(dep.name.clone(), lib_source);

    Ok((modules, sources))
}

pub(crate) fn rewrite_imports(
    module: &mut Module,
    dep_name: &str,
    known_dependencies: &HashSet<String>,
) {
    for import in &mut module.imports {
        let Some(first) = import.path.segments.first() else {
            continue;
        };
        if first == "std" || known_dependencies.contains(first) || first == dep_name {
            continue;
        }
        import.path.segments.insert(0, dep_name.to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use glyph_core::ast::{Import, ImportKind, ImportPath};
    use glyph_core::span::Span;
    use tempfile::TempDir;

    fn write_file(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    fn write_lib_case(dir: &Path, name: &str, deps: &[(&str, &str)]) {
        let manifest_path = dir.join("glyph.toml");
        let mut deps_toml = String::new();
        if !deps.is_empty() {
            deps_toml.push_str("\n[dependencies]\n");
            for (dep_name, dep_path) in deps {
                deps_toml.push_str(&format!("{} = {{ path = \"{}\" }}\n", dep_name, dep_path));
            }
        }

        let toml = format!(
            r#"[package]
name = "{name}"
version = "0.1.0"

[lib]
path = "src/lib.glyph"
{deps}"#,
            name = name,
            deps = deps_toml
        );
        write_file(&manifest_path, &toml);
        write_file(&dir.join("src/lib.glyph"), "fn helper() -> i32 { ret 1 }");
    }

    fn write_bin_case(dir: &Path, name: &str, deps: &[(&str, &str)]) {
        let manifest_path = dir.join("glyph.toml");
        let mut deps_toml = String::new();
        if !deps.is_empty() {
            deps_toml.push_str("\n[dependencies]\n");
            for (dep_name, dep_path) in deps {
                deps_toml.push_str(&format!("{} = {{ path = \"{}\" }}\n", dep_name, dep_path));
            }
        }

        let toml = format!(
            r#"[package]
name = "{name}"
version = "0.1.0"
{deps}

[[bin]]
name = "{name}"
path = "src/main.glyph"
"#,
            name = name,
            deps = deps_toml
        );
        write_file(&manifest_path, &toml);
        write_file(&dir.join("src/main.glyph"), "fn main() -> i32 { ret 0 }");
    }

    #[test]
    fn rewrite_imports_prefixes_internal_modules() {
        let span = Span::new(0, 0);
        let mut module = Module {
            imports: vec![
                Import {
                    kind: ImportKind::Wildcard,
                    path: ImportPath {
                        segments: vec!["utils".into()],
                        span,
                    },
                    span,
                },
                Import {
                    kind: ImportKind::Wildcard,
                    path: ImportPath {
                        segments: vec!["std".into(), "io".into()],
                        span,
                    },
                    span,
                },
                Import {
                    kind: ImportKind::Wildcard,
                    path: ImportPath {
                        segments: vec!["dep_other".into()],
                        span,
                    },
                    span,
                },
                Import {
                    kind: ImportKind::Wildcard,
                    path: ImportPath {
                        segments: vec!["my_lib".into(), "sub".into()],
                        span,
                    },
                    span,
                },
            ],
            items: Vec::new(),
        };

        let mut deps = HashSet::new();
        deps.insert("dep_other".to_string());
        deps.insert("my_lib".to_string());
        rewrite_imports(&mut module, "my_lib", &deps);

        assert_eq!(module.imports[0].path.segments, vec!["my_lib", "utils"]);
        assert_eq!(module.imports[1].path.segments, vec!["std", "io"]);
        assert_eq!(module.imports[2].path.segments, vec!["dep_other"]);
        assert_eq!(module.imports[3].path.segments, vec!["my_lib", "sub"]);
    }

    #[test]
    fn resolve_dependencies_orders_depth_first() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().join("app");
        let dep_a = temp.path().join("dep_a");
        let dep_b = temp.path().join("dep_b");

        fs::create_dir_all(&root).unwrap();
        fs::create_dir_all(&dep_a).unwrap();
        fs::create_dir_all(&dep_b).unwrap();

        write_lib_case(&dep_b, "dep_b", &[]);
        write_lib_case(&dep_a, "dep_a", &[("dep_b", "../dep_b")]);
        write_bin_case(&root, "app", &[("dep_a", "../dep_a")]);

        let manifest = crate::load_manifest(&root.join("glyph.toml"), &root).unwrap();
        let mut visited = HashSet::new();
        let deps = resolve_dependencies(&root, &manifest, &mut visited).unwrap();
        let names: Vec<_> = deps.iter().map(|d| d.name.as_str()).collect();
        assert_eq!(names, vec!["dep_b", "dep_a"]);
    }

    #[test]
    fn resolve_dependencies_rejects_cycles() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().join("app");
        let dep_a = temp.path().join("dep_a");
        let dep_b = temp.path().join("dep_b");

        fs::create_dir_all(&root).unwrap();
        fs::create_dir_all(&dep_a).unwrap();
        fs::create_dir_all(&dep_b).unwrap();

        write_lib_case(&dep_a, "dep_a", &[("dep_b", "../dep_b")]);
        write_lib_case(&dep_b, "dep_b", &[("dep_a", "../dep_a")]);
        write_bin_case(&root, "app", &[("dep_a", "../dep_a")]);

        let manifest = crate::load_manifest(&root.join("glyph.toml"), &root).unwrap();
        let mut visited = HashSet::new();
        let err = resolve_dependencies(&root, &manifest, &mut visited).unwrap_err();
        assert!(
            err.message.contains("circular dependency detected"),
            "err: {}",
            err.message
        );
    }
}
