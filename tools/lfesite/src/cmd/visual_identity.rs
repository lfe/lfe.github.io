//! Source-backed projection for the draft visual-identity reference.
//!
//! This intentionally understands only the flat color declarations in the
//! site's `:root` and `.dark` blocks. It is not a general CSS parser.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

use anyhow::{bail, Context, Result};
use serde_yaml::{Mapping, Value};

const OUTPUT: &str = "_data/visual_identity_palette.yml";
const NOTES: &str = "_data/visual_identity.yml";

/// Refresh the derived palette/role data when the six reference pages exist.
pub fn refresh(source_dir: &Path) -> Result<()> {
    if !source_dir.join("visual-identity").is_dir() {
        return Ok(());
    }

    let project_dir = source_dir
        .parent()
        .context("visual-identity source directory has no project parent")?;
    let palette_path = project_dir.join("tailwind/palette.json");
    let css_path = project_dir.join("tailwind/site.css");
    let notes_path = source_dir.join(NOTES);
    let output_path = source_dir.join(OUTPUT);

    let palette_text = fs::read_to_string(&palette_path)
        .with_context(|| format!("reading palette source {}", palette_path.display()))?;
    // serde_yaml accepts JSON as a YAML subset; no JSON dependency or second parser is needed.
    let palette: Value = serde_yaml::from_str(&palette_text)
        .with_context(|| format!("parsing palette JSON {}", palette_path.display()))?;
    let css = fs::read_to_string(&css_path)
        .with_context(|| format!("reading theme source {}", css_path.display()))?;
    let notes_text = fs::read_to_string(&notes_path)
        .with_context(|| format!("reading human role notes {}", notes_path.display()))?;
    let notes: Value = serde_yaml::from_str(&notes_text)
        .with_context(|| format!("parsing human role notes {}", notes_path.display()))?;

    let projection = project(&palette, &css, &notes)
        .with_context(|| "validating visual-identity source projection")?;
    let rendered = serde_yaml::to_string(&projection)
        .context("serializing visual-identity palette projection")?;

    let current = fs::read_to_string(&output_path).ok();
    if current.as_deref() != Some(rendered.as_str()) {
        fs::write(&output_path, rendered)
            .with_context(|| format!("writing derived data {}", output_path.display()))?;
    }
    Ok(())
}

fn project(palette: &Value, css: &str, notes: &Value) -> Result<Value> {
    let root = parse_declarations(css_block(css, ":root")?, ":root")?;
    let dark = parse_declarations(css_block(css, ".dark")?, ".dark")?;
    let palette_map = as_mapping(palette, "palette root")?;
    let primitives = string_map(required(palette_map, "primitives")?, "palette primitives")?;
    let semantic = as_mapping(required(palette_map, "semantic")?, "palette semantic")?;
    let light_semantic = string_map(required(semantic, "light")?, "palette semantic.light")?;
    let dark_semantic = string_map(required(semantic, "dark")?, "palette semantic.dark")?;

    let primitive_names: BTreeSet<String> =
        primitives.keys().map(|name| format!("--{name}")).collect();
    for (name, expected) in &primitives {
        let variable = format!("--{name}");
        validate_hex(expected).with_context(|| format!("palette primitive {variable}"))?;
        let actual = root
            .get(&variable)
            .with_context(|| format!("theme :root is missing primitive {variable}"))?;
        if actual != expected {
            bail!("palette primitive {variable} is {expected}, but tailwind/site.css declares {actual}");
        }
    }

    if light_semantic.keys().collect::<BTreeSet<_>>()
        != dark_semantic.keys().collect::<BTreeSet<_>>()
    {
        bail!("palette semantic light/dark role keys differ");
    }
    let semantic_names: BTreeSet<String> = light_semantic
        .keys()
        .map(|name| format!("--{name}"))
        .collect();
    for (name, expected) in &light_semantic {
        let variable = format!("--{name}");
        let actual = root
            .get(&variable)
            .with_context(|| format!("theme :root is missing semantic role {variable}"))?;
        if actual != expected {
            bail!("palette light expression for {variable} is {expected}, but theme declares {actual}");
        }
    }
    for (name, expected) in &dark_semantic {
        let variable = format!("--{name}");
        let actual = dark
            .get(&variable)
            .with_context(|| format!("theme .dark is missing semantic role {variable}"))?;
        if actual != expected {
            bail!(
                "palette dark expression for {variable} is {expected}, but theme declares {actual}"
            );
        }
    }

    let exception_names: BTreeSet<String> = root
        .keys()
        .filter(|name| name.starts_with("--lfe-code-") || name.as_str() == "--lfe-on-art")
        .cloned()
        .collect();
    let declared_lfe: BTreeSet<String> = root
        .keys()
        .filter(|name| name.starts_with("--lfe-"))
        .cloned()
        .collect();
    let classified: BTreeSet<String> = primitive_names
        .union(&semantic_names)
        .cloned()
        .chain(exception_names.iter().cloned())
        .collect();
    if declared_lfe != classified {
        let unclassified: Vec<_> = declared_lfe.difference(&classified).cloned().collect();
        let missing: Vec<_> = classified.difference(&declared_lfe).cloned().collect();
        bail!("theme --lfe-* inventory is not classified; unclassified: {unclassified:?}; missing: {missing:?}");
    }

    let role_notes = as_mapping(
        required(as_mapping(notes, "human notes")?, "roles")?,
        "human notes.roles",
    )?;
    let expected_note_keys: BTreeSet<String> = semantic_names
        .union(&exception_names)
        .map(|name| name.trim_start_matches("--").to_owned())
        .collect();
    let actual_note_keys: BTreeSet<String> = role_notes
        .keys()
        .map(|key| key_string(key, "human role note"))
        .collect::<Result<_>>()?;
    if expected_note_keys != actual_note_keys {
        let missing: Vec<_> = expected_note_keys
            .difference(&actual_note_keys)
            .cloned()
            .collect();
        let stale: Vec<_> = actual_note_keys
            .difference(&expected_note_keys)
            .cloned()
            .collect();
        bail!("human role note inventory mismatch; missing: {missing:?}; stale: {stale:?}");
    }
    let mut roles = Vec::new();
    let mut role_groups: BTreeMap<String, Vec<Value>> = BTreeMap::new();
    let mut mode_light = Vec::new();
    let mut mode_dark = Vec::new();
    let mut all_roles: Vec<String> = semantic_names
        .iter()
        .chain(exception_names.iter())
        .cloned()
        .collect();
    all_roles.sort();
    for variable in all_roles {
        let key = variable.trim_start_matches("--");
        let note = role_notes
            .get(Value::String(key.to_owned()))
            .with_context(|| format!("human notes are missing role {variable}"))?;
        let note = as_mapping(note, &format!("human notes.roles.{key}"))?;
        let category = nonempty_string(
            required(note, "category")?,
            &format!("notes for {variable}.category"),
        )?;
        let purpose = nonempty_string(
            required(note, "purpose")?,
            &format!("notes for {variable}.purpose"),
        )?;
        let consumers = nonempty_string(
            required(note, "consumers")?,
            &format!("notes for {variable}.consumers"),
        )?;
        let light_expression = root
            .get(&variable)
            .with_context(|| format!("theme :root is missing role {variable}"))?;
        let dark_expression = dark.get(&variable).unwrap_or(light_expression);
        let light_hex = resolve(&variable, &root, &mut BTreeSet::new())
            .with_context(|| format!("resolving light role {variable}"))?;
        let mut dark_values = root.clone();
        dark_values.extend(dark.clone());
        let dark_hex = resolve(&variable, &dark_values, &mut BTreeSet::new())
            .with_context(|| format!("resolving dark role {variable}"))?;
        let role = map(vec![
            ("name", string(variable.clone())),
            ("key", string(key)),
            (
                "kind",
                string(if semantic_names.contains(&variable) {
                    "site"
                } else {
                    "code/art"
                }),
            ),
            ("category", string(category.clone())),
            ("purpose", string(purpose)),
            ("consumers", string(consumers)),
            ("light_expression", string(light_expression)),
            ("light_hex", string(light_hex)),
            ("dark_expression", string(dark_expression)),
            ("dark_hex", string(dark_hex)),
        ]);
        role_groups.entry(category).or_default().push(role.clone());
        roles.push(role);
        mode_light.push(format!("{variable}: {light_expression};"));
        mode_dark.push(format!("{variable}: {dark_expression};"));
    }

    let mut families = Vec::new();
    let ramps = as_mapping(required(palette_map, "ramps")?, "palette ramps")?;
    let mut family_entries: Vec<_> = ramps.iter().collect();
    let family_order = ["slate", "orange", "gold", "green", "blue", "red", "plum"];
    family_entries.sort_by_key(|(name, _)| {
        let name = name.as_str().unwrap_or("");
        family_order
            .iter()
            .position(|preferred| *preferred == name)
            .unwrap_or(usize::MAX)
    });
    for (family, stops_value) in family_entries {
        let family_name = key_string(family, "palette ramps family")?;
        let stops = as_mapping(stops_value, &format!("palette ramps.{family_name}"))?;
        let mut named_stops = Vec::new();
        for (stop_key, stop_value) in stops {
            let stop = key_string(stop_key, "palette stop")?;
            let data = as_mapping(stop_value, &format!("palette ramps.{family_name}.{stop}"))?;
            let hex = nonempty_string(required(data, "hex")?, "ramp hex")?;
            validate_hex(&hex).with_context(|| format!("ramp {family_name}-{stop}"))?;
            let variable = format!("--lfe-{family_name}-{stop}");
            if root.get(&variable) != Some(&hex) {
                bail!(
                    "palette ramp {variable} is {hex}, but theme :root declares {:?}",
                    root.get(&variable)
                );
            }
            let quantized = triple(
                required(data, "quantized")?,
                &format!("{variable}.quantized"),
            )?;
            let mut fields = vec![
                ("name", string(stop.clone())),
                ("variable", string(variable)),
                ("hex", string(hex)),
                ("oklch", string(format_oklch(quantized))),
                (
                    "chroma_reduction",
                    number_or_string(data.get(Value::String("chromaReduction".into()))),
                ),
            ];
            if let Some(base) = data.get(Value::String("baselineHex".into())) {
                fields.push(("baseline_hex", base.clone()));
            }
            named_stops.push(map(fields));
        }
        named_stops.sort_by_key(|stop| {
            stop.get("name")
                .and_then(Value::as_str)
                .and_then(|s| s.parse::<u16>().ok())
                .unwrap_or(u16::MAX)
        });
        families.push(map(vec![
            ("name", string(family_name.clone())),
            ("title", string(title_case(&family_name))),
            ("stops", Value::Sequence(named_stops)),
        ]));
    }

    let anchors_map = as_mapping(required(palette_map, "anchors")?, "palette anchors")?;
    let mut anchors = Vec::new();
    for (anchor_key, value) in anchors_map {
        let key = key_string(anchor_key, "palette anchor")?;
        let data = as_mapping(value, &format!("palette anchor {key}"))?;
        let hex = nonempty_string(required(data, "hex")?, "anchor hex")?;
        validate_hex(&hex).with_context(|| format!("anchor {key}"))?;
        let variable = format!("--lfe-{key}");
        let resolved = resolve(&variable, &root, &mut BTreeSet::new())
            .with_context(|| format!("resolving anchor {variable}"))?;
        if resolved != hex {
            bail!("palette anchor {variable} is {hex}, but CSS resolves to {resolved}");
        }
        let quantized = triple(
            required(data, "quantized")?,
            &format!("{variable}.quantized"),
        )?;
        anchors.push(map(vec![
            ("name", string(key)),
            ("variable", string(variable)),
            ("hex", string(hex)),
            ("oklch", string(format_oklch(quantized))),
            ("origin", required(data, "origin")?.clone()),
        ]));
    }

    let mut compatibility = Vec::new();
    for (name, expression) in root.iter().filter(|(name, _)| name.starts_with("--gold-")) {
        let light_hex = resolve(name, &root, &mut BTreeSet::new())
            .with_context(|| format!("resolving compatibility alias {name}"))?;
        let mut dark_values = root.clone();
        dark_values.extend(dark.clone());
        let dark_hex = resolve(name, &dark_values, &mut BTreeSet::new())
            .with_context(|| format!("resolving dark compatibility alias {name}"))?;
        compatibility.push(map(vec![
            ("name", string(name)),
            ("expression", string(expression)),
            ("light_hex", string(light_hex)),
            ("dark_hex", string(dark_hex)),
        ]));
    }

    let human_notes = as_mapping(notes, "human notes")?;
    let status_notes = required(human_notes, "status_examples")?
        .as_sequence()
        .context("human notes.status_examples must be a sequence")?;
    let mut status_examples = Vec::new();
    let mut seen_statuses = BTreeSet::new();
    let mut dark_values = root.clone();
    dark_values.extend(dark.clone());
    for (index, sample) in status_notes.iter().enumerate() {
        let sample = as_mapping(sample, &format!("status example {index}"))?;
        let name = nonempty_string(required(sample, "name")?, "status example.name")?;
        if !seen_statuses.insert(name.clone()) {
            bail!("duplicate status example {name}");
        }
        let mut fields = vec![
            ("name".to_owned(), string(name.clone())),
            (
                "label".to_owned(),
                string(nonempty_string(
                    required(sample, "label")?,
                    "status example.label",
                )?),
            ),
            (
                "title".to_owned(),
                string(nonempty_string(
                    required(sample, "title")?,
                    "status example.title",
                )?),
            ),
            (
                "body".to_owned(),
                string(nonempty_string(
                    required(sample, "body")?,
                    "status example.body",
                )?),
            ),
        ];
        for part in ["text", "surface", "boundary"] {
            let variable = format!("--lfe-{name}-{part}");
            let expression = root
                .get(&variable)
                .with_context(|| format!("status example {name} has no role {variable}"))?;
            let dark_expression = dark.get(&variable).unwrap_or(expression);
            let light_hex = resolve(&variable, &root, &mut BTreeSet::new())
                .with_context(|| format!("resolving light status role {variable}"))?;
            let dark_hex = resolve(&variable, &dark_values, &mut BTreeSet::new())
                .with_context(|| format!("resolving dark status role {variable}"))?;
            fields.push((format!("light_{part}_hex"), string(light_hex)));
            fields.push((format!("dark_{part}_hex"), string(dark_hex)));
            fields.push((format!("light_{part}_expression"), string(expression)));
            fields.push((format!("dark_{part}_expression"), string(dark_expression)));
        }
        status_examples.push(map_owned(fields));
    }

    let design = as_mapping(required(palette_map, "design")?, "palette design")?;
    let recipe = as_mapping(required(palette_map, "recipe")?, "palette recipe")?;
    let hues = as_mapping(
        required(recipe, "chromaticHues")?,
        "palette recipe.chromaticHues",
    )?;
    let family_hues: Vec<Value> = hues
        .iter()
        .map(|(name, degrees)| {
            Ok(map(vec![
                ("name", string(key_string(name, "family hue")?)),
                ("degrees", degrees.clone()),
            ]))
        })
        .collect::<Result<_>>()?;
    let recipe = as_mapping(required(palette_map, "recipe")?, "palette recipe")?;
    let anchor = as_mapping(required(design, "anchor")?, "palette design.anchor")?;
    let quantized_anchor = triple(required(anchor, "oklch")?, "design anchor oklch")?;
    let mut light_declarations = mode_light.join(" ");
    light_declarations.push(' ');
    let mut dark_declarations = mode_dark.join(" ");
    dark_declarations.push(' ');

    Ok(map(vec![
        (
            "design",
            map(vec![
                ("commit", required(design, "commit")?.clone()),
                ("candidate", required(design, "candidate")?.clone()),
                ("artifact", required(design, "artifact")?.clone()),
                ("anchor_hex", required(anchor, "hex")?.clone()),
                ("anchor_oklch", string(format_oklch(quantized_anchor))),
            ]),
        ),
        (
            "recipe",
            map(vec![
                ("chromatic_hues", required(recipe, "chromaticHues")?.clone()),
                ("profile", required(recipe, "profile")?.clone()),
                ("gamut", required(recipe, "gamut")?.clone()),
                ("slate", required(recipe, "slate")?.clone()),
                ("slate_source", required(recipe, "slateSource")?.clone()),
                ("conversion", required(recipe, "conversion")?.clone()),
            ]),
        ),
        ("families", Value::Sequence(families)),
        ("anchors", Value::Sequence(anchors)),
        ("roles", Value::Sequence(roles)),
        (
            "role_groups",
            Value::Sequence(
                role_groups
                    .into_iter()
                    .map(|(category, roles)| {
                        map(vec![
                            ("category", string(category)),
                            ("roles", Value::Sequence(roles)),
                        ])
                    })
                    .collect(),
            ),
        ),
        ("compatibility", Value::Sequence(compatibility)),
        ("status_examples", Value::Sequence(status_examples)),
        ("family_hues", Value::Sequence(family_hues)),
        (
            "boards",
            map(vec![
                (
                    "light",
                    map(vec![
                        ("label", string("Light mode")),
                        ("declarations", string(light_declarations)),
                    ]),
                ),
                (
                    "dark",
                    map(vec![
                        ("label", string("Dark mode")),
                        ("declarations", string(dark_declarations)),
                    ]),
                ),
            ]),
        ),
    ]))
}

fn css_block<'a>(css: &'a str, selector: &str) -> Result<&'a str> {
    let needle = format!("{selector} {{");
    let matches = css
        .match_indices(&needle)
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    if matches.is_empty() {
        bail!("theme is missing bounded selector `{selector}`");
    }
    if matches.len() != 1 {
        bail!("theme has ambiguous duplicate selector `{selector}`");
    }
    let start = matches[0];
    let open = start + needle.len() - 1;
    let bytes = css.as_bytes();
    let mut depth = 0usize;
    let mut index = open;
    let mut in_comment = false;
    while index < bytes.len() {
        if !in_comment
            && index + 1 < bytes.len()
            && bytes[index] == b'/'
            && bytes[index + 1] == b'*'
        {
            in_comment = true;
            index += 2;
            continue;
        }
        if in_comment && index + 1 < bytes.len() && bytes[index] == b'*' && bytes[index + 1] == b'/'
        {
            in_comment = false;
            index += 2;
            continue;
        }
        if !in_comment {
            match bytes[index] {
                b'{' => depth += 1,
                b'}' => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(&css[open + 1..index]);
                    }
                }
                _ => {}
            }
        }
        index += 1;
    }
    bail!("theme selector `{selector}` has an unterminated block")
}

fn parse_declarations(block: &str, selector: &str) -> Result<BTreeMap<String, String>> {
    let mut declarations = BTreeMap::new();
    let mut cleaned = String::with_capacity(block.len());
    let mut chars = block.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '/' && chars.peek() == Some(&'*') {
            chars.next();
            let mut closed = false;
            while let Some(comment_ch) = chars.next() {
                if comment_ch == '*' && chars.peek() == Some(&'/') {
                    chars.next();
                    closed = true;
                    break;
                }
            }
            if !closed {
                bail!("unterminated comment in {selector} color declarations");
            }
            cleaned.push('\n');
        } else {
            cleaned.push(ch);
        }
    }
    for declaration in cleaned.split(';') {
        let declaration = declaration.trim();
        if declaration.is_empty() {
            continue;
        }
        let (name, value) = declaration
            .split_once(':')
            .with_context(|| format!("malformed declaration in {selector}: {declaration}"))?;
        let name = name.trim();
        if !name.starts_with("--") {
            continue;
        }
        let value = value.trim();
        if value.is_empty() {
            bail!("empty color declaration {name} in {selector}");
        }
        if declarations
            .insert(name.to_owned(), value.to_owned())
            .is_some()
        {
            bail!("duplicate declaration {name} in {selector}");
        }
    }
    Ok(declarations)
}

fn resolve(
    name: &str,
    declarations: &BTreeMap<String, String>,
    seen: &mut BTreeSet<String>,
) -> Result<String> {
    if !seen.insert(name.to_owned()) {
        bail!("color alias cycle at {name}");
    }
    let expression = declarations
        .get(name)
        .with_context(|| format!("missing color alias {name}"))?;
    if is_hex(expression) {
        return Ok(expression.to_ascii_lowercase());
    }
    let inner = expression
        .strip_prefix("var(")
        .and_then(|value| value.strip_suffix(')'))
        .map(str::trim)
        .with_context(|| format!("unsupported color expression for {name}: {expression}"))?;
    if !inner.starts_with("--") || inner.contains(',') || inner.chars().any(char::is_whitespace) {
        bail!("unsupported color expression for {name}: {expression}");
    }
    resolve(inner, declarations, seen)
}

fn is_hex(value: &str) -> bool {
    value.len() == 7
        && value.starts_with('#')
        && value.as_bytes()[1..].iter().all(u8::is_ascii_hexdigit)
}

fn validate_hex(value: &str) -> Result<()> {
    if !is_hex(value) {
        bail!("expected a six-digit sRGB hex color, got `{value}`");
    }
    Ok(())
}

fn triple(value: &Value, context: &str) -> Result<[f64; 3]> {
    let seq = value
        .as_sequence()
        .with_context(|| format!("{context} must be an array"))?;
    if seq.len() != 3 {
        bail!("{context} must contain L, C, and h");
    }
    let mut values = [0.0; 3];
    for (index, item) in seq.iter().enumerate() {
        values[index] = item
            .as_f64()
            .with_context(|| format!("{context}[{index}] must be numeric"))?;
    }
    Ok(values)
}

fn format_oklch(values: [f64; 3]) -> String {
    format!(
        "L {:.3} · C {:.3} · h {:.1}°",
        values[0], values[1], values[2]
    )
}

fn number_or_string(value: Option<&Value>) -> Value {
    value.cloned().unwrap_or(Value::Null)
}

fn as_mapping<'a>(value: &'a Value, context: &str) -> Result<&'a Mapping> {
    value
        .as_mapping()
        .with_context(|| format!("{context} must be a mapping"))
}

fn required<'a>(mapping: &'a Mapping, key: &str) -> Result<&'a Value> {
    mapping
        .get(Value::String(key.to_owned()))
        .with_context(|| format!("missing `{key}`"))
}

fn string_map(value: &Value, context: &str) -> Result<BTreeMap<String, String>> {
    let mapping = as_mapping(value, context)?;
    mapping
        .iter()
        .map(|(key, value)| Ok((key_string(key, context)?, nonempty_string(value, context)?)))
        .collect()
}

fn key_string(value: &Value, context: &str) -> Result<String> {
    value
        .as_str()
        .map(str::to_owned)
        .with_context(|| format!("{context} keys must be strings"))
}

fn nonempty_string(value: &Value, context: &str) -> Result<String> {
    let text = value
        .as_str()
        .with_context(|| format!("{context} must be a string"))?;
    if text.trim().is_empty() {
        bail!("{context} must not be empty");
    }
    Ok(text.to_owned())
}

fn map(entries: Vec<(&str, Value)>) -> Value {
    let mut mapping = Mapping::new();
    for (key, value) in entries {
        mapping.insert(Value::String(key.to_owned()), value);
    }
    Value::Mapping(mapping)
}

fn map_owned(entries: Vec<(String, Value)>) -> Value {
    let mut mapping = Mapping::new();
    for (key, value) in entries {
        mapping.insert(Value::String(key), value);
    }
    Value::Mapping(mapping)
}

fn string(value: impl Into<String>) -> Value {
    Value::String(value.into())
}

fn title_case(value: &str) -> String {
    let mut chars = value.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    fn fixture() -> (tempfile::TempDir, std::path::PathBuf) {
        let temp = tempdir().unwrap();
        let project = temp.path();
        let source = project.join("src");
        fs::create_dir_all(source.join("visual-identity")).unwrap();
        fs::create_dir_all(source.join("_data")).unwrap();
        fs::create_dir_all(project.join("tailwind")).unwrap();
        fs::write(source.join(NOTES), "status_examples: []\nroles:\n  lfe-canvas:\n    category: Surfaces\n    purpose: Page ground\n    consumers: Site canvas utilities\n  lfe-code-text:\n    category: Code and art\n    purpose: Syntax foreground\n    consumers: Highlighted code blocks\n").unwrap();
        fs::write(project.join("tailwind/palette.json"), r##"{"design":{"commit":"abc","candidate":"test","artifact":"frozen","anchor":{"hex":"#111111","oklch":[0.1,0.01,275.0]}},"recipe":{"chromaticHues":{},"profile":[],"gamut":"test","slate":"test","slateSource":"test","conversion":"test"},"ramps":{"slate":{"50":{"hex":"#ffffff","quantized":[0.99,0.0,0.0]}}},"anchors":{},"primitives":{"lfe-slate-50":"#ffffff","lfe-black":"#111111"},"semantic":{"light":{"lfe-canvas":"var(--lfe-slate-50)"},"dark":{"lfe-canvas":"var(--lfe-black)"}}}"##).unwrap();
        fs::write(project.join("tailwind/site.css"), ":root { --lfe-slate-50: #ffffff; --lfe-black: #111111; --lfe-canvas: var(--lfe-slate-50); --lfe-code-text: #222222; --gold-50: var(--lfe-slate-50); }\n.dark { --lfe-canvas: var(--lfe-black); --lfe-code-text: #eeeeee; }\n").unwrap();
        (temp, source)
    }

    #[test]
    fn projection_resolves_modes_and_keeps_code_exceptions_local() {
        let (_temp, source) = fixture();
        refresh(&source).unwrap();
        let output = fs::read_to_string(source.join(OUTPUT)).unwrap();
        assert!(output.contains("light_hex: '#ffffff'"));
        assert!(output.contains("dark_hex: '#111111'"));
        assert!(output.contains("--lfe-code-text: #eeeeee;"));
        assert!(output.contains("kind: code/art"));
    }

    #[test]
    fn stale_source_mismatch_fails_without_overwriting_projection() {
        let (_temp, source) = fixture();
        refresh(&source).unwrap();
        let output = source.join(OUTPUT);
        let before = fs::read(&output).unwrap();
        let project = source.parent().unwrap();
        fs::write(
            project.join("tailwind/palette.json"),
            fs::read_to_string(project.join("tailwind/palette.json"))
                .unwrap()
                .replace("#ffffff", "#fefefe"),
        )
        .unwrap();
        let error = format!("{:#}", refresh(&source).unwrap_err());
        assert!(error.contains("primitive"));
        assert_eq!(fs::read(output).unwrap(), before);
    }

    #[test]
    fn missing_alias_and_cycle_fail_with_context() {
        let (_temp, source) = fixture();
        let project = source.parent().unwrap();
        let css = project.join("tailwind/site.css");
        fs::write(&css, ":root { --lfe-slate-50: #ffffff; --lfe-black: #111111; --lfe-canvas: var(--missing); --lfe-code-text: #222222; }\n.dark { --lfe-canvas: var(--lfe-black); --lfe-code-text: #eeeeee; }\n").unwrap();
        let palette = project.join("tailwind/palette.json");
        fs::write(
            &palette,
            fs::read_to_string(&palette)
                .unwrap()
                .replace("var(--lfe-slate-50)", "var(--missing)"),
        )
        .unwrap();
        let error = format!("{:#}", refresh(&source).unwrap_err());
        assert!(error.contains("missing color alias --missing"));
        fs::write(
            &palette,
            fs::read_to_string(&palette)
                .unwrap()
                .replace("var(--missing)", "var(--lfe-canvas)"),
        )
        .unwrap();
        fs::write(&css, ":root { --lfe-slate-50: #ffffff; --lfe-black: #111111; --lfe-canvas: var(--lfe-canvas); --lfe-code-text: #222222; }\n.dark { --lfe-canvas: var(--lfe-black); --lfe-code-text: #eeeeee; }\n").unwrap();
        let error = format!("{:#}", refresh(&source).unwrap_err());
        assert!(error.contains("color alias cycle"));
    }

    #[test]
    fn refreshed_output_tracks_consistent_source_changes_and_is_stable() {
        let (_temp, source) = fixture();
        refresh(&source).unwrap();
        let project = source.parent().unwrap();
        let palette_path = project.join("tailwind/palette.json");
        let css_path = project.join("tailwind/site.css");
        fs::write(
            &palette_path,
            fs::read_to_string(&palette_path)
                .unwrap()
                .replace("#ffffff", "#fefefe"),
        )
        .unwrap();
        fs::write(
            &css_path,
            fs::read_to_string(&css_path)
                .unwrap()
                .replace("#ffffff", "#fefefe"),
        )
        .unwrap();
        refresh(&source).unwrap();
        let output = source.join(OUTPUT);
        let after = fs::read(&output).unwrap();
        assert!(String::from_utf8_lossy(&after).contains("#fefefe"));
        refresh(&source).unwrap();
        assert_eq!(fs::read(output).unwrap(), after);
    }

    #[test]
    fn absent_reference_pages_preserve_existing_prerender_behavior() {
        let temp = tempdir().unwrap();
        let data = temp.path().join("_data");
        fs::create_dir_all(&data).unwrap();
        fs::write(data.join("sample.yml"), "summary_md: A small note.\n").unwrap();
        super::super::prerender::run_with_data_dir(temp.path()).unwrap();
        let rendered = fs::read_to_string(data.join("sample.yml")).unwrap();
        let parsed: Value = serde_yaml::from_str(&rendered).unwrap();
        assert_eq!(
            parsed
                .as_mapping()
                .and_then(|map| map.get(Value::String("summary_html".to_owned())))
                .and_then(Value::as_str),
            Some("<p>A small note.</p>\n")
        );
        assert!(!data.join("visual_identity_palette.yml").exists());
    }
}
