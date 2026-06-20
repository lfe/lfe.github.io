/// Generate a URL-safe slug from a string.
///
/// 1. Lowercase
/// 2. Replace spaces and underscores with hyphens
/// 3. Strip everything that isn't `[a-z0-9-]`
/// 4. Collapse consecutive hyphens
/// 5. Trim leading/trailing hyphens
#[must_use]
pub fn slugify(input: &str) -> String {
    let s = input.to_lowercase();
    let s = s.replace([' ', '_'], "-");
    let s: String = s
        .chars()
        .filter(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '-')
        .collect();
    let mut result = String::new();
    let mut prev_hyphen = false;
    for c in s.chars() {
        if c == '-' {
            if !prev_hyphen {
                result.push('-');
            }
            prev_hyphen = true;
        } else {
            result.push(c);
            prev_hyphen = false;
        }
    }
    result.trim_matches('-').to_string()
}

/// Split content into (front-matter text, body text).
///
/// Front-matter is the YAML between `---` delimiters at the top of
/// a markdown file. Returns `None` if the file doesn't start with `---`.
pub fn split_front_matter(content: &str) -> Option<(String, &str)> {
    let trimmed = content.trim_start();
    let rest = trimmed.strip_prefix("---")?;
    let end = rest.find("\n---")?;
    let fm = rest[..end].to_string();
    let body_start = end + 4; // skip \n---
    let body = if body_start < rest.len() && rest.as_bytes()[body_start] == b'\n' {
        &rest[body_start + 1..]
    } else {
        &rest[body_start..]
    };
    Some((fm, body))
}

/// Replace the YAML node at `key` within front-matter text, preserving every
/// other byte exactly.
///
/// Handles both shapes the node can take on disk:
///
/// - an inline scalar / `null` value (`  written_for: null`) — the single line
///   is replaced;
/// - a multi-line child block (`  written_for:` followed by more-indented
///   `lfe:`/`erlang:` lines) — the key line and all of its children are
///   replaced.
///
/// `key` is matched at whatever indentation it appears at; `new_block` is the
/// full replacement text including the key line and its indentation (a trailing
/// newline is optional). Returns `None` if `key` is not present.
///
/// This is deliberately a targeted textual splice rather than a full
/// parse/serialize round-trip, which would reorder keys, drop comments, and
/// reformat unrelated front-matter.
#[must_use]
pub fn replace_yaml_block(front_matter: &str, key: &str, new_block: &str) -> Option<String> {
    let lines: Vec<&str> = front_matter.split_inclusive('\n').collect();
    let needle = format!("{key}:");

    let start = lines
        .iter()
        .position(|line| line.trim_start().starts_with(&needle))?;

    let key_line = lines[start];
    let key_indent = indent_width(key_line);

    // Everything after `key:` on the key line; non-empty means an inline value.
    let after_colon = key_line.trim_start()[needle.len()..].trim();
    let end = if after_colon.is_empty() {
        // Block value: consume following lines indented deeper than the key.
        let mut e = start + 1;
        while e < lines.len() {
            let line = lines[e];
            if line.trim().is_empty() || indent_width(line) <= key_indent {
                break;
            }
            e += 1;
        }
        e
    } else {
        start + 1
    };

    let mut out = String::new();
    out.push_str(&lines[..start].concat());
    out.push_str(new_block.strip_suffix('\n').unwrap_or(new_block));
    out.push('\n');
    out.push_str(&lines[end..].concat());
    Some(out)
}

/// Count leading space characters on a line (front-matter uses spaces only).
fn indent_width(line: &str) -> usize {
    line.chars().take_while(|c| *c == ' ').count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        assert_eq!(slugify("Hello World"), "hello-world");
    }

    #[test]
    fn test_unicode_stripped() {
        assert_eq!(slugify("LFE Friday — timer:tc/3"), "lfe-friday-timertc3");
    }

    #[test]
    fn test_underscores() {
        assert_eq!(slugify("my_post_title"), "my-post-title");
    }

    #[test]
    fn test_consecutive_spaces() {
        assert_eq!(slugify("too   many   spaces"), "too-many-spaces");
    }

    #[test]
    fn test_leading_trailing() {
        assert_eq!(slugify(" -hello- "), "hello");
    }

    #[test]
    fn test_empty() {
        assert_eq!(slugify(""), "");
    }

    #[test]
    fn test_digits() {
        assert_eq!(slugify("OTP 27 Changes"), "otp-27-changes");
    }

    #[test]
    fn test_split_front_matter() {
        let input = "---\ntitle: Hello\n---\nBody text\n";
        let (fm, body) = split_front_matter(input).unwrap();
        assert_eq!(fm, "\ntitle: Hello");
        assert_eq!(body, "Body text\n");
    }

    #[test]
    fn test_split_front_matter_no_delimiters() {
        assert!(split_front_matter("No front matter here").is_none());
    }

    const NULL_FM: &str = "\
title: \"X\"
data:
  author: duncan-mcgreggor
  written_for: null
  last_validated: null
  cover_image: \"/img.png\"
";

    const BLOCK_FM: &str = "\
title: \"X\"
data:
  author: duncan-mcgreggor
  written_for:
    lfe: \"0.9\"
    erlang: \"17\"
  last_validated: null
  cover_image: \"/img.png\"
";

    // Note: written with explicit \n rather than a multi-line literal because
    // a `\`-continuation would strip the leading indentation off the first line.
    const NEW_BLOCK: &str = "  written_for:\n    lfe: \"2.2.0\"\n    erlang: \"28\"";

    #[test]
    fn test_replace_yaml_block_inline_null_to_block() {
        let out = replace_yaml_block(NULL_FM, "written_for", NEW_BLOCK).unwrap();
        assert!(out.contains("  written_for:\n    lfe: \"2.2.0\"\n    erlang: \"28\"\n"));
        // Siblings are untouched.
        assert!(out.contains("  author: duncan-mcgreggor\n"));
        assert!(out.contains("  last_validated: null\n"));
        assert!(out.contains("  cover_image: \"/img.png\"\n"));
        assert!(!out.contains("written_for: null"));
    }

    #[test]
    fn test_replace_yaml_block_block_to_block() {
        let out = replace_yaml_block(BLOCK_FM, "written_for", NEW_BLOCK).unwrap();
        assert!(out.contains("    lfe: \"2.2.0\"\n    erlang: \"28\"\n"));
        assert!(!out.contains("0.9"));
        assert!(!out.contains("erlang: \"17\""));
        // The following sibling key survived the child-block consumption.
        assert!(out.contains("  last_validated: null\n"));
        assert!(out.contains("  cover_image: \"/img.png\"\n"));
    }

    #[test]
    fn test_replace_yaml_block_missing_key() {
        assert!(replace_yaml_block(NULL_FM, "nonexistent", NEW_BLOCK).is_none());
    }
}
