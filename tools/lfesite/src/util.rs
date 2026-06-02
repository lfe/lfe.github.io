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
    let s: String = s.chars().filter(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || *c == '-').collect();
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
}
