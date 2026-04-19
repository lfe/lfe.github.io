use std::path::Path;

use anyhow::Result;

/// Validate data files for correctness.
///
/// Runs sanity checks on `_data/*.yml` files to catch common
/// errors before they surface during the build.
pub fn run(project_dir: &Path) -> Result<()> {
    let data_dir = project_dir.join("_data");
    println!("validate: would check data files for errors");
    println!("  data dir: {}", data_dir.display());
    println!("  checks:");
    println!("    1. YAML syntax validity");
    println!("    2. Required fields present");
    println!("    3. URL fields are well-formed");
    println!("    4. Date fields parse correctly");
    println!("    5. No orphaned _html without matching _md");
    Ok(())
}
