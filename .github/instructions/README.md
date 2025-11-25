# Copilot Instructions Directory

Topic-specific instruction files for GitHub Copilot code review and coding agent.

## Structure

- **Main file**: `../.github/copilot-instructions.md` - High-level, repo-wide guidelines
- **Topic files**: Individual files for specific concerns with targeted `applyTo` rules

## Topic Files

| File | Lines | Applies To | Purpose |
|------|-------|------------|---------|
| `r-coding-standards.instructions.md` | ~100 | `**/*.R` | R code style, naming, documentation |
| `testing.instructions.md` | ~120 | `tests/**/*` | Test patterns, testthat usage |
| `package-development.instructions.md` | ~140 | `**/*` (excludes code review) | Dev workflow, commands |

## File Format

Each file follows this structure:

```yaml
---
applyTo: "file/pattern/**/*"
# or
excludeAgent: copilot_code_review
---

# Title

## Purpose & Scope

Brief description of what this file covers.

---

## Section 1

Guidelines...

## Section 2

More guidelines...

---

## Code Examples

```language
// Examples...
```
```

## Best Practices

✅ **Keep files concise** - Each file under 250 lines (max 1000)

✅ **Clear structure** - Use headings, bullets, and sections

✅ **Show examples** - Include correct and incorrect patterns

✅ **Be direct** - Short, imperative rules

✅ **Path-specific** - Use `applyTo` frontmatter for targeted application

✅ **No external links** - Copilot won't follow them

✅ **No vague language** - Avoid "be more accurate", "identify all issues"

## Maintenance

When updating these files:

1. **Keep the same structure and format**
2. **Ensure `applyTo` patterns are correct** - Target only relevant files
3. **Add examples for new concepts** - Include code blocks showing usage
4. **Keep files focused on their specific topic** - Don't mix concerns
5. **Update the main `copilot-instructions.md`** if adding new topic files

### What NOT to Do

❌ Don't include external links - Copilot won't follow them

❌ Don't use vague language - Avoid "be more accurate", "identify all issues"

❌ Don't create long dense paragraphs - Use bullets and short sections

❌ Don't try to change Copilot's UX - Can't change comment formatting, fonts, etc.
