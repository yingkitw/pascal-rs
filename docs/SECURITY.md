# Security Policy

## Supported versions

| Version | Supported |
|---------|-----------|
| 0.1.x   | Yes       |

## Reporting vulnerabilities

Please report security issues privately via GitHub Security Advisories on the repository, not in public issues.

Include:
- Description and impact
- Steps to reproduce
- Affected versions

## Dependency scanning

CI runs `cargo audit` on every push. Run locally:

```bash
cargo install cargo-audit
cargo audit
```
