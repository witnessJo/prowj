"""Read pyproject.toml for Prowj without importing or running project code."""
import json
import re
import sys
import tomllib


def commands(data):
    """Return (label, argv) pairs for explicitly declared project commands."""
    project = data.get("project", {})
    uv = data.get("tool", {}).get("uv", {})
    if uv.get("managed") is False:
        return []
    result = [("uv: sync", ["uv", "sync"])]
    for name, entry in project.get("scripts", {}).items():
        if isinstance(entry, str):
            result.append((f"uv: {name}", ["uv", "run", "--", name]))

    groups = data.get("dependency-groups", {})

    def dependencies(items, seen=frozenset()):
        for item in items:
            if isinstance(item, str):
                match = re.match(r"\s*([A-Za-z0-9][A-Za-z0-9._-]*)", item)
                if match:
                    yield re.sub(r"[-_.]+", "-", match[1]).lower()
            elif isinstance(item, dict):
                group = item.get("include-group")
                if group in groups and group not in seen:
                    yield from dependencies(groups[group], seen | {group})

    contexts = [((), project.get("dependencies", []))]
    contexts += [(("--group", name), values) for name, values in groups.items()]
    contexts += [(("--group", "dev"), uv.get("dev-dependencies", []))]
    contexts += [(("--extra", name), values)
                 for name, values in project.get("optional-dependencies", {}).items()]
    found = set()
    for flags, values in contexts:
        for name in dependencies(values):
            if name not in ("pytest", "ruff") or name in found:
                continue
            found.add(name)
            prefix = ["uv", "run", *flags, "--"]
            if name == "pytest":
                result.append(("uv: pytest", prefix + ["pytest"]))
            else:
                result.extend([
                    ("uv: ruff check", prefix + ["ruff", "check", "."]),
                    ("uv: ruff format", prefix + ["ruff", "format", "."]),
                ])
    return result


if __name__ == "__main__":
    try:
        with open(sys.argv[1], "rb") as source:
            print(json.dumps(commands(tomllib.load(source))))
    except (OSError, ValueError, TypeError, AttributeError):
        sys.exit(1)
