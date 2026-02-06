"""Configuration loading for col-mem."""

import tomllib
from pathlib import Path
from dataclasses import dataclass, field

DEFAULT_CONFIG_PATHS = [
    Path(__file__).parent.parent.parent / "config.toml",  # project root
    Path.home() / ".config" / "col-mem" / "config.toml",
]


@dataclass
class Config:
    db_path: Path
    collection_name: str
    embedding_model: str
    top_k: int
    default_type: str


def load_config(config_path: Path | None = None) -> Config:
    """Load config from TOML file."""
    if config_path:
        paths = [config_path]
    else:
        paths = DEFAULT_CONFIG_PATHS

    for path in paths:
        if path.exists():
            with open(path, "rb") as f:
                data = tomllib.load(f)
            break
    else:
        raise FileNotFoundError(f"No config found in: {paths}")

    db_path = Path(data["database"]["path"]).expanduser()

    return Config(
        db_path=db_path,
        collection_name=data["database"].get("collection_name", "memories"),
        embedding_model=data["embedding"]["model"],
        top_k=data["search"]["top_k"],
        default_type=data.get("metadata", {}).get("default_type", "memory"),
    )
