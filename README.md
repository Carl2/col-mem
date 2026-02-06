# col-mem

Persistent, semantically-searchable memory for gptel conversations in Emacs.

## Overview

col-mem adds long-term memory to your LLM conversations:

- **Save memories**: Summarize buffer/region → store in vector database
- **Recall memories**: Semantic search → select with consult → inject as context

## Installation

### Python Backend

Requires Python 3.12+ and [uv](https://github.com/astral-sh/uv):

```bash
cd col_mem
uv venv
uv pip install -e .
```

### Emacs Package

```elisp
(add-to-list 'load-path "/path/to/col-mem/emacs/")
(require 'col-mem)

;; Configure path to project
(setq col-mem-project-root "/path/to/col-mem/")
```

## CLI Usage

```bash
# Store a memory
uv run col-mem store '{"title": "...", "summary": "...", "project": "my_proj"}'

# Search memories
uv run col-mem search "query" --top-k 5 --project my_proj

# List all memories
uv run col-mem list

# Delete a memory
uv run col-mem delete <id>
```

## Configuration

Edit `config.toml`:

```toml
[database]
path = "~/.local/share/col_mem/chroma_db"
collection_name = "memories"

[embedding]
model = "all-MiniLM-L6-v2"

[search]
top_k = 10

[metadata]
default_type = "general"
```

## Architecture

```
User writes/chats in Emacs
         │
         ▼
   ┌─────────────┐      ┌─────────────┐      ┌────────────┐
   │ col-mem-save│ ───> │ LLM Summary │ ───> │  ChromaDB  │
   └─────────────┘      └─────────────┘      └────────────┘
                                                   │
   ┌─────────────┐      ┌─────────────┐            │
   │ User Query  │ ───> │   Search    │ <──────────┘
   └─────────────┘      └─────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ Consult picker  │
                    └─────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ gptel + context │ ───> LLM response
                    └─────────────────┘
```

## License

MIT
