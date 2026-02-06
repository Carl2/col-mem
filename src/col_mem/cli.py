"""CLI entry point for col-mem."""

import argparse
import json
import sys
from .memory import MemoryStore


def cmd_store(args):
    """Store a memory from JSON input."""
    try:
        data = json.loads(args.json_data)
    except json.JSONDecodeError as e:
        print(json.dumps({"error": f"Invalid JSON: {e}"}))
        sys.exit(1)
    
    title = data.get("title")
    summary = data.get("summary")
    
    if not title or not summary:
        print(json.dumps({"error": "title and summary are required"}))
        sys.exit(1)
    
    store = MemoryStore()
    memory_id = store.store(
        title=title,
        summary=summary,
        project=data.get("project"),
        type_=data.get("type"),
        tags=data.get("tags"),
        source=data.get("source"),
    )
    
    print(json.dumps({"id": memory_id, "title": title}))


def cmd_search(args):
    """Search memories."""
    store = MemoryStore()
    
    where = {}
    if args.project:
        where["project"] = args.project
    if args.type:
        where["type"] = args.type
    
    results = store.search(
        query=args.query,
        top_k=args.top_k,
        where=where if where else None,
    )
    
    print(json.dumps({"results": results}))


def cmd_list(args):
    """List all memories."""
    store = MemoryStore()
    
    where = {}
    if args.project:
        where["project"] = args.project
    if args.type:
        where["type"] = args.type
    
    # Get all from collection
    results = store.collection.get(
        where=where if where else None,
        include=["metadatas", "documents"],
    )
    
    memories = []
    for i, id_ in enumerate(results["ids"]):
        meta = results["metadatas"][i]
        tags = meta.get("tags", "")
        memories.append({
            "id": id_,
            "title": meta.get("title", ""),
            "summary": results["documents"][i],
            "project": meta.get("project"),
            "type": meta.get("type"),
            "tags": tags.split(",") if tags else [],
            "source": meta.get("source"),
            "created_at": meta.get("created_at"),
        })
    
    print(json.dumps({"memories": memories}))


def cmd_delete(args):
    """Delete a memory by ID."""
    store = MemoryStore()
    
    # Check if exists
    existing = store.get(args.id)
    if not existing:
        print(json.dumps({"error": f"Memory not found: {args.id}"}))
        sys.exit(1)
    
    store.collection.delete(ids=[args.id])
    print(json.dumps({"deleted": args.id}))


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        prog="col-mem",
        description="GPTel long-term memory CLI",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)
    
    # store command
    store_parser = subparsers.add_parser("store", help="Store a memory")
    store_parser.add_argument(
        "json_data",
        help='JSON: {"title": "...", "summary": "...", "project": "...", "type": "...", "tags": [...], "source": "..."}',
    )
    store_parser.set_defaults(func=cmd_store)
    
    # search command
    search_parser = subparsers.add_parser("search", help="Search memories")
    search_parser.add_argument("query", help="Search query")
    search_parser.add_argument("--top-k", type=int, default=5, help="Number of results")
    search_parser.add_argument("--project", help="Filter by project")
    search_parser.add_argument("--type", help="Filter by type")
    search_parser.set_defaults(func=cmd_search)
    
    # list command
    list_parser = subparsers.add_parser("list", help="List all memories")
    list_parser.add_argument("--project", help="Filter by project")
    list_parser.add_argument("--type", help="Filter by type")
    list_parser.set_defaults(func=cmd_list)
    
    # delete command
    delete_parser = subparsers.add_parser("delete", help="Delete a memory")
    delete_parser.add_argument("id", help="Memory ID to delete")
    delete_parser.set_defaults(func=cmd_delete)
    
    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
