"""ChromaDB memory storage and retrieval."""

# Suppress warnings BEFORE importing anything else
import os
import sys
import warnings

os.environ["TOKENIZERS_PARALLELISM"] = "false"
os.environ["HF_HUB_DISABLE_PROGRESS_BARS"] = "1"
os.environ["HF_HUB_DISABLE_SYMLINKS_WARNING"] = "1"
os.environ["HF_HUB_DISABLE_TELEMETRY"] = "1"
warnings.filterwarnings("ignore")

# Suppress all logging below ERROR level
import logging
logging.getLogger("huggingface_hub").setLevel(logging.ERROR)
logging.getLogger("sentence_transformers").setLevel(logging.ERROR)
logging.getLogger("chromadb").setLevel(logging.ERROR)

# Redirect stderr temporarily during imports to catch any stray warnings
import io
_stderr = sys.stderr
sys.stderr = io.StringIO()

# Now import the rest
import chromadb
from chromadb.config import Settings
from datetime import datetime, timezone
from pathlib import Path
from uuid import uuid4

# Suppress transformer logging
from transformers import logging as tf_logging
tf_logging.set_verbosity_error()

# Restore stderr
sys.stderr = _stderr

from .config import Config, load_config


class MemoryStore:
    """Vector memory store backed by ChromaDB."""

    def __init__(self, config: Config | None = None):
        """Initialize ChromaDB client and collection.
        
        Args:
            config: Configuration object. If None, loads from default paths.
        """
        self.config = config or load_config()
        
        # Ensure db directory exists
        self.config.db_path.mkdir(parents=True, exist_ok=True)
        
        # Suppress stderr during client initialization (model loading warnings)
        _stderr = sys.stderr
        sys.stderr = io.StringIO()
        
        try:
            # Initialize persistent ChromaDB client
            self._client = chromadb.PersistentClient(
                path=str(self.config.db_path),
                settings=Settings(
                    anonymized_telemetry=False,  # Disable telemetry
                ),
            )
            
            # Get or create collection with sentence-transformer embeddings
            self._collection = self._client.get_or_create_collection(
                name=self.config.collection_name,
                metadata={"hnsw:space": "cosine"},  # Use cosine similarity
                embedding_function=chromadb.utils.embedding_functions.SentenceTransformerEmbeddingFunction(
                    model_name=self.config.embedding_model,
                ),
            )
        finally:
            sys.stderr = _stderr

    @property
    def collection(self):
        """Access the underlying ChromaDB collection."""
        return self._collection

    def store(
        self,
        title: str,
        summary: str,
        project: str | None = None,
        type_: str | None = None,
        tags: list[str] | None = None,
        source: str | None = None,
    ) -> str:
        """Store a memory in the vector database.
        
        Args:
            title: Short descriptive title for the memory.
            summary: The content to embed and store.
            project: Optional project name for filtering.
            type_: Memory type (preference, fact, etc). Defaults to config.
            tags: Optional list of tags.
            source: Optional source reference (buffer name, file, etc).
            
        Returns:
            The generated memory ID (UUID).
        """
        memory_id = str(uuid4())
        
        # Build metadata
        metadata = {
            "title": title,
            "type": type_ or self.config.default_type,
            "created_at": datetime.now(timezone.utc).isoformat(),
        }
        
        if project:
            metadata["project"] = project
        if tags:
            metadata["tags"] = ",".join(tags)  # ChromaDB doesn't support list values
        if source:
            metadata["source"] = source
            
        self._collection.add(
            ids=[memory_id],
            documents=[summary],
            metadatas=[metadata],
        )
        
        return memory_id

    def search(
        self,
        query: str,
        top_k: int | None = None,
        where: dict | None = None,
    ) -> list[dict]:
        """Search memories by semantic similarity.
        
        Args:
            query: Search query text.
            top_k: Number of results to return. Defaults to config.
            where: Optional metadata filter dict (e.g., {"project": "col_mem"}).
            
        Returns:
            List of results with id, title, summary, score, and metadata.
        """
        top_k = top_k or self.config.top_k
        
        results = self._collection.query(
            query_texts=[query],
            n_results=top_k,
            where=where,
            include=["metadatas", "documents", "distances"],
        )
        
        # Format results
        memories = []
        if results["ids"] and results["ids"][0]:
            for i, memory_id in enumerate(results["ids"][0]):
                meta = results["metadatas"][0][i]
                distance = results["distances"][0][i]
                # Convert cosine distance to similarity score (0-1, higher is better)
                score = 1 - distance
                
                tags = meta.get("tags", "")
                memories.append({
                    "id": memory_id,
                    "title": meta.get("title", ""),
                    "summary": results["documents"][0][i],
                    "score": round(score, 4),
                    "project": meta.get("project"),
                    "type": meta.get("type"),
                    "tags": tags.split(",") if tags else [],
                    "source": meta.get("source"),
                    "created_at": meta.get("created_at"),
                })
        
        return memories

    def get(self, memory_id: str) -> dict | None:
        """Get a single memory by ID.
        
        Args:
            memory_id: The memory UUID.
            
        Returns:
            Memory dict or None if not found.
        """
        results = self._collection.get(
            ids=[memory_id],
            include=["metadatas", "documents"],
        )
        
        if not results["ids"]:
            return None
            
        meta = results["metadatas"][0]
        tags = meta.get("tags", "")
        
        return {
            "id": memory_id,
            "title": meta.get("title", ""),
            "summary": results["documents"][0],
            "project": meta.get("project"),
            "type": meta.get("type"),
            "tags": tags.split(",") if tags else [],
            "source": meta.get("source"),
            "created_at": meta.get("created_at"),
        }

    def count(self) -> int:
        """Return the number of memories in the store."""
        return self._collection.count()
