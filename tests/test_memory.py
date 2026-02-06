"""Tests for memory store functionality."""

import pytest
from col_mem.memory import MemoryStore


class TestStore:
    """Tests for the store() function."""

    def test_store_basic(self, memory_store):
        """Store a memory with just title and summary."""
        memory_id = memory_store.store(
            title="Python async patterns",
            summary="User prefers asyncio over threads, uses await syntax.",
        )
        
        assert memory_id is not None
        assert len(memory_id) == 36  # UUID format
        assert memory_store.count() == 1

    def test_store_with_metadata(self, memory_store):
        """Store a memory with full metadata."""
        memory_id = memory_store.store(
            title="Emacs config preferences",
            summary="User uses use-package, prefers consult over helm.",
            project="dotfiles",
            type_="preference",
            tags=["emacs", "config"],
            source="buffer: init.el",
        )
        
        assert memory_id is not None
        assert memory_store.count() == 1
        
        # Verify stored data
        retrieved = memory_store.get(memory_id)
        assert retrieved is not None
        assert retrieved["title"] == "Emacs config preferences"
        assert retrieved["summary"] == "User uses use-package, prefers consult over helm."
        assert retrieved["project"] == "dotfiles"
        assert retrieved["type"] == "preference"
        assert retrieved["tags"] == ["emacs", "config"]
        assert retrieved["source"] == "buffer: init.el"
        assert retrieved["created_at"] is not None

    def test_store_multiple(self, memory_store):
        """Store multiple memories."""
        id1 = memory_store.store(title="First", summary="First memory content")
        id2 = memory_store.store(title="Second", summary="Second memory content")
        id3 = memory_store.store(title="Third", summary="Third memory content")
        
        assert memory_store.count() == 3
        assert len({id1, id2, id3}) == 3  # All unique IDs

    def test_store_default_type(self, memory_store):
        """Verify default type is applied from config."""
        memory_id = memory_store.store(
            title="Test",
            summary="Test content",
        )
        
        retrieved = memory_store.get(memory_id)
        assert retrieved["type"] == "memory"  # default from temp_config

    def test_store_empty_tags(self, memory_store):
        """Store with no tags returns empty list on retrieval."""
        memory_id = memory_store.store(
            title="No tags",
            summary="Memory without tags",
        )
        
        retrieved = memory_store.get(memory_id)
        assert retrieved["tags"] == []


class TestGet:
    """Tests for the get() function."""

    def test_get_nonexistent(self, memory_store):
        """Get returns None for nonexistent ID."""
        result = memory_store.get("nonexistent-uuid-12345")
        assert result is None

    def test_get_returns_correct_memory(self, memory_store):
        """Get returns the correct memory by ID."""
        id1 = memory_store.store(title="First", summary="First content")
        id2 = memory_store.store(title="Second", summary="Second content")
        
        result = memory_store.get(id1)
        assert result["title"] == "First"
        assert result["summary"] == "First content"
        
        result = memory_store.get(id2)
        assert result["title"] == "Second"
        assert result["summary"] == "Second content"
