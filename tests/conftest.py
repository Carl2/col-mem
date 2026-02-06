"""Pytest fixtures for col_mem tests."""

import pytest
import tempfile
from pathlib import Path

from col_mem.config import Config
from col_mem.memory import MemoryStore


@pytest.fixture
def temp_config(tmp_path):
    """Create a temporary config for testing."""
    return Config(
        db_path=tmp_path / "test_chroma_db",
        collection_name="test_memories",
        embedding_model="all-MiniLM-L6-v2",
        top_k=5,
        default_type="memory",
    )


@pytest.fixture
def memory_store(temp_config):
    """Create a MemoryStore with temp database."""
    store = MemoryStore(config=temp_config)
    yield store
    # Cleanup: delete the collection after test
    store._client.delete_collection(temp_config.collection_name)
