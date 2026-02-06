"""Tests for memory search functionality."""

import pytest
from col_mem.memory import MemoryStore
from col_mem.config import Config
from pathlib import Path
import tempfile
import shutil


@pytest.fixture
def temp_store():
    """Create a MemoryStore with temp directory."""
    temp_dir = tempfile.mkdtemp()
    config = Config(
        db_path=Path(temp_dir) / "test_chroma",
        embedding_model="all-MiniLM-L6-v2",
        collection_name="test_memories",
        top_k=5,
        default_type="general",
    )
    store = MemoryStore(config)
    yield store
    # Cleanup
    shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture
def populated_store(temp_store):
    """Store with pre-populated test data."""
    # Store diverse memories for search testing
    temp_store.store(
        title="Python async patterns",
        summary="User prefers asyncio over threads. Uses async/await syntax extensively. "
                "Favors aiohttp for HTTP requests and asyncpg for database connections.",
        project="backend",
        type_="preference",
        tags=["python", "async"],
    )
    temp_store.store(
        title="Emacs configuration",
        summary="User uses Doom Emacs with evil-mode. Prefers consult for completion. "
                "Has custom gptel setup for AI conversations.",
        project="dotfiles",
        type_="preference",
        tags=["emacs", "config"],
    )
    temp_store.store(
        title="Green car preferences",
        summary="User wants to buy a green electric vehicle. Considering Tesla Model 3 "
                "or Hyundai Ioniq. Prefers longer range over faster charging.",
        type_="fact",
        tags=["car", "personal"],
    )
    temp_store.store(
        title="Database schema design",
        summary="User follows strict normalization for relational databases. "
                "Uses UUID primary keys. Prefers PostgreSQL over MySQL.",
        project="backend",
        type_="preference",
        tags=["database", "sql"],
    )
    temp_store.store(
        title="Meeting notes January",
        summary="Discussed project timeline with team. Backend API due in two weeks. "
                "Need to finalize database schema before frontend work begins.",
        project="work",
        type_="note",
        tags=["meeting", "work"],
    )
    return temp_store


class TestBasicSearch:
    """Test basic semantic search functionality."""

    def test_search_returns_list(self, populated_store):
        """Search should return a list."""
        results = populated_store.search("python programming")
        assert isinstance(results, list)

    def test_search_finds_relevant_memory(self, populated_store):
        """Search should find semantically relevant memories."""
        results = populated_store.search("async await python")
        assert len(results) > 0
        # The Python async memory should be in top results
        titles = [r["title"] for r in results]
        assert "Python async patterns" in titles

    def test_search_returns_score(self, populated_store):
        """Each result should have a similarity score."""
        results = populated_store.search("emacs configuration")
        assert len(results) > 0
        for result in results:
            assert "score" in result
            assert 0.0 <= result["score"] <= 1.0

    def test_search_results_sorted_by_score(self, populated_store):
        """Results should be sorted by descending score."""
        results = populated_store.search("database design")
        if len(results) > 1:
            scores = [r["score"] for r in results]
            assert scores == sorted(scores, reverse=True)

    def test_search_returns_full_metadata(self, populated_store):
        """Results should include all metadata fields."""
        results = populated_store.search("electric car")
        assert len(results) > 0
        result = results[0]
        assert "id" in result
        assert "title" in result
        assert "summary" in result
        assert "score" in result
        assert "created_at" in result
        assert "type" in result
        assert "tags" in result

    def test_search_empty_db(self, temp_store):
        """Search on empty DB should return empty list."""
        results = temp_store.search("anything")
        assert results == []


class TestSearchTopK:
    """Test top_k limiting."""

    def test_search_respects_top_k(self, populated_store):
        """Search should return at most top_k results."""
        results = populated_store.search("preferences", top_k=2)
        assert len(results) <= 2

    def test_search_default_top_k_from_config(self, populated_store):
        """Search should use config top_k by default."""
        results = populated_store.search("project")
        # Config has top_k=5, we have 5 memories
        assert len(results) <= populated_store.config.top_k

    def test_search_top_k_larger_than_results(self, populated_store):
        """top_k larger than DB size should return all results."""
        results = populated_store.search("project", top_k=100)
        assert len(results) == 5  # We have 5 memories


class TestSearchWithFilters:
    """Test metadata filtering in search."""

    def test_search_filter_by_project(self, populated_store):
        """Filter by project should only return matching memories."""
        results = populated_store.search(
            "preferences",
            where={"project": "backend"}
        )
        for result in results:
            assert result["project"] == "backend"

    def test_search_filter_by_type(self, populated_store):
        """Filter by type should only return matching memories."""
        results = populated_store.search(
            "user wants",
            where={"type": "fact"}
        )
        for result in results:
            assert result["type"] == "fact"

    def test_search_filter_no_matches(self, populated_store):
        """Filter with no matches should return empty list."""
        results = populated_store.search(
            "python",
            where={"project": "nonexistent"}
        )
        assert results == []

    def test_search_filter_combined_with_semantic(self, populated_store):
        """Filter should combine with semantic search."""
        # Search for database-related with project filter
        results = populated_store.search(
            "database schema",
            where={"project": "backend"}
        )
        # Should find the database schema design memory
        assert len(results) > 0
        titles = [r["title"] for r in results]
        assert "Database schema design" in titles


class TestSearchSemanticQuality:
    """Test semantic search quality."""

    def test_semantic_not_keyword(self, populated_store):
        """Search should work semantically, not just keyword matching."""
        # "EV" and "automobile" don't appear in the car memory,
        # but should still match "electric vehicle"
        results = populated_store.search("EV automobile purchase")
        titles = [r["title"] for r in results[:2]]  # Check top 2
        assert "Green car preferences" in titles

    def test_different_queries_different_rankings(self, populated_store):
        """Different queries should produce different rankings."""
        results1 = populated_store.search("python programming async")
        results2 = populated_store.search("emacs editor configuration")
        
        # Top results should be different
        assert results1[0]["title"] != results2[0]["title"]

    def test_relevant_score_higher(self, populated_store):
        """More relevant memories should have higher scores."""
        results = populated_store.search("asyncio python concurrent programming")
        
        # Find specific memories in results
        python_result = next((r for r in results if r["title"] == "Python async patterns"), None)
        car_result = next((r for r in results if r["title"] == "Green car preferences"), None)
        
        # Python async should score higher for this query than car preferences
        if python_result and car_result:
            assert python_result["score"] > car_result["score"]
