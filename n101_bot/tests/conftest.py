"""Shared pytest config for n101_bot tests.

Registers the `live` marker used by smoke tests that require a running
Ollama daemon. Does not change collection or any default behavior.
"""


def pytest_configure(config):
    config.addinivalue_line(
        "markers",
        "live: end-to-end smoke test that hits the live Ollama daemon",
    )
