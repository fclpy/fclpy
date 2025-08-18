import pytest
import logging

def pytest_configure(config):
    logging.basicConfig(level=logging.INFO)

class Results:
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.errors = []

    def test(self, name, condition, error_msg=None):
        if condition:
            self.passed += 1
        else:
            self.failed += 1
            if error_msg:
                self.errors.append(f"{name}: {error_msg}")

    def summary(self):
        return self.failed == 0


@pytest.fixture
def results():
    """Provide a lightweight Results object for existing test functions expecting it."""
    return Results()
