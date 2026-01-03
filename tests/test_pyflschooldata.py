"""
Tests for pyflschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pyflschooldata
    assert pyflschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pyflschooldata
    assert hasattr(pyflschooldata, 'fetch_enr')
    assert callable(pyflschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pyflschooldata
    assert hasattr(pyflschooldata, 'get_available_years')
    assert callable(pyflschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pyflschooldata
    assert hasattr(pyflschooldata, '__version__')
    assert isinstance(pyflschooldata.__version__, str)
