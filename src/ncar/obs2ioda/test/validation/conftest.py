"""
Pytest configuration and test suite dispatcher for validation tests.
"""

from typing import Tuple, Generator, Any
from pathlib import Path
import pytest

from test_utils.ncep_prepbufr_bufr_suite import NCEP_PREBUFR_BUFR_SUITE
from test_utils.goes_abi_suite import GOES_ABI_SUITE
from test_utils.utils import get_pytest_markers


#: Dictionary mapping pytest markers to configured test suites.
TEST_SUITES = {
    "ncep_prepbufr_bufr": NCEP_PREBUFR_BUFR_SUITE,
    "goes_abi": GOES_ABI_SUITE,
}


def pytest_configure(config):
    """
    Register custom pytest markers from the pytest.ini configuration.

    Parameters
    ----------
    config : Any
        Pytest configuration object.
    """
    config.markers = [marker.split(":")[0] for marker in config.inicfg["markers"].splitlines()]

def pytest_generate_tests(metafunc: Any) -> None:
    """
    Dynamically parameterize tests using the appropriate test suite based on markers.

    Parameters
    ----------
    metafunc : Any
        The pytest metafunc object, passed during test collection.
    """
    for name, suite in TEST_SUITES.items():
        if name in get_pytest_markers(metafunc):
            suite.setup(metafunc)

@pytest.fixture(scope="session", autouse=True)
def teardown(request) -> Generator[None, Any, None]:
    """
    Global fixture that runs once per session and handles teardown for all test suites.

    Parameters
    ----------
    request : Any
        Pytest request object.

    Yields
    ------
    None
        This fixture is used only for its side effects (teardown after tests).
    """
    yield
    for marker in request.config.markers:
        if marker in TEST_SUITES:
            TEST_SUITES[marker].teardown()

@pytest.fixture(scope="module")
def ncep_prepbufr_bufr_file_pair(request: Any) -> Tuple[Path, Path]:
    """
    Module-scoped fixture providing a pair of reference and output files for NCEP PREPBUFR tests.

    Parameters
    ----------
    request : Any
        The pytest request object containing parameterized file pair.

    Returns
    -------
    Tuple[Path, Path]
        Tuple of reference and test NetCDF file paths.
    """
    return request.param

@pytest.fixture(scope="module")
def goes_abi_file_pair(request: Any) -> Tuple[Path, Path]:
    """
    Module-scoped fixture providing a pair of reference and output files for GOES-ABI tests.

    Parameters
    ----------
    request : Any
        The pytest request object containing parameterized file pair.

    Returns
    -------
    Tuple[Path, Path]
        Tuple of reference and test NetCDF file paths.
    """
    return request.param
