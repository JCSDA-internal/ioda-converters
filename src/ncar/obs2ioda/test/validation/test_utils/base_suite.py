from pathlib import Path
from typing import Any
from typing_extensions import Callable
from .utils import clean_directory, parametrize_file_pairs

class TestSuite:
    """
    A reusable test suite class for running IODA format validation tests.

    This class is designed to be used with pytest to manage:
    - File setup and teardown logic
    - Parametrization of test file pairs
    - Execution of preprocessing programs or converters

    Attributes
    ----------
    marker : str
        Pytest marker used to identify tests belonging to this suite.
    executable : Path
        Path to the executable that generates the output files for validation.
    output_dir : Path
        Directory where the output NetCDF files will be written.
    ref_dir : Path
        Directory containing the reference NetCDF files for comparison.
    ext : str
        File extension used to filter relevant files (e.g., ".nc", ".h5").
    fixture_name : str
        Name of the pytest fixture to which file pairs will be bound.
    setup_fn : Callable[[Path], None], optional
        Optional setup function that prepares the output directory before tests run.
    """

    def __init__(
            self,
            marker: str,
            executable: Path,
            output_dir: Path,
            ref_dir: Path,
            ext: str,
            fixture_name: str,
            setup_fn: Callable[[Path], None] = None
    ):
        """
        Initialize a new test suite configuration.

        Parameters
        ----------
        marker : str
            Pytest marker for selecting tests in this suite.
        executable : Path
            Path to the program that produces test output files.
        output_dir : Path
            Location where test outputs will be stored.
        ref_dir : Path
            Location of the reference output files.
        ext : str
            File extension used to select files for comparison.
        fixture_name : str
            Name of the pytest fixture for test parametrization.
        setup_fn : Callable[[Path], None], optional
            Function to prepare the test output directory, if needed.
        """
        self.marker = marker
        self.executable = executable
        self.output_dir = output_dir
        self.ref_dir = ref_dir
        self.ext = ext
        self.fixture_name = fixture_name
        self.setup_fn = setup_fn

    def setup(self, metafunc: Any):
        """
        Set up the test case by calling the optional setup function
        and configuring the test parameterization.

        Parameters
        ----------
        metafunc : Any
            Pytest metafunc object used during test collection.
        """
        if self.setup_fn:
            self.setup_fn(self.output_dir)
        parametrize_file_pairs(
            metafunc,
            self.fixture_name,
            self.ref_dir,
            self.output_dir,
            self.ext
        )

    def teardown(self):
        """
        Clean up the output directory after the test is complete.
        This removes all files that were generated during the test.
        """
        clean_directory(self.output_dir)
