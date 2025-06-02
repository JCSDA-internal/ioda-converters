from pathlib import Path
from .base_suite import TestSuite
from .FilePathConfig import TEST_WRITE_GOES_ABI_IODA_V3_EXECUTABLE_PATH, VALIDATION_TEST_DIRECTORY
from .utils import run_executable

def setup_goes_abi(output_dir: Path):
    """
    Setup function for GOES-ABI test suite. Runs the test executable and creates output.

    Parameters
    ----------
    output_dir : Path
        Directory to write test output to.
    """
    output_file = output_dir / "write_goes_abi_ioda_v3.nc"
    output_dir.mkdir(parents=True, exist_ok=True)
    run_executable(TEST_WRITE_GOES_ABI_IODA_V3_EXECUTABLE_PATH, str(output_dir), str(output_file))

GOES_ABI_SUITE = TestSuite(
    marker="goes_abi",
    executable=Path(TEST_WRITE_GOES_ABI_IODA_V3_EXECUTABLE_PATH),
    output_dir=Path(VALIDATION_TEST_DIRECTORY) / "data/goes_abi/write_ioda_v3/output",
    ref_dir=Path(VALIDATION_TEST_DIRECTORY) / "data/goes_abi/write_ioda_v3/reference",
    ext=".nc",
    fixture_name="goes_abi_file_pair",
    setup_fn=setup_goes_abi,
)
