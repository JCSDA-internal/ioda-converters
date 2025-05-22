import subprocess
import tarfile
import requests
import numpy as np
import pytest
from pathlib import Path
from netCDF4 import Dataset, Variable, Group
from typing import List, Tuple, Dict, Any
from FilePathConfig import OBS2IODA_V3_EXECUTABLE_PATH

def obs2ioda_test_directory() -> Path:
    """
    Get the path to the test directory.

    :return: Path object pointing to the test directory.
    """
    return Path.cwd() / "test/validation"

# === Formatting Utility ===

def format_netcdf_assert_msg(file: str, group: str, variable: str, detail: str,
                             ref_val: Any, test_val: Any) -> str:
    """
    Format a detailed assertion error message for NetCDF comparisons.

    :param file: Name of the NetCDF file being tested.
    :param group: Path of the NetCDF group.
    :param variable: Name of the variable being compared.
    :param detail: Description of the mismatch.
    :param ref_val: Expected (reference) value.
    :param test_val: Actual (test) value.
    :return: Formatted string suitable for assertion output.
    """
    return (
        f"[NetCDF Mismatch] File: '{file}' | Group: '{group}' | Variable: '{variable}'\n"
        f"Reason: {detail}\n"
        f"Expected: {ref_val}\n"
        f"Actual:      {test_val}"
    )


# === Dataset Introspection ===

def describe_variable(var: Variable) -> Dict[str, Any]:
    """
    Extract metadata and data from a NetCDF variable.

    :param var: A netCDF4.Variable object.
    :return: Dictionary with dtype, dimensions, attributes, and data.
    """
    return {
        "dtype": var.dtype,
        "dimensions": var.dimensions,
        "attributes": set(var.ncattrs()),
        "data": var[...]
    }


def extract_structure(nc_path: Path) -> Dict[str, Dict[str, Dict[str, Any]]]:
    """
    Recursively extract structure of a NetCDF file, including variables and metadata.

    :param nc_path: Path to the NetCDF file.
    :return: Nested dictionary mapping group paths to variable metadata dictionaries.
    """
    def recurse(group: Group) -> Dict[str, Dict[str, Dict[str, Any]]]:
        structure: Dict[str, Dict[str, Dict[str, Any]]] = {group.path: {}}
        for name, var in group.variables.items():
            structure[group.path][name] = describe_variable(var)
        for subgroup in group.groups.values():
            structure.update(recurse(subgroup))
        return structure

    with Dataset(nc_path.as_posix(), "r") as ds:
        return recurse(ds)


# === File Pair Collection ===

def collect_file_pairs() -> List[Tuple[Path, Path]]:
    """
    Collect pairs of reference and test NetCDF files based on matching filenames.

    :raises FileNotFoundError: If reference or test directories are missing.
    :raises ValueError: If no test files are found.
    :return: List of (reference, test) Path tuples.
    """
    ref_dir = obs2ioda_test_directory() / "data/v3/reference"
    test_dir = obs2ioda_test_directory() / "data/v3/output"
    if not ref_dir.exists() or not test_dir.exists():
        raise FileNotFoundError("Reference or test directory does not exist.")

    file_pairs: List[Tuple[Path, Path]] = []
    for test_file in sorted(test_dir.glob("*.h5")):
        ref_file = ref_dir / test_file.name
        if not ref_file.exists():
            raise FileNotFoundError(f"Missing reference file for: {test_file.name}")
        file_pairs.append((ref_file, test_file))

    if not file_pairs:
        raise ValueError("No .h5 files found in test directory.")
    return file_pairs


# === Dynamic Parametrization ===

def pytest_generate_tests(metafunc: Any) -> None:
    """
    Pytest hook to dynamically generate test cases from reference/test file pairs.

    Downloads test data if not already present, runs obs2ioda_v3 conversion, and
    parameterizes tests with resulting NetCDF file pairs.

    :param metafunc: Pytest hook metafunction.
    """
    url = "https://www2.mmm.ucar.edu/obs2ioda/test_data/data.tar.gz"
    dest_dir = obs2ioda_test_directory()
    archive_path = dest_dir / Path(url).name

    if not archive_path.exists():
        dest_dir.mkdir(parents=True, exist_ok=True)
        with requests.get(url, stream=True) as r:
            r.raise_for_status()
            with open(archive_path, "wb") as f:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)

    extracted_dir = archive_path.with_suffix("").with_suffix("")
    if not extracted_dir.exists():
        with tarfile.open(archive_path, "r:gz") as tar:
            tar.extractall(path=dest_dir)

    input_dir = extracted_dir / "bufr"
    output_dir = extracted_dir / "v3/output"
    executable = Path(OBS2IODA_V3_EXECUTABLE_PATH)

    if not executable.exists():
        raise FileNotFoundError(f"obs2ioda_v3 executable not found at {executable}")

    if not output_dir.exists():
        output_dir.mkdir(exist_ok=True)
        for file in output_dir.glob("*"):
            file.unlink()
        for bufr_file in input_dir.glob("*gdas*"):
            cmd = [str(executable), "-i", str(input_dir), "-o", str(output_dir), bufr_file.name]
            try:
                subprocess.run(cmd, check=True, capture_output=True)
            except subprocess.CalledProcessError as e:
                raise RuntimeError(f"obs2ioda_v3 failed for {bufr_file.name}: {e.stderr}")

    if "file_pair" in metafunc.fixturenames:
        file_pairs = collect_file_pairs()
        metafunc.parametrize("file_pair", file_pairs, ids=[pair[1].name for pair in file_pairs])


@pytest.fixture(scope="session", autouse=True)
def teardown() -> None:
    """
    Session-level fixture that cleans up output files after all tests complete.

    Deletes all files in the `data/v3/output` directory.
    """
    yield
    output_dir = obs2ioda_test_directory() / "data/v3/output"
    if output_dir.exists():
        for file in output_dir.glob("*"):
            file.unlink()
        if output_dir.exists():
            output_dir.rmdir()


@pytest.fixture(scope="module")
def file_pair(request: Any) -> Tuple[Path, Path]:
    """
    Fixture providing a reference/test file path tuple to each test case.

    :param request: Pytest request object containing the file pair param.
    :return: A tuple of Paths (reference_file, test_file).
    """
    return request.param


# === Test Definitions ===

def test_group_names_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that group names in the test NetCDF file match those in the reference file.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    ref_groups = set(extract_structure(ref_path).keys())
    test_groups = set(extract_structure(test_path).keys())
    assert ref_groups == test_groups, (
        f"[NetCDF Mismatch] File: '{test_path.name}'\n"
        f"Group name mismatch\nReference: {ref_groups}\nTest:      {test_groups}"
    )


def test_variable_names_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that variable names in each group match between reference and test NetCDF files.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    ref_struct = extract_structure(ref_path)
    test_struct = extract_structure(test_path)
    for group in ref_struct:
        ref_vars = set(ref_struct[group].keys())
        test_vars = set(test_struct[group].keys())
        assert ref_vars == test_vars, (
            f"[NetCDF Mismatch] File: '{test_path.name}' | Group: '{group}'\n"
            f"Variable name mismatch\nReference: {ref_vars}\nTest:      {test_vars}"
        )


def test_variable_dtype_and_dimensions_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that variable data types and dimensions match between reference and test files.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    ref_struct = extract_structure(ref_path)
    test_struct = extract_structure(test_path)
    for group in ref_struct:
        for varname in ref_struct[group]:
            ref = ref_struct[group][varname]
            test = test_struct[group][varname]
            assert ref["dtype"] == test["dtype"], format_netcdf_assert_msg(
                test_path.name, group, varname, "Dtype mismatch", ref["dtype"], test["dtype"]
            )
            assert ref["dimensions"] == test["dimensions"], format_netcdf_assert_msg(
                test_path.name, group, varname, "Dimension mismatch", ref["dimensions"], test["dimensions"]
            )


def test_variable_attributes_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that variable attribute name sets match between reference and test NetCDF files.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    ref_struct = extract_structure(ref_path)
    test_struct = extract_structure(test_path)
    for group in ref_struct:
        for varname in ref_struct[group]:
            ref_attrs = ref_struct[group][varname]["attributes"]
            test_attrs = test_struct[group][varname]["attributes"]
            assert ref_attrs == test_attrs, format_netcdf_assert_msg(
                test_path.name, group, varname, "Attribute name set mismatch", ref_attrs, test_attrs
            )


def test_variable_attribute_values_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that all shared variable attribute values match exactly between files.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    with Dataset(ref_path.as_posix(), "r") as ref_ds, Dataset(test_path.as_posix(), "r") as test_ds:
        def recurse(ref_grp: Group, test_grp: Group) -> None:
            for group_name in ref_grp.groups:
                recurse(ref_grp.groups[group_name], test_grp.groups[group_name])
            for varname in ref_grp.variables:
                ref_var = ref_grp.variables[varname]
                test_var = test_grp.variables[varname]
                for attr in set(ref_var.ncattrs()) & set(test_var.ncattrs()):
                    ref_val = ref_var.getncattr(attr)
                    test_val = test_var.getncattr(attr)
                    if isinstance(ref_val, np.ndarray) or isinstance(test_val, np.ndarray):
                        assert np.array_equal(ref_val, test_val), format_netcdf_assert_msg(
                            test_path.name, ref_grp.path, varname,
                            f"Attribute array value mismatch for '{attr}'", ref_val, test_val
                        )
                    else:
                        assert ref_val == test_val, format_netcdf_assert_msg(
                            test_path.name, ref_grp.path, varname,
                            f"Attribute value mismatch for '{attr}'", ref_val, test_val
                        )
        recurse(ref_ds, test_ds)


def test_variable_data_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that variable data arrays match between reference and test NetCDF files.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    ref_struct = extract_structure(ref_path)
    test_struct = extract_structure(test_path)
    for group in ref_struct:
        if group == "/":
            continue
        for varname in ref_struct[group]:
            ref = ref_struct[group][varname]
            test = test_struct[group][varname]
            dtype = ref["dtype"]
            ref_data = ref["data"]
            test_data = test["data"]
            if ref_data.size == 0 and test_data.size == 0:
                continue
            if np.issubdtype(dtype, np.str_):
                for i, (r, o) in enumerate(zip(ref_data.flatten(), test_data.flatten())):
                    if r == "-" or o == "-":
                        continue
                    assert r == o, format_netcdf_assert_msg(
                        test_path.name, group, varname,
                        f"String mismatch at index {i}", r, o
                    )
            elif np.issubdtype(dtype, np.number):
                assert np.ma.allclose(ref_data.flatten(), test_data.flatten(),
                                      masked_equal=True, atol=1e-6, rtol=1e-5), (
                    format_netcdf_assert_msg(
                        test_path.name, group, varname,
                        "Numeric data mismatch", ref_data, test_data
                    )
                )
            else:
                assert np.array_equal(ref_data, test_data), format_netcdf_assert_msg(
                    test_path.name, group, varname,
                    "Non-numeric array mismatch", ref_data, test_data
                )


def test_global_attributes_match(file_pair: Tuple[Path, Path]) -> None:
    """
    Test that global attributes and their values match between reference and test NetCDF files.

    :param file_pair: Tuple of (reference, test) NetCDF file paths.
    """
    ref_path, test_path = file_pair
    with Dataset(ref_path.as_posix(), "r") as ref_ds, Dataset(test_path.as_posix(), "r") as test_ds:
        ref_attrs = set(ref_ds.ncattrs())
        test_attrs = set(test_ds.ncattrs())
        assert ref_attrs == test_attrs, format_netcdf_assert_msg(
            test_path.name, "/", "global", "Global attribute name mismatch", ref_attrs, test_attrs
        )
        for attr in ref_attrs:
            ref_val = ref_ds.getncattr(attr)
            test_val = test_ds.getncattr(attr)
            if isinstance(ref_val, np.ndarray) or isinstance(test_val, np.ndarray):
                assert np.array_equal(ref_val, test_val), format_netcdf_assert_msg(
                    test_path.name, "/", "global",
                    f"Global attribute array value mismatch for '{attr}'", ref_val, test_val
                )
            else:
                assert ref_val == test_val, format_netcdf_assert_msg(
                    test_path.name, "/", "global",
                    f"Global attribute value mismatch for '{attr}'", ref_val, test_val
                )
