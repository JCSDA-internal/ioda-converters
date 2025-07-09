from pathlib import Path
from typing import Tuple, List, Any, Dict
import subprocess

from netCDF4 import Group, Dataset, Variable


def run_executable(executable: Path, *args: str) -> None:
    """
    Run an external executable with arguments and raise an error if it fails.

    Parameters
    ----------
    executable : Path
        Path to the executable.
    *args : str
        Additional command-line arguments to pass.

    Raises
    ------
    RuntimeError
        If the executable returns a non-zero exit code.
    """
    result = subprocess.run([str(executable), *args], capture_output=True, text=True)
    if result.returncode != 0:
        raise RuntimeError(f"{executable.name} failed:\n{result.stderr}")


def clean_directory(dir_path: Path):
    """
    Remove all files in a directory and delete the directory itself.

    Parameters
    ----------
    dir_path : Path
        Path to the directory to be cleaned.
    """
    if dir_path.exists():
        for file in dir_path.glob("*"):
            file.unlink()
        dir_path.rmdir()


def collect_file_pairs(ref_dir: Path, test_dir: Path, ext: str) -> List[Tuple[Path, Path]]:
    """
    Find and return matching (reference, test) file pairs with the given extension.

    Parameters
    ----------
    ref_dir : Path
        Directory containing reference files.
    test_dir : Path
        Directory containing test output files.
    ext : str
        File extension to match (e.g., '.nc').

    Returns
    -------
    List[Tuple[Path, Path]]
        List of matching file path pairs.

    Raises
    ------
    ValueError
        If no matching file pairs are found.
    """
    pairs = [
        (ref, test)
        for test in sorted(test_dir.glob(f"*{ext}"))
        if (ref := ref_dir / test.name).exists()
    ]
    if not pairs:
        raise ValueError(f"No matching {ext} file pairs found.")
    return pairs


def parametrize_file_pairs(metafunc: Any, fixture_name: str, ref_dir: Path, out_dir: Path, ext: str):
    """
    Parametrize a pytest test function using file pairs as test cases.

    Parameters
    ----------
    metafunc : Any
        The pytest metafunc object.
    fixture_name : str
        Name of the pytest fixture.
    ref_dir : Path
        Directory containing reference files.
    out_dir : Path
        Directory containing generated output files.
    ext : str
        File extension to filter on.
    """
    if fixture_name in metafunc.fixturenames:
        file_pairs = collect_file_pairs(ref_dir, out_dir, ext)
        metafunc.parametrize(fixture_name, file_pairs, ids=[p[1].name for p in file_pairs])


def extract_structure(file_path: Path) -> Dict[str, Dict[str, Dict[str, Any]]]:
    """
    Recursively extract the structure of a NetCDF file into nested dictionaries.

    Parameters
    ----------
    file_path : Path
        Path to the NetCDF file.

    Returns
    -------
    Dict[str, Dict[str, Dict[str, Any]]]
        A nested dictionary with group paths as keys, and variable metadata as values.
    """
    def recurse(group: Group) -> Dict[str, Dict[str, Dict[str, Any]]]:
        structure = {group.path: {name: describe_variable(var) for name, var in group.variables.items()}}
        for sub in group.groups.values():
            structure.update(recurse(sub))
        return structure

    with Dataset(file_path.as_posix(), "r") as ds:
        return recurse(ds)


def describe_variable(var: Variable) -> Dict[str, Any]:
    """
    Extract metadata and data from a NetCDF variable.

    Parameters
    ----------
    var : Variable
        A NetCDF variable object.

    Returns
    -------
    Dict[str, Any]
        Dictionary containing dtype, dimensions, attributes, and the variable data.
    """
    return {
        "dtype": var.dtype,
        "dimensions": var.dimensions,
        "attributes": set(var.ncattrs()),
        "data": var[...],
    }


def format_netcdf_assert_msg(file: str, group: str, var: str,
                             detail: str, ref_val: Any, test_val: Any) -> str:
    """
    Format a message describing a mismatch between NetCDF reference and test data.

    Parameters
    ----------
    file : str
        Name of the NetCDF file.
    group : str
        Name of the group containing the variable.
    var : str
        Name of the variable with the mismatch.
    detail : str
        Description of the type of mismatch.
    ref_val : Any
        Reference value.
    test_val : Any
        Test value.

    Returns
    -------
    str
        A formatted assertion error message.
    """
    return (
        f"[NetCDF Mismatch] File: '{file}' | Group: '{group}' | Variable: '{var}'\n"
        f"Reason: {detail}\nExpected: {ref_val}\nActual:   {test_val}"
    )


def get_pytest_markers(metafunc: Any) -> List[str]:
    """
    Extract the list of markers used to select tests via the `-m` command-line option.

    If `-m` is provided, parse and return markers split by 'or'. If not provided,
    return all configured markers.

    Parameters
    ----------
    metafunc : Any
        The pytest `metafunc` object used during dynamic test generation.

    Returns
    -------
    List[str]
        A list of marker names used for test selection.
    """
    args = metafunc.config.invocation_params.args
    if "-m" in args:
        return [m.strip() for m in args[args.index("-m") + 1].split("or") if m.strip()]
    return metafunc.config.markers
