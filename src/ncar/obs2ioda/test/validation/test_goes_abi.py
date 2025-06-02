from test_utils.utils import extract_structure, format_netcdf_assert_msg

import pytest
import numpy as np

@pytest.mark.goes_abi
def test_variable_names_match(goes_abi_file_pair):
    """
    Verify that all variable names in each group match between reference and output files.

    Parameters
    ----------
    goes_abi_file_pair : tuple
        Tuple containing (reference_file_path, output_file_path).
    """
    reference_file, output_file = goes_abi_file_pair
    ref_struct = extract_structure(reference_file)
    test_struct = extract_structure(output_file)

    for group in ref_struct:
        ref_vars = set(ref_struct[group].keys())
        test_vars = set(test_struct[group].keys())
        assert ref_vars == test_vars, format_netcdf_assert_msg(
            output_file.name, group, "<ALL>", "Variable name mismatch", ref_vars, test_vars
        )

@pytest.mark.goes_abi
def test_dtype_match(goes_abi_file_pair):
    """
    Validate that all variables have matching data types between reference and output files.

    Parameters
    ----------
    goes_abi_file_pair : tuple
        Tuple containing (reference_file_path, output_file_path).
    """
    reference_file, output_file = goes_abi_file_pair
    ref_struct = extract_structure(reference_file)
    test_struct = extract_structure(output_file)

    for group in ref_struct:
        for varname in ref_struct[group]:
            ref = ref_struct[group][varname]
            test = test_struct[group][varname]
            assert ref["dtype"] == test["dtype"], format_netcdf_assert_msg(
                output_file.name, group, varname, "Dtype mismatch", ref["dtype"], test["dtype"]
            )

@pytest.mark.goes_abi
def test_dimensions_match(goes_abi_file_pair):
    """
    Validate that the dimensions of all variables match between reference and output files.

    Parameters
    ----------
    goes_abi_file_pair : tuple
        Tuple containing (reference_file_path, output_file_path).
    """
    reference_file, output_file = goes_abi_file_pair
    ref_struct = extract_structure(reference_file)
    test_struct = extract_structure(output_file)

    for group in ref_struct:
        for varname in ref_struct[group]:
            ref = ref_struct[group][varname]
            test = test_struct[group][varname]
            assert ref["dimensions"] == test["dimensions"], format_netcdf_assert_msg(
                output_file.name, group, varname, "Dimension mismatch", ref["dimensions"], test["dimensions"]
            )

@pytest.mark.goes_abi
def test_attributes_match(goes_abi_file_pair):
    """
    Validate that all variable attribute names and values match between reference and output files.

    Parameters
    ----------
    goes_abi_file_pair : tuple
        Tuple containing (reference_file_path, output_file_path).
    """
    reference_file, output_file = goes_abi_file_pair
    ref_struct = extract_structure(reference_file)
    test_struct = extract_structure(output_file)

    for group in ref_struct:
        for varname in ref_struct[group]:
            ref = ref_struct[group][varname]
            test = test_struct[group][varname]
            assert ref["attributes"] == test["attributes"], format_netcdf_assert_msg(
                output_file.name, group, varname, "Attribute mismatch", ref["attributes"], test["attributes"]
            )

@pytest.mark.goes_abi
def test_data_match(goes_abi_file_pair):
    """
    Validate that variable data arrays match between reference and output files.

    Parameters
    ----------
    goes_abi_file_pair : tuple
        Tuple containing (reference_file_path, output_file_path).
    """
    reference_file, output_file = goes_abi_file_pair
    ref_struct = extract_structure(reference_file)
    test_struct = extract_structure(output_file)

    for group in ref_struct:
        for varname in ref_struct[group]:
            ref = ref_struct[group][varname]
            test = test_struct[group][varname]

            if np.issubdtype(ref["dtype"], np.number):
                assert np.allclose(ref["data"], test["data"], rtol=1e-5, atol=1e-6), format_netcdf_assert_msg(
                    output_file.name, group, varname, "Numerical data mismatch", ref["data"], test["data"]
                )
            elif np.issubdtype(ref["dtype"], np.str_):
                for i, (rval, tval) in enumerate(zip(ref["data"].flatten(), test["data"].flatten())):
                    assert rval == tval, format_netcdf_assert_msg(
                        output_file.name, group, varname,
                        f"String mismatch at index {i}", rval, tval
                    )
            else:
                assert np.array_equal(ref["data"], test["data"]), format_netcdf_assert_msg(
                    output_file.name, group, varname, "Data mismatch", ref["data"], test["data"]
                )
