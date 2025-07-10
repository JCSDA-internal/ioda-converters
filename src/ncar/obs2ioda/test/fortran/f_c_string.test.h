#ifndef OBS2IODA_F_C_STRING_TEST_H
#define OBS2IODA_F_C_STRING_TEST_H

extern "C" {

/// @brief Check if the given C string equals "abc".
/// @param c_string A null-terminated C string pointer.
/// @return 0 if equal to "abc", 1 otherwise.
int str_equals_abc(const char* c_string);

/// @brief Check if the given C string equals "abc def".
/// @param c_string A null-terminated C string pointer.
/// @return 0 if equal to "abc def", 1 otherwise.
int str_equals_abc_space_def(const char* c_string);

/// @brief Check if the first element of a C string array equals "abc".
/// @param c_string_array Array of null-terminated C strings.
/// @return 0 if first element is "abc", 1 otherwise.
int str_array_equals_abc(const char** c_string_array);

/// @brief Check if the first three elements of a C string array are
/// "abc", "def", and "ghi".
/// @param c_string_array Array of null-terminated C strings.
/// @return 0 if all three match, 1 otherwise.
int str_array_equals_abc_def_ghi(const char** c_string_array);

} // extern "C"

#endif // OBS2IODA_F_C_STRING_TEST_H
