#include "f_c_string.test.h"
#include <string>

/// @brief Check if the C string is equal to "abc".
/// @param c_string Null-terminated C string from Fortran.
/// @return 0 if match, 1 otherwise.
int str_equals_abc(const char* c_string) {
    std::string str(c_string);
    if (str == "abc") {
        return 0;
    }
    return 1;
}

/// @brief Check if the C string is equal to "abc def".
/// @param c_string Null-terminated C string from Fortran.
/// @return 0 if match, 1 otherwise.
int str_equals_abc_space_def(const char* c_string) {
    std::string str(c_string);
    if (str == "abc def") {
        return 0;
    }
    return 1;
}

/// @brief Check if the first string in a C string array is "abc".
/// @param c_string_array Array of null-terminated strings.
/// @return 0 if first element is "abc", 1 otherwise.
int str_array_equals_abc(const char** c_string_array) {
    std::string str(c_string_array[0]);
    if (str == "abc") {
        return 0;
    }
    return 1;
}

/// @brief Check if the first three strings in the array are "abc", "def", and "ghi".
/// @param c_string_array Array of null-terminated strings.
/// @return 0 if all three strings match the expected values, 1 otherwise.
int str_array_equals_abc_def_ghi(const char** c_string_array) {
    std::string str1(c_string_array[0]);
    std::string str2(c_string_array[1]);
    std::string str3(c_string_array[2]);
    if (str1 == "abc" && str2 == "def" && str3 == "ghi") {
        return 0;
    }
    return 1;
}
