/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <pybind11/pybind11.h>
#include <vector>
#include <string>

#include "QuerySet.h"
#include "File.h"
#include "ResultSet.h"


namespace py = pybind11;

using Ingester::bufr::ResultSet;
using Ingester::bufr::QuerySet;
using Ingester::bufr::File;

    PYBIND11_MODULE(bufr, m)
    {
        m.doc() = "Provides the ability to get data from BUFR files via query strings.";

        py::class_<QuerySet>(m, "QuerySet")
            .def(py::init<>())
            .def(py::init<const std::vector<std::string>&>())
            .def("size", &QuerySet::size, "Get the number of queries in the query set.")
            .def("add", &QuerySet::add, "Add a query to the query set.");

        py::class_<File>(m, "File")
            .def(py::init<const std::string&, const std::string&>(),
                 py::arg("filename"),
                 py::arg("wmoTablePath") = std::string(""))
            .def("execute", &File::execute,
                             py::arg("query_set"),
                             py::arg("next") = static_cast<int>(0),
                             "Execute a query set on the file. Returns a ResultSet object.")
            .def("rewind", &File::rewind,
                           "Rewind the file to the beginning.")
            .def("close", &File::close,
                          "Close the file.")
            .def("__enter__", [](File &f) { return &f; })
            .def("__exit__", [](File &f, py::args args) { f.close(); });

        py::class_<ResultSet>(m, "ResultSet")
            .def("get", &ResultSet::getNumpyArray,
                        py::arg("field_name"),
                        py::arg("group_by") = std::string(""),
                        py::arg("type") = std::string(""),
                        "Get a numpy array of the specified field name. If the group_by "
                        "field is specified, the array is grouped by the specified field."
                        "It is also possible to specify a type to override the default type.")
            .def("get_datetime", &ResultSet::getNumpyDatetimeArray,
                        py::arg("year"),
                        py::arg("month"),
                        py::arg("day"),
                        py::arg("hour"),
                        py::arg("minute") = std::string(""),
                        py::arg("second") = std::string(""),
                        py::arg("group_by") = std::string(""),
                        "Get a numpy array of datetime objects. The datetime objects are "
                        "constructed from the specified year, month, day, hour, minute, "
                        "and second fields. If the minute and second fields are not "
                        "specified, they are assumed to be 0. If the group_by field is "
                        "specified, the datetime objects are grouped by the specified "
                        "field.");
    }
//}  // namespace bufr
//}  // namespace Ingester

