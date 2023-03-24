/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
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

namespace Ingester {
namespace bufr {
    namespace py = pybind11;

    PYBIND11_MODULE(bufr, m)
    {
        m.doc() = "Provides BUFR querying abilities to python.";

        py::class_<QuerySet>(m, "QuerySet")
            .def(py::init<>())
            .def(py::init<const std::vector<std::string>&>())
            .def("size", &QuerySet::size)
            .def("add",
                 (void (QuerySet::*)(const std::string&, const std::string&)) &QuerySet::add);

        py::class_<File>(m, "File")
            .def(py::init<const std::string&, const std::string&>(),
                 py::arg("filename"),
                 py::arg("wmoTablePath") = std::string(""))
            .def("execute",
                 &File::execute,
                 py::arg("query_set"),
                 py::arg("next") = static_cast<int>(0))
            .def("rewind", &File::rewind)
            .def("close", &File::close)
            .def("__enter__", [](File &f) { return &f; })
            .def("__exit__", [](File &f, py::args args) { f.close(); });

        py::class_<ResultSet>(m, "ResultSet")
            .def("get", &ResultSet::getNumpyArray,
                        py::arg("field_name"),
                        py::arg("group_by") = std::string(""));
    }
}  // namespace bufr
}  // namespace Ingester

