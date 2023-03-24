//
// Created by Ronald McLaren on 3/20/23.
//


#include "eckit/exception/Exceptions.h"

#include <pybind11/pybind11.h>
#include <pybind11/numpy.h>
#include <pybind11/stl.h>
#include <pybind11/stl_bind.h>

#include <vector>
#include <string>
#include <regex>
#include <iostream>

#include "QuerySet.h"
#include "File.h"
#include "ResultSet.h"

namespace Ingester {
namespace bufr {
    namespace py = pybind11;

    PYBIND11_MODULE(bufr, m)
    {
        m.doc() = "Provides BUFR querying abilities to python.";

        py::register_exception<eckit::BadParameter>(m, "Exception");
        py::register_exception<eckit::BadValue>(m, "Exception");

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
                 py::arg("next") = int(0))
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

