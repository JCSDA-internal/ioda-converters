/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <pybind11/pybind11.h>
#include <memory>
#include <vector>
#include <string>

#include "BufrParser/Query/QuerySet.h"
#include "BufrParser/Query/File.h"
#include "BufrParser/Query/ResultSet.h"
#include "BufrParser/BufrParser.h"
#include "IodaEncoder/IodaEncoder.h"
#include "DataObject.h"
#include "DataContainer.h"
#include "bufr2ioda_func.h"

namespace py = pybind11;

using Ingester::bufr::ResultSet;
using Ingester::bufr::QuerySet;
using Ingester::bufr::File;
using Ingester::BufrParser;
using Ingester::IodaEncoder;
using Ingester::DataObjectBase;
using Ingester::DataContainer;
using Ingester::CategoryMap;
using Ingester::parse1;
using Ingester::encode_save;

    PYBIND11_MODULE(bufr, m)
    {
        m.doc() = "Provides the ability to process data from BUFR files.";

        m.def("parse", &parse1, "A function to parse a config file and get the data container");

        m.def("encode_save", [](const std::string& yamlPath, std::shared_ptr<DataContainer> shared_data_container) {
        encode_save(yamlPath, shared_data_container);
        }, "A function to save data container into a file");

        py::class_<BufrParser>(m, "BufrParser")
            .def(py::init<const eckit::LocalConfiguration&>())
            .def("parse", &BufrParser::parse,
                           py::arg("size_t"),
                           "Get Parser to parse a config file and get the data container.");

        py::class_<IodaEncoder>(m, "IodaEncoder")
            .def(py::init<const eckit::Configuration&>())
            .def("encode", &IodaEncoder::encode,
                           "Get the class to encode the dataset");

        py::class_<DataObjectBase, std::shared_ptr<DataObjectBase>>(m, "DataObjectBase");

        py::class_<DataContainer, std::shared_ptr<DataContainer>>(m, "DataContainer")
            .def(py::init<>())
            .def(py::init<const Ingester::CategoryMap&>())
            .def("add", &DataContainer::addNumpyArray,
                        py::arg("field_name"),
                        py::arg("data_object"),
                        py::arg("dim_paths"),
                        py::arg("category_id") = std::vector<std::string>(),
                        "Add a new variable object into the data container.")
            .def("get", &DataContainer::getNumpyArray, "Get the value of the variable object as numpy array. ")
            .def("getPaths", &DataContainer::getPaths, "Get path names for a field.")
            .def("set", &DataContainer::set, "Set the variable value back after re-map.")
            .def("getCategoryMap", &DataContainer::getCategoryMap, "Get the map.")
            .def("allSubCategories", &DataContainer::allSubCategories, "Get the sub categories for the satellite.");

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

