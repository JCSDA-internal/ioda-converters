/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

namespace Ingester {
namespace bufr {

    namespace __tokenizers
    {
        namespace Filter
        {
            struct Token
            {
            };

            struct Index : public Token
            {
                size_t index;
            };

            struct Operator : public Token
            {
            };

            struct RangeOperator : public Operator
            {
                static const char Seperator = '-';

                size_t start;
                size_t end;
            };

            struct List : public Operator
            {
                static const char Seperator = ',';

                std::vector<Token> tokens;
            };

            class Tokenizer
            {
            public:
                static std::vector<Token> tokenize(const std::string& str);
            };
        }

        namespace Query
        {
            struct Token
            {
            };

            struct Seperator : Token
            {
                static const char Separator = '/';
            };

            struct Mnemonic : public Token
            {
                std::string mmenonic;
            };

            struct Index : public Token
            {
                static const char Start = '[';
                static const char End = ']';

                size_t index;
            };

            struct Filter : public Token
            {
                static const char Start = '{';
                static const char End = '}';

                std::vector<Filter::Token> tokens;
            };

            class Tokenizer
            {
            public:
                static std::vector<Token> tokenize(const std::string& query);
            };
        }


    }  // namespace __detials

    struct Query
    {
        std::string queryStr;
        std::string subset;
        std::vector<std::string> mnemonics;
        int index;
    };

    /// \brief Parses a user supplied query string into its component parts.
    /// \note Will be refactored to properly tokenize the query string.
    class QueryParser
    {
     public:
        static std::vector<Query> parse(const std::string& queryStr);

     private:
        /// \brief Split a multi query (ex: ["*/CLONH", "*/CLON"]) into a vector of single queries.
        /// \param query The query to split.
        static std::vector<std::string> splitMultiquery(const std::string& query);

        /// \brief Split a single query (ex: "*/ROSEQ1/ROSEQ2/PCCF[2]") into its component parts.
        /// \param query The query to split.
        static Query splitQueryStr(const std::string& query);

     private:
        /// \brief Private constructor.
        QueryParser();
    };
}  // namespace bufr
}  // namespace Ingester
