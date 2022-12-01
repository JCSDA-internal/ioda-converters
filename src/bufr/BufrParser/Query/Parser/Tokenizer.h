/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <memory>
#include <regex>
#include <typeinfo>
#include <cstddef>

#include "ObjectFactory.h"


namespace Ingester {
namespace bufr {

namespace filter {
    struct Token
    {
        virtual bool contains() = 0;
    };

    struct Index : public Token
    {
        static const std::regex pattern = std::regex("\\d");
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

    typedef ObjectFactory<Token> TokenFactory;

    class Tokenizer
    {
     public:
        static std::vector<Token> tokenize(const std::string& str);
    };
}  // namespace filter

namespace query {

    struct Token
    {
        virtual std::vector<Token> parse() = 0;
    };

    typedef std::shared_ptr<Token> TokenType;

    template<typename T>
    struct TokenBase : public Token
    {
        static bool contains(std::string::const_iterator start, std::string::const_iterator end)
        {
            return std::regex_match(start, end, std::regex(T::pattern));
        }
    };

    struct Seperator : TokenBase<Seperator>
    {
        constexpr static const char* pattern = "\\/";
    };

    struct Mnemonic : public TokenBase<Mnemonic>
    {
        constexpr static const char* pattern = "[A-Z]+";

        std::string mmenonic;
    };

    struct Index : public TokenBase<Index>
    {
        constexpr static const char* pattern = "\\[\\d\\]";

        size_t index;
    };

    struct Filter : public TokenBase<Filter>
    {
        constexpr static const char* pattern = "\\{.+\\}";

        std::vector<TokenType> subTokens;
    };


    typedef ObjectFactory<TokenType> TokenFactory;

    struct NullToken {};
    template<typename Head, typename... Tail> struct TokenList
    {
        using head = Head;
        using tail = TokenList<Tail...>;
    };

    template<typename Single> struct TokenList <Single>
    {
        using head = Single;
        using tail = NullToken;
    };

    class Tokenizer
    {
     public:
        template<typename... Tokens>
        static std::vector<TokenType> tokenize(const std::string& query)
        {
            _tokenize(query.begin(), query.end());
        }

     private:
        static std::vector<TokenType> _tokenize(std::string::const_iterator start,
                                                std::string::const_iterator end)
        {

            // find seperators
            // get mnemonic in each division
            // check for indices
            // check and tokenize filters

            auto valid = std::regex(
                "(\\*|[A-Z]+)(\\[\\d\\])?(\\/([A-Z]+))*\\/[A-Z]+((\\[\\d\\])|(\\{.+\\}))?")

            auto seps = std::regex("\\/")

            std::vector<TokenType> tokens;

            while (true)
            {
                if (Mnemonic::contains(start, end))
                {
                    tokens.push_back(Mnemonic::make(start, end));
                }
                else if (Seperator::contains(start, end))
                {
                    tokens.push_back(Seperator::make(start, end));
                }
                else if (Index::contains(start, end))
                {

                }
                else
                {
                    break;
                }
            }

            return tokens;
        }
    };
}  // namespace query

}  // bufr
}  // Ingester

