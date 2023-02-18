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
#include <regex>  // NOLINT
#include <typeinfo>
#include <cstddef>
#include <iostream>
#include <sstream>

#include "eckit/exception/Exceptions.h"


namespace Ingester {
namespace bufr {

    class Token;
    class QueryToken;
    typedef std::vector<std::shared_ptr<Token>> Tokens;

    // Token abstract base class
    class Token {
     public:
        Token() = default;
        virtual ~Token() = default;

        std::string str() const  { return str_; }
        Tokens tokens() const { return subTokens_; }
        virtual std::string debugStr() const = 0;
        virtual std::vector<std::shared_ptr<QueryToken>> queryTokens() const
        {
            throw eckit::BadParameter("Token::queryTokens: called on wrong token type"
                                      + debugStr());
        }

     protected:
        Tokens subTokens_;
        std::string str_;

        virtual void tokenize() {}
    };

    // template base class for tokens
    template <class T>
    class TokenBase : public Token {
     public:
        TokenBase() = default;

        static std::shared_ptr<T> parse(std::string::const_iterator &start,
                                        std::string::const_iterator &end)
        {
            static const auto regex = std::regex(T::Pattern);
            std::shared_ptr<T> token = nullptr;
            std::smatch matches;
            if (std::regex_search(start, end, matches, regex))
            {
                if (matches.position() == 0)
                {
                    start += matches[0].str().size();
                    token = std::make_shared<T>();
                    token->str_ = matches[0].str();
                    token->tokenize();
                }
            }

            return token;
        }

        std::string debugStr() const override { return T::DebugStr; }
    };

    class SeparatorToken : public TokenBase<SeparatorToken> {
     public:
        constexpr static const char* Pattern = "\\/";
        constexpr static const char* DebugStr = "<sep>";
    };

    class AnySubset : public TokenBase<AnySubset> {
     public:
        constexpr static const char* Pattern = "\\*";
        constexpr static const char* DebugStr = "<anySubset>";
    };

    class IndexToken : public TokenBase<IndexToken> {
     public:
        constexpr static const char* Pattern = "\\[[0-9]+\\]";
        constexpr static const char* DebugStr = "<index>";

        std::string debugStr() const override
        {
            std::ostringstream debugStr;
            debugStr << "<index=" << index() << ">";
            return debugStr.str();
        }

        size_t index() const
        {
            return std::stoi(std::string(++str_.begin(), --str_.end()));
        }
    };

    class MnemonicToken : public TokenBase<MnemonicToken> {
     public:
        constexpr static const char* Pattern = "([A-Z0-9_]+)";
        constexpr static const char* DebugStr = "<mnemonic>";

        std::string debugStr() const override
        {
            std::ostringstream debugStr;
            debugStr << "<mnemonic=" << str_ << ">";
            return debugStr.str();
        }
    };

    class FilterToken : public TokenBase<FilterToken> {
     public:
        constexpr static const char* Pattern = "\\{[0-9\\-](\\,[0-9\\-]+)*\\}";
        constexpr static const char* DebugStr = "<filter>";

        std::vector<size_t> indices() { return indices_; }

        void tokenize() final
        {
            static const auto filterRegex =
                std::regex("\\{(\\d+\\-\\d+|\\d+)(\\,(\\d+\\-\\d+|\\d+))?\\}");
            static const auto rangeRegex = std::regex("(\\d+)\\-(\\d+)");
            static const auto indexRegex = std::regex("[^\\-](\\d+)[^\\-]");

            std::vector<size_t> indices;

            // match index
            std::smatch matches;
            if (std::regex_match(str_, matches, filterRegex))
            {
                // match all ranges
                std::smatch rangeMatches;
                std::regex_search(str_, rangeMatches, rangeRegex);
                for (auto rangeMatch : rangeMatches)
                {
                   std::cout << "rangeMatch: " << rangeMatch.str() << std::endl;
                }

                // match all ranges
                std::smatch idxMatches;
                std::regex_search(str_, idxMatches, indexRegex);
                for (auto rangeMatch : rangeMatches)
                {
                    std::cout << "indexMatch: " << rangeMatch.str() << std::endl;
                }
            }
            else
            {
                throw eckit::BadParameter("FilterToken::indices: invalid filter: " + str_);
            }
        }

     private:
        std::vector<size_t> indices_;
    };

    class QueryToken : public TokenBase<QueryToken> {
     public:
        constexpr static const char* Pattern =
            "([A-Z0-9_\\*\\/]+(\\[\\d+\\])?)+(\\{[0-9\\-\\,]+\\})?";
        constexpr static const char* DebugStr = "<query>";
        constexpr static const char* SubPattern = "[^,]+";

        std::string debugStr() const override
        {
            std::ostringstream debugStr;

            debugStr << "<query>";
            for (const auto& token : subTokens_)
            {
                debugStr << token->debugStr();
            }
            debugStr << "</query>";

            return debugStr.str();
        }

        void tokenize() final
        {
            const std::string iterStr = str_;
            auto start = iterStr.begin();
            auto end = iterStr.end();

            while (start != end)
            {
                if (auto anySubset = AnySubset::parse(start, end))
                    subTokens_.push_back(anySubset);
                else if (auto mnemonic = MnemonicToken::parse(start, end))
                    subTokens_.push_back(mnemonic);
                else if (auto sep = SeparatorToken::parse(start, end))
                    subTokens_.push_back(sep);
                else if (auto filter = FilterToken::parse(start, end))
                    subTokens_.push_back(filter);
                else if (auto index = IndexToken::parse(start, end))
                    subTokens_.push_back(index);
                else
                    throw eckit::BadValue("Failed to parse query " + std::string(start, end));
            }
        }

        std::vector<std::shared_ptr<QueryToken>> queryTokens() const final
        {
            return { std::make_shared<QueryToken>(*this) };
        }

        std::vector<Tokens> split() const
        {
            std::vector<Tokens> splitTokens;
            splitTokens.emplace_back();

            for (const auto& token : subTokens_)
            {
                if (auto sep = std::dynamic_pointer_cast<SeparatorToken>(token))
                {
                    splitTokens.emplace_back();
                }
                else
                {
                    splitTokens.back().push_back(token);
                }
            }

            return splitTokens;
        }
    };

    class MultiQueryToken : public TokenBase<MultiQueryToken> {
     public:
        constexpr static const char* Pattern = "^\\[(.*)\\]$";

        constexpr static const char* DebugStr = "<multi>";

        std::string debugStr() const override
        {
            std::ostringstream debugStr;

            debugStr << "<multi>" << std::endl;
            for (const auto& query : queries_)
            {
                debugStr << "  " << query->debugStr() << std::endl;
            }
            debugStr << "</multi>";

            return debugStr.str();
        }

        void tokenize() final
        {
            static const auto multiPattern = "\\[" + std::string(QueryToken::Pattern) +
                                             "((\\,)" + std::string(QueryToken::Pattern) + ")*"
                                             + "\\]";

            static const auto queryRegex =
                std::regex("((?=\\,)|())" + std::string(QueryToken::Pattern));
            static const auto multiRegex = std::regex(multiPattern);

            const std::string iterStr = str_;
            auto start = iterStr.begin();
            auto end = iterStr.end();

            if (std::regex_match(start, end, multiRegex))
            {
                start++; end--;
                for (auto match = std::sregex_iterator(start, end, queryRegex);
                     match != std::sregex_iterator();
                     ++match)
                {
                    const std::string matchStr = match->str();
                    auto matchStart = matchStr.begin();
                    auto matchEnd = matchStr.end();
                    queries_.push_back(QueryToken::parse(matchStart, matchEnd));
                }
            }
            else
            {
                throw eckit::BadParameter("Invalid multi query " + str_ + ".");
            }
        }

        std::vector<std::shared_ptr<QueryToken>> queryTokens() const final
        {
            return queries_;
        }

        Tokens split() const
        {
            Tokens queryTokens;
            for (const auto& token : subTokens_)
            {
                if (auto sep = std::dynamic_pointer_cast<QueryToken>(token))
                {
                    queryTokens.push_back(token);
                }
                else
                {
                    throw eckit::BadParameter("ParseError: Expected list of queries" + str_ + ".");
                }
            }

            return queryTokens;
        }

     private:
        std::vector<std::shared_ptr<QueryToken>> queries_;
    };

    // tokenizer to tokenize a query string in the format */ABCD/EFG/XYZ[1]
    class Tokenizer
    {
     public:
        Tokenizer() = default;
        ~Tokenizer() = default;

        // tokenize a query string
        // return a vector of tokens
        static Tokens tokenize(const std::string &query);
    };


};  // namespace bufr
}  // namespace Ingester

