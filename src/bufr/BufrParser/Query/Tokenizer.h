/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
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
#include <set>

#include "eckit/exception/Exceptions.h"


namespace Ingester {
namespace bufr {

    class Token;
    class QueryToken;
    typedef std::vector<std::shared_ptr<Token>> Tokens;

    /// \brief Token abstract base class
    class Token {
     public:
        Token() = default;
        virtual ~Token() = default;

        /// \brief Get the string representation of the token
        std::string str() const  { return str_; }

        /// \brief Get the debug string for the token.
        virtual std::string debugStr() const = 0;

        /// \brief Get the subtokens for this token.
        virtual std::vector<std::shared_ptr<QueryToken>> queryTokens() const
        {
            throw eckit::BadParameter("Token::queryTokens: called on wrong token type"
                                      + debugStr());
        }

     protected:
        Tokens subTokens_;
        std::string str_;

        /// \brief Tokenize the str_ into tokens.
        virtual void tokenize() {}
    };

    /// \brief Template base class for tokens
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

    /// \brief Token for the seperator between query components (i.e. '/')
    class SeparatorToken : public TokenBase<SeparatorToken> {
     public:
        constexpr static const char* Pattern = "\\/";
        constexpr static const char* DebugStr = "<sep>";
    };

    /// \brief Token for the wildcard subset
    class AnySubset : public TokenBase<AnySubset> {
     public:
        constexpr static const char* Pattern = "\\*";
        constexpr static const char* DebugStr = "<anySubset>";
    };

    /// \brief Token for the index part of a query (i.e. '[2]')
    class IndexToken : public TokenBase<IndexToken> {
     public:
        constexpr static const char* Pattern = "\\[[0-9]+\\]";
        constexpr static const char* DebugStr = "<index>";

        // \brief Get the debug string for the token.
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

    /// \brief Token for the mnemonic parts of a query (i.e. 'TMBR')
    class MnemonicToken : public TokenBase<MnemonicToken> {
     public:
        constexpr static const char* Pattern = "([A-Z0-9_]+)";
        constexpr static const char* DebugStr = "<mnemonic>";

        // \brief Get the debug string for the token.
        std::string debugStr() const override
        {
            std::ostringstream debugStr;
            debugStr << "<mnemonic=" << str_ << ">";
            return debugStr.str();
        }
    };

    /// \brief Token for the filter parts of a query (i.e. '{1-3,5}')
    class FilterToken : public TokenBase<FilterToken> {
     public:
        constexpr static const char* Pattern = "\\{\\d+(\\-\\d+)?(\\,\\d+(\\-\\d+)?)*\\}";
        constexpr static const char* DebugStr = "<filter>";

        std::vector<size_t> indices() { return indices_; }

        /// \brief Tokenize the str_ into tokens.
        void tokenize() final
        {
            static const auto filterRegex =
                std::regex("\\{(\\d+(\\-\\d+)?)(,\\d+(\\-\\d+)?)*\\}");

            std::set<size_t> indices;

            // match index
            std::smatch matches;
            if (std::regex_match(str_, matches, filterRegex))
            {
                // regular expression pattern to match ranges and standalone numbers
                static const std::regex pattern("(\\d+)-(\\d+)|(\\d+)");

                std::smatch match;

                // iterate over all matches in the input string
                for (auto it = std::sregex_iterator(str_.begin(), str_.end(), pattern);
                     it != std::sregex_iterator();
                     ++it)
                {
                    // check which capture group matched
                    if ((*it)[1].matched && (*it)[2].matched)  // range match
                    {
                        int start = std::stoi((*it)[1].str());
                        int end = std::stoi((*it)[2].str());
                        for (int i = start; i <= end; i++)
                        {
                            indices.insert(i);
                        }
                    }
                    else  // standalone number match
                    {
                        int number = std::stoi((*it)[3].str());
                        indices.insert(number);
                    }
                }
            }
            else
            {
                throw eckit::BadParameter("FilterToken::indices: invalid filter: " + str_);
            }

            indices_ = std::vector<size_t>(indices.begin(), indices.end());
        }

        /// \brief Get the debug string for the token.
        std::string debugStr() const override
        {
            std::ostringstream debugStr;

            debugStr << "<filter=";
            for (const auto& index : indices_)
            {
                debugStr << index;
                if (index != indices_.back()) debugStr << ",";
            }
            debugStr << ">";

            return debugStr.str();
        }

     private:
        std::vector<size_t> indices_;
    };

    /// \brief Token for a query (i.e. '*/BRIT{2-4}/PCCF[2]'). This token contains tokens for all
    ///        the elements of the query string.
    class QueryToken : public TokenBase<QueryToken> {
     public:
        constexpr static const char* Pattern =
            "([A-Z0-9_\\*\\/]+(\\[\\d+\\])?+(\\{[0-9\\-\\,]+\\})?)+";
        constexpr static const char* DebugStr = "<query>";
        constexpr static const char* SubPattern = "[^,]+";

        // \brief Get the debug string for the token.
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

        /// \brief Tokenize the str_ into tokens.
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

        /// \brief Split the query by its seperators (i.e. '*/BRIT{2-4}/PCCF[2]' -> [*, BRIT{2-4},
        ///        PCCF[2]].
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

    /// \brief Token for a multi-query (i.e. '[*/BRIT/TMBR,*/BRITCSTC/TMBR]') This token will have
    ///        sub-tokens for each query.
    class MultiQueryToken : public TokenBase<MultiQueryToken> {
     public:
        constexpr static const char* Pattern = "^\\[(.*)\\]$";
        constexpr static const char* DebugStr = "<multi>";

        /// \brief Get the debug string for the token.
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

        /// \brief Tokenize the str_ into tokens.
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

        /// \brief Return the Querys (tokens) contained in the multi-query.
        std::vector<std::shared_ptr<QueryToken>> queryTokens() const final
        {
            return queries_;
        }

        /// \brief Split the multi-query into a vector of Query (tokens).
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

    /// \brief Static class to tokenize a query string
    class Tokenizer
    {
     public:
        Tokenizer() = default;
        ~Tokenizer() = default;

        /// \brief Tokenize a query string
        /// \return A vector of tokens
        static Tokens tokenize(const std::string &query);
    };


};  // namespace bufr
}  // namespace Ingester

