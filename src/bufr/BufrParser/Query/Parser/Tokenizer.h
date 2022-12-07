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
#include <iostream>
#include <sstream>

#include "ObjectFactory.h"


namespace Ingester {
namespace bufr {
    // tokenizer to tokenize a query string in the format */ABCD/EFG/XYZ[1]
    // into a vector of tokens
    // each token is a mnemonic, seperator, index or filter
    // the tokenizer is a recursive descent parser
    // the grammar is as follows
    //
    // subset = "*" | [A-Z_]+
    // index = "[" [0-9]+ "]"
    // sequence = "/" [A-Z_]+
    // target element = "/" [A-Z_]+ index? filter?
    // index = "[" [0-9]+ "]"
    // filter = "{" [A-Z_] (,[A-Z_]+ ")?}"

    // define the tokens
    enum class TokenType {
        Seperator,
        Mnemonic,
        Index,
        Filter,
        Unknown
    };

    // Token abstract base class
    class Token {
    public:
        Token() = default;
        virtual ~Token() = default;
        virtual TokenType type() const = 0;
        virtual std::string str() const = 0;

        /// \brief Check if the next token matches this token
        /// \param iter std::string iterator for the query string
        /// \param end std::string iterator for the end of the query string
        /// \return true if the next token matches this token
        virtual bool match(std::string::const_iterator &iter,
                           std::string::const_iterator &end) const = 0;
    };

    // template base class for tokens
    template <class T>
    class TokenBase : public Token {
    public:
        TokenBase() = default;
        virtual ~TokenBase() = default;

        static std::shared_ptr<Token> parse(std::string::const_iterator &iter,
                                            std::string::const_iterator &end)
        {
            auto regex = T::regex(T::pattern);
            std::smatch match;
            if (std::regex_search(iter, end, match, regex))
            {
                iter += match.length();
                return std::make_shared<T>(match.str());
            }
        }

        /// \brief Check if the next token matches this token
        /// \param iter std::string iterator for the query string
        /// \param end std::string iterator for the end of the query string
        /// \return true if the next token matches this token
        bool match(std::string::const_iterator &iter,
                   std::string::const_iterator &end) const final
        {
            // search the string and check the next substring matches this token
            auto regex = std::regex(T::Pattern);
            std::smatch match;
            if (std::regex_search(iter, end, match, regex))
            {
                // check if the match is at the beginning of the string
                if (match.position() == 0)
                {
                    // move the iterator to the end of the match
                    iter += match.length();
                    return true;
                }
            }

            return false;
        }

        TokenType type() const final
        {
            return T::Type;
        }
    };

    class SeperatorToken : public TokenBase<SeperatorToken> {
    public:
        constexpr static const char* Pattern = "\\/";
        constexpr static const TokenType Type = TokenType::Seperator;

        explicit SeperatorToken(const std::string &target) {}
        std::string str() const override { return "/"; }
    };

    class MnemonicToken : public TokenBase<MnemonicToken> {
    public:
        constexpr static const char* Pattern = "[A-Z0-9_]+";
        constexpr static const TokenType Type = TokenType::Mnemonic;

        explicit MnemonicToken(const std::string &mnemonic) : mnemonic_(mnemonic) {}
        std::string str() const override { return mnemonic_; }
    private:
        std::string mnemonic_;
    };

    class IndexToken : public TokenBase<IndexToken> {
    public:
        constexpr static const char* Pattern = "\\[[0-9]\\]";
        constexpr static const TokenType Type = TokenType::Index;

        explicit IndexToken(const std::string &index) : index_(index) {}
        std::string str() const override { return index_; }
    private:
        std::string index_;
    };

    class FilterToken : public TokenBase<FilterToken> {
    public:
        constexpr static const char* Pattern = "{[A-Z_] (,[A-Z_]+)?}";
        constexpr static const TokenType Type = TokenType::Filter;

        explicit FilterToken(const std::string &filter) : filter_(filter) {}
        std::string str() const override { return filter_; }
    private:
        std::string filter_;
    };

    class UnknownToken : public TokenBase<UnknownToken> {
    public:
        constexpr static const char* Pattern = ".*";
        constexpr static const TokenType Type = TokenType::Unknown;

        explicit UnknownToken(const std::string &unknown = "") : unknown_(unknown) {}
        std::string str() const override { return unknown_; }
    private:
        std::string unknown_;
    };


    // tokenizer to tokenize a query string in the format */ABCD/EFG/XYZ[1]
    class Tokenizer
    {
    public:
        Tokenizer() = default;
        ~Tokenizer() = default;

        // tokenize a query string
        // return a vector of tokens
        std::vector<std::shared_ptr<Token>> tokenize(const std::string &query);
    };


};  // namespace bufr
}  // namespace Ingester

