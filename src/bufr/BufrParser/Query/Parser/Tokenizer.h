//
// Created by Ronald McLaren on 11/28/22.
//

#pragma once

#include <string>
#include <vector>
#include <memory>

namespace Ingester {
namespace bufr {

namespace filter {
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

    typedef ObjectFactory<Token> TokenFactory;

    class Tokenizer
    {
     public:
        static std::vector<Token> tokenize(const std::string& str);
    };
}

namespace query {
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

    typedef ObjectFactory<Token> TokenFactory;

    class Tokenizer
    {
     public:
        static std::vector<Token> tokenize(const std::string& query);
    };
}


}  // bufr
}  // Ingester

