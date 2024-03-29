#include "fmt/core.h"
#include "tokenizer/tokenizer.h"
#include "analyser/analyser.h"

namespace fmt {
    template<>
    struct formatter<c0::ErrorCode> {
        template<typename ParseContext>
        constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const c0::ErrorCode &p, FormatContext &ctx) {
            std::string name;
            switch (p) {
                case c0::ErrNoError:
                    name = "No error.";
                    break;
                case c0::ErrStreamError:
                    name = "Stream error.";
                    break;
                case c0::ErrEOF:
                    name = "EOF";
                    break;
                case c0::ErrInvalidInput:
                    name = "The input is invalid.";
                    break;
                case c0::ErrInvalidIdentifier:
                    name = "Identifier is invalid";
                    break;
                case c0::ErrIntegerOverflow:
                    name = "The integer is too big(int64_t).";
                    break;
                case c0::ErrNoBegin:
                    name = "The program should start with 'begin'.";
                    break;
                case c0::ErrNoEnd:
                    name = "The program should end with 'end'.";
                    break;
                case c0::ErrNeedIdentifier:
                    name = "Need an identifier here.";
                    break;
                case c0::ErrConstantNeedValue:
                    name = "The constant need a value to initialize.";
                    break;
                case c0::ErrNoSemicolon:
                    name = "Zai? Wei shen me bu xie fen hao.";
                    break;
                case c0::ErrInvalidVariableDeclaration:
                    name = "The declaration is invalid.";
                    break;
                case c0::ErrIncompleteExpression:
                    name = "The expression is incomplete.";
                    break;
                case c0::ErrNotDeclared:
                    name = "The variable or constant must be declared before being used.";
                    break;
                case c0::ErrAssignToConstant:
                    name = "Trying to assign value to a constant.";
                    break;
                case c0::ErrDuplicateDeclaration:
                    name = "The variable or constant has been declared.";
                    break;
                case c0::ErrNotInitialized:
                    name = "The variable has not been initialized.";
                    break;
                case c0::ErrInvalidAssignment:
                    name = "The assignment statement is invalid.";
                    break;
                case c0::ErrInvalidPrint:
                    name = "The output statement is invalid.";
                    break;
                case c0::ErrNumberStartWithZero:
                    name = "Decimal literal shouldn't start with zero.";
                    break;
                case c0::ErrInvalidEscape:
                    name = "The escape is invalid.";
                    break;
                case c0::ErrInvalidCharLiteral:
                    name = "The char literal is invalid.";
                    break;
                case c0::ErrInvalidStringLiteral:
                    name = "The string literal is invalid.";
                    break;
                case c0::ErrNoBracket:
                    name = "Need a bracket here.";
                    break;
                case c0::ErrNoTypeSpecifier:
                    name = "Need a type specifier here.";
                    break;
                case c0::ErrNoBrace:
                    name = "Need a brace here.";
                    break;
                case c0::ErrInvalidStatement:
                    name = "The statement is invalid.";
                    break;
                case c0::ErrIncompleteConditionStatement:
                    name = "The condition statement is incomplete.";
                    break;
                case c0::ErrNoMain:
                    name = "Main function is not defined.";
                    break;
                case c0::ErrSurplusTokenAfterFunctionDefinition:
                    name = "Found surplus token after function definition.";
                    break;
                case c0::ErrNoPreviousLevel:
                    name = "No previous level.";
                    break;
                case c0::ErrNoReturnValue:
                    name = "No return value.";
                    break;
                case c0::ErrParamsSizeNotIdentical:
                    name = "Params size not identical.";
                    break;
                case c0::ErrWIP:
                    name = "WIP feature.";
                    break;
                case c0::ErrUseVoidFunctionInPrimaryExpression:
                    name = "Use void function in primary expression.";
                    break;
            }
            return format_to(ctx.out(), name);
        }
    };

    template<>
    struct formatter<c0::CompilationError> {
        template<typename ParseContext>
        constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const c0::CompilationError &p, FormatContext &ctx) {
            return format_to(ctx.out(), "Line: {} Column: {} Error: {}", p.GetPos().first, p.GetPos().second,
                             p.GetCode());
        }
    };
}

namespace fmt {
    template<>
    struct formatter<c0::Token> {
        template<typename ParseContext>
        constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const c0::Token &p, FormatContext &ctx) {
            return format_to(ctx.out(),
                             "Line: {} Column: {} Type: {} Value: {}",
                             p.GetStartPos().first, p.GetStartPos().second, p.GetType(), p.GetValueString());
        }
    };

    template<>
    struct formatter<c0::TokenType> {
        template<typename ParseContext>
        constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const c0::TokenType &p, FormatContext &ctx) {
            std::string name;
            switch (p) {
                case c0::NULL_TOKEN:
                    name = "NullToken";
                    break;
                case c0::IDENTIFIER:
                    name = "Identifier";
                    break;
                case c0::CONST:
                    name = "Const";
                    break;
                case c0::CHAR:
                    name = "Char";
                    break;
                case c0::INT:
                    name = "Int";
                    break;
                case c0::DOUBLE:
                    name = "Double";
                    break;
                case c0::VOID:
                    name = "Void";
                    break;
                case c0::IF:
                    name = "If";
                    break;
                case c0::ELSE:
                    name = "Else";
                    break;
                case c0::WHILE:
                    name = "While";
                    break;
                case c0::RETURN:
                    name = "Return";
                    break;
                case c0::PRINT:
                    name = "Print";
                    break;
                case c0::SCAN:
                    name = "Scan";
                    break;
                case c0::PLUS_SIGN:
                    name = "PlusSign";
                    break;
                case c0::MINUS_SIGN:
                    name = "MinusSign";
                    break;
                case c0::MULTIPLICATION_SIGN:
                    name = "MultiplicationSign";
                    break;
                case c0::DIVISION_SIGN:
                    name = "DivisionSign";
                    break;
                case c0::DECIMAL_LITERAL:
                    name = "UnsignedInteger";
                    break;
                case c0::FLOATING_LITERAL:
                    name = "FloatingLiteral";
                    break;
                case c0::GREATER_SIGN:
                    name = "GreaterSign";
                    break;
                case c0::GREATER_OR_EQUAL_SIGN:
                    name = "GreaterOrEqualSign";
                    break;
                case c0::LESS_SIGN:
                    name = "LessSign";
                    break;
                case c0::LESS_OR_EQUAL_SIGN:
                    name = "LessOrEqualSign";
                    break;
                case c0::EQUAL_SIGN:
                    name = "EqualSign";
                    break;
                case c0::NOT_EQUAL_SIGN:
                    name = "NotEqualSign";
                    break;
                case c0::CHAR_LITERAL:
                    name = "CharLiteral";
                    break;
                case c0::STRING_LITERAL:
                    name = "StringLiteral";
                    break;
                case c0::LEFT_BRACKET:
                    name = "LeftBracket";
                    break;
                case c0::RIGHT_BRACKET:
                    name = "RightBracket";
                    break;
                case c0::LEFT_BRACE:
                    name = "LeftBrace";
                    break;
                case c0::RIGHT_BRACE:
                    name = "RightBrace";
                    break;
                case c0::COMMA:
                    name = "Comma";
                    break;
                case c0::COLON:
                    name = "Colon";
                    break;
                case c0::SEMICOLON:
                    name = "Semicolon";
                    break;
                case c0::ASSIGNMENT_OPERATOR:
                    name = "AssignmentOperator";
                    break;
                case c0::STRUCT:
                    name = "Struct";
                    break;
                case c0::SWITCH:
                    name = "Switch";
                    break;
                case c0::CASE:
                    name = "Case";
                    break;
                case c0::DEFAULT:
                    name = "Default";
                    break;
                case c0::FOR:
                    name = "For";
                    break;
                case c0::DO:
                    name = "Do";
                    break;
                case c0::BREAK:
                    name = "Break";
                    break;
                case c0::CONTINUE:
                    name = "Continue";
                    break;
                default:
                    name = "Noname";
                    break;
            }
            return format_to(ctx.out(), name);
        }
    };
}

namespace fmt {
    template<>
    struct formatter<c0::Operation> {
        template<typename ParseContext>
        constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const c0::Operation &p, FormatContext &ctx) {
            std::string name;
            switch (p) {
                case c0::ILL:
                    name = "ILL";
                    break;
                case c0::ADD:
                    name = "ADD";
                    break;
                case c0::SUB:
                    name = "SUB";
                    break;
                case c0::MUL:
                    name = "MUL";
                    break;
                case c0::DIV:
                    name = "DIV";
                    break;
                case c0::WRT:
                    name = "WRT";
                    break;
                case c0::LIT:
                    name = "LIT";
                    break;
                case c0::LOD:
                    name = "LOD";
                    break;
                case c0::STO:
                    name = "STO";
                    break;
                case c0::IPUSH:
                    name = "IPUSH";
                    break;
                case c0::LOADA:
                    name = "LOADA";
                    break;
                case c0::ILOAD:
                    name = "ILOAD";
                    break;
                case c0::IMUL:
                    name = "IMUL";
                    break;
                case c0::IDIV:
                    name = "IDIV";
                    break;
                case c0::IADD:
                    name = "IADD";
                    break;
                case c0::ISUB:
                    name = "ISUB";
                    break;
                case c0::RET:
                    name = "RET";
                    break;
                case c0::IRET:
                    name = "IRET";
                    break;
                case c0::JE:
                    name = "JE";
                    break;
                case c0::JNE:
                    name = "JNE";
                    break;
                case c0::JL:
                    name = "JL";
                    break;
                case c0::JGE:
                    name = "JGE";
                    break;
                case c0::JG:
                    name = "JG";
                    break;
                case c0::JLE:
                    name = "JLE";
                    break;
                case c0::ICMP:
                    name = "ICMP";
                    break;
                case c0::JMP:
                    name = "JMP";
                    break;
                case c0::CALL:
                    name = "CALL";
                    break;
                case c0::ISTORE:
                    name = "ISTORE";
                    break;
                case c0::SPRINT:
                    name = "SPRINT";
                    break;
                case c0::IPRINT:
                    name = "IPRINT";
                    break;
                case c0::CPRINT:
                    name = "CPRINT";
                    break;
                case c0::LOADC:
                    name = "LOADC";
                    break;
                case c0::ISCAN:
                    name = "ISCAN";
                    break;
                case c0::CSCAN:
                    name = "CSCAN";
                    break;
                case c0::NOP:
                    name = "NOP";
                    break;
                case c0::POP:
                    name = "POP";
                    break;
                case c0::INEG:
                    name = "INEG";
                    break;
                case c0::BIPUSH:
                    name = "BIPUSH";
                    break;
                case c0::PRINTL:
                    name = "PRINTL";
                    break;
                case c0::I2C:
                    name = "I2C";
                    break;
            }
            return format_to(ctx.out(), name);
        }
    };

    template<>
    struct formatter<c0::Instruction> {
        template<typename ParseContext>
        constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

        template<typename FormatContext>
        auto format(const c0::Instruction &p, FormatContext &ctx) {
            std::string name;
            switch (p.ParameterNum()) {
                case 2:
                    return format_to(ctx.out(), "{} {} {}, {}", p.GetIndex(), p.GetOperation(), p.GetX(), p.GetY());
                case 1:
                    return format_to(ctx.out(), "{} {} {}", p.GetIndex(), p.GetOperation(), p.GetX());
                default:
                    return format_to(ctx.out(), "{} {}", p.GetIndex(), p.GetOperation());
            }
        }
    };
}