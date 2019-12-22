#include "tokenizer/tokenizer.h"

#include <regex>
#include <cctype>
#include <sstream>

namespace c0 {
    TokenType getStringTokenType(std::string str) {
        if (str == "const")
            return TokenType::CONST;
        else if (str == "void")
            return TokenType::VOID;
        else if (str == "int")
            return TokenType::INT;
        else if (str == "char")
            return TokenType::CHAR;
        else if (str == "double")
            return TokenType::DOUBLE;
        else if (str == "struct")
            return TokenType::STRUCT;
        else if (str == "if")
            return TokenType::IF;
        else if (str == "else")
            return TokenType::ELSE;
        else if (str == "switch")
            return TokenType::SWITCH;
        else if (str == "case")
            return TokenType::CASE;
        else if (str == "default")
            return TokenType::DEFAULT;
        else if (str == "while")
            return TokenType::WHILE;
        else if (str == "for")
            return TokenType::FOR;
        else if (str == "do")
            return TokenType::DO;
        else if (str == "return")
            return TokenType::RETURN;
        else if (str == "break")
            return TokenType::BREAK;
        else if (str == "continue")
            return TokenType::CONTINUE;
        else if (str == "print")
            return TokenType::PRINT;
        else if (str == "scan")
            return TokenType::SCAN;
        else
            return TokenType::IDENTIFIER;
    }

    std::optional<char> getEscaped(char ch) {
        switch (ch) {
            case '\\':
                return '\\';
            case '\'':
                return '\'';
            case '"':
                return '"';
            case 'n':
                return '\n';
            case 'r':
                return '\r';
            case 't':
                return '\t';
                // TODO '\x'<hexadecimal-digit><hexadecimal-digit> I don't know what this means... \x11 we need a new state
            default:
                return {};
        }
    }

    std::pair<std::optional<Token>, std::optional<CompilationError>> Tokenizer::NextToken() {
        if (!_initialized)
            readAll();
        if (_rdr.bad())
            return std::make_pair(std::optional<Token>(),
                                  std::make_optional<CompilationError>(0, 0, ErrorCode::ErrStreamError));
        if (isEOF())
            return std::make_pair(std::optional<Token>(),
                                  std::make_optional<CompilationError>(0, 0, ErrorCode::ErrEOF));
        auto p = nextToken();
        if (p.second.has_value())
            return std::make_pair(p.first, p.second);
        auto err = checkToken(p.first.value());
        if (err.has_value())
            return std::make_pair(p.first, err.value());
        return std::make_pair(p.first, std::optional<CompilationError>());
    }

    std::pair<std::vector<Token>, std::optional<CompilationError>> Tokenizer::AllTokens() {
        std::vector<Token> result;
        while (true) {
            auto p = NextToken();
            if (p.second.has_value()) {
                if (p.second.value().GetCode() == ErrorCode::ErrEOF)
                    return std::make_pair(result, std::optional<CompilationError>());
                else
                    return std::make_pair(std::vector<Token>(), p.second);
            }
            result.emplace_back(p.first.value());
        }
    }

    // 注意：这里的返回值中 Token 和 CompilationError 只能返回一个，不能同时返回。
    std::pair<std::optional<Token>, std::optional<CompilationError>> Tokenizer::nextToken() {
        // 用于存储已经读到的组成当前token字符
        std::stringstream ss;
        // 分析token的结果，作为此函数的返回值
        std::pair<std::optional<Token>, std::optional<CompilationError>> result;
        // <行号，列号>，表示当前token的第一个字符在源代码中的位置
        std::pair<int64_t, int64_t> pos;
        // 记录当前自动机的状态，进入此函数时是初始状态
        DFAState current_state = DFAState::INITIAL_STATE;
        // 这是一个死循环，除非主动跳出
        // 每一次执行while内的代码，都可能导致状态的变更
        std::string escape = ""; // bool escape = !escape.empty()
        while (true) {
            // 读一个字符，请注意auto推导得出的类型是std::optional<char>
            // 这里其实有两种写法
            // 1. 每次循环前立即读入一个 char
            // 2. 只有在可能会转移的状态读入一个 char
            // 因为我们实现了 unread，为了省事我们选择第一种
            auto current_char = nextChar();
            // 针对当前的状态进行不同的操作
            switch (current_state) {

                // 初始状态
                // 这个 case 我们给出了核心逻辑，但是后面的 case 不用照搬。
                case INITIAL_STATE: {
                    // 已经读到了文件尾
                    if (!current_char.has_value())
                        // 返回一个空的token，和编译错误ErrEOF：遇到了文件尾
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(0, 0, ErrEOF));

                    // 获取读到的字符的值，注意auto推导出的类型是char
                    auto ch = current_char.value();
                    // 标记是否读到了不合法的字符，初始化为否
                    auto invalid = false;

                    // 使用了自己封装的判断字符类型的函数，定义于 tokenizer/utils.hpp
                    // see https://en.cppreference.com/w/cpp/string/byte/isblank
                    if (c0::isspace(ch)) // 读到的字符是空白字符（空格、换行、制表符等）
                        current_state = DFAState::INITIAL_STATE; // 保留当前状态为初始状态，此处直接break也是可以的
                    else if (!c0::isprint(ch)) // control codes and backspace
                        invalid = true;
                    else if (c0::isdigit(ch) && ch != '0')
                        current_state = DFAState::NON_ZERO;
                    else if (c0::isalpha(ch)) // C0变量不能以_开头
                        current_state = DFAState::IDENTIFIER_STATE; // 切换到标识符的状态
                    else {
                        switch (ch) {
                            case '0':
                                current_state = DFAState::ZERO;
                                break;
                            case '=':
                                current_state = DFAState::EQUAL_SIGN_STATE;
                                break;
                            case '-':
                                current_state = DFAState::MINUS_SIGN_STATE;
                                break;
                            case '+':
                                current_state = DFAState::PLUS_SIGN_STATE;
                                break;
                            case '*':
                                current_state = DFAState::MULTIPLICATION_SIGN_STATE;
                                break;
                            case '/':
                                current_state = DFAState::DIVISION_SIGN_STATE;
                                break;
                            case ':':
                                current_state = DFAState::COLON_STATE;
                                break;
                            case ';':
                                current_state = DFAState::SEMICOLON_STATE;
                                break;
                            case '(':
                                current_state = DFAState::LEFTBRACKET_STATE;
                                break;
                            case ')':
                                current_state = DFAState::RIGHTBRACKET_STATE;
                                break;
                            case ',':
                                current_state = DFAState::COMMA_STATE;
                                break;
                            case '{':
                                current_state = DFAState::LEFTBRACE_STATE;
                                break;
                            case '}':
                                current_state = DFAState::RIGHTBRACE_STATE;
                                break;
                            case '<':
                                current_state = DFAState::LESS_SIGN_STATE;
                                break;
                            case '>':
                                current_state = DFAState::GREATER_SIGN_STATE;
                                break;
                            case '!':
                                current_state = DFAState::EXCLAMATION_SIGN_STATE;
                                break;
                            case '.':
                                current_state = DFAState::FLOAT_STATE;
                                break;
                            case '\'':
                                current_state = DFAState::CHAR_LITERAL_STATE;
                                break;
                            case '\"':
                                current_state = DFAState::STRING_LITERAL_STATE;
                                break;
                            default:
                                invalid = true;
                                break;
                        }
                    }
                    // 如果读到的字符导致了状态的转移，说明它是一个token的第一个字符
                    if (current_state != DFAState::INITIAL_STATE)
                        pos = previousPos(); // 记录该字符的的位置为token的开始位置
                    // 读到了不合法的字符
                    if (invalid) {
                        // 回退这个字符
                        unreadLast();
                        // 返回编译错误：非法的输入
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(pos, ErrorCode::ErrInvalidInput));
                    }
                    // 如果读到的字符导致了状态的转移，说明它是一个token的第一个字符
                    if (current_state != DFAState::INITIAL_STATE) // ignore white spaces
                        ss << ch; // 存储读到的字符
                    break;
                }

                case NON_ZERO: {
                    // 请填空：
                    // 如果当前已经读到了文件尾，则解析已经读到的字符串为整数
                    if (!current_char.has_value()) { // 当前已经读到了文件尾
                        // 解析已经读到的字符串为整数
                        try {
                            // 解析成功则返回无符号整数类型的token
                            return std::make_pair(
                                    std::make_optional<Token>(TokenType::DECIMAL_LITERAL, std::stoi(ss.str()), pos,
                                                              currentPos()),
                                    std::optional<CompilationError>());
                        } catch (...) {
                            // 否则返回编译错误
                            return std::make_pair(
                                    std::optional<Token>(),
                                    std::make_optional<CompilationError>(pos, ErrorCode::ErrIntegerOverflow));
                        }
                    }
                        //     解析成功则返回无符号整数类型的token，否则返回编译错误
                        // 如果读到的字符是数字，则存储读到的字符
                        // 如果读到的是字母，则存储读到的字符，并切换状态到标识符
                        // 如果读到的字符不是上述情况之一，则回退读到的字符，并解析已经读到的字符串为整数
                        //     解析成功则返回无符号整数类型的token，否则返回编译错误
                    else {
                        // 获取读到的字符的值，注意auto推导出的类型是char
                        auto ch = current_char.value();
                        if (ch == '.') {
                            ss << ch;
                            current_state = DFAState::FLOAT_STATE;
                        } else if (c0::isdigit(ch))  // 如果读到的字符是数字
                            ss << ch; // 存储读到的字符
                        else if (c0::isalpha(ch)) {
                            ss << ch; // 存储读到的字符
                            current_state = DFAState::IDENTIFIER_STATE; // 切换状态到标识符
                        } else {
                            unreadLast(); // 回退读到的字符
                            // 解析已经读到的字符串为整数
                            try {
                                // 解析成功则返回无符号整数类型的token
                                return std::make_pair(
                                        std::make_optional<Token>(TokenType::DECIMAL_LITERAL, std::stoi(ss.str()),
                                                                  pos,
                                                                  currentPos()),
                                        std::optional<CompilationError>());
                            } catch (...) {
                                // 否则返回编译错误
                                return std::make_pair(
                                        std::optional<Token>(),
                                        std::make_optional<CompilationError>(pos, ErrorCode::ErrIntegerOverflow));
                            }
                        }
                    }
                    break;
                }

                case ZERO: {
                    // 请填空：
                    // 如果当前已经读到了文件尾，则解析已经读到的字符串为整数
                    if (!current_char.has_value()) { // 当前已经读到了文件尾
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::DECIMAL_LITERAL, 0, pos,
                                                          currentPos()),
                                std::optional<CompilationError>());

                    } else {
                        // 获取读到的字符的值，注意auto推导出的类型是char
                        auto ch = current_char.value();
                        if (ch == '.') {
                            ss << ch;
                            current_state = DFAState::FLOAT_STATE;
                        } else if (ch == 'x' || ch == 'X') {
                            ss << ch;
                            current_state = DFAState::HEX_STATE;
                        } else if (c0::isdigit(ch)) // 如果读到的字符是数字，不能以0开头
                            return std::make_pair(
                                    std::optional<Token>(),
                                    std::make_optional<CompilationError>(pos, ErrorCode::ErrNumberStartWithZero));
                        else if (c0::isalpha(ch)) {
                            ss << ch; // 存储读到的字符
                            current_state = DFAState::IDENTIFIER_STATE; // 切换状态到标识符
                        } else {
                            unreadLast(); // 回退读到的字符
                            // 解析已经读到的字符串为整数
                            try {
                                // 解析成功则返回无符号整数类型的token
                                return std::make_pair(
                                        std::make_optional<Token>(TokenType::DECIMAL_LITERAL, std::stoi(ss.str()),
                                                                  pos,
                                                                  currentPos()),
                                        std::optional<CompilationError>());
                            } catch (...) {
                                // 否则返回编译错误
                                return std::make_pair(
                                        std::optional<Token>(),
                                        std::make_optional<CompilationError>(pos, ErrorCode::ErrIntegerOverflow));
                            }
                        }
                    }
                    break;
                }

                case HEX_STATE: {
                    if (!current_char.has_value()) {
                        if (ss.str().back() == 'x' || ss.str().back() == 'X') // need at least one hexadecimal-digit
                            return std::make_pair(std::optional<Token>(),
                                                  std::make_optional<CompilationError>(pos,
                                                                                       ErrorCode::ErrInvalidInput));
                        else
                            return std::make_pair(
                                    std::make_optional<Token>(TokenType::DECIMAL_LITERAL, std::stoi(ss.str(), nullptr, 16), pos,
                                                              currentPos()),
                                    std::optional<CompilationError>());
                    } else {
                        auto ch = current_char.value();
                        if (c0::isdigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))  // 如果读到的字符是数字
                            ss << ch; // 存储读到的字符
                        else if (c0::isalpha(ch)) {
                            ss << ch; // 存储读到的字符
                            current_state = DFAState::IDENTIFIER_STATE; // 切换状态到标识符
                        } else {
                            unreadLast(); // 回退读到的字符
                            if (ss.str().back() == 'x' || ss.str().back() == 'X') // need at least one hexadecimal-digit
                                return std::make_pair(std::optional<Token>(),
                                                      std::make_optional<CompilationError>(pos,
                                                                                           ErrorCode::ErrInvalidInput));
                            else
                                return std::make_pair(
                                        std::make_optional<Token>(TokenType::DECIMAL_LITERAL, std::stoi(ss.str(), nullptr, 16), pos,
                                                                  currentPos()),
                                        std::optional<CompilationError>());
                        }
                    }
                    break;
                }

                case FLOAT_STATE: { // already have '.'
                    if (!current_char.has_value()) {
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::FLOATING_LITERAL, ss.str(), pos,
                                                          currentPos()),
                                std::optional<CompilationError>());
                    } else {
                        auto ch = current_char.value();
                        if (ch == 'e' || ch == 'E') {
                            ss << ch;
                            current_state = DFAState::FLOAT_WITH_EXPONENT_STATE;
                        } else if (c0::isdigit(ch))
                            ss << ch;
                        else {
                            unreadLast(); // 回退读到的字符
                            return std::make_pair(
                                    std::make_optional<Token>(TokenType::FLOATING_LITERAL, ss.str(), pos,
                                                              currentPos()),
                                    std::optional<CompilationError>());
                        }
                    }
                    break;
                }

                case FLOAT_WITH_EXPONENT_STATE: { // already have 'e' or 'E', now parsing the optional sign
                    if (!current_char.has_value()) { // EOF after 'e'
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(pos, ErrorCode::ErrInvalidInput));
                    } else {
                        auto ch = current_char.value();
                        if (ch == '+' || ch == '-') {
                            ss << ch;
                            current_state = FLOAT_WITH_EXPONENT_AND_OPTIONAL_SIGN_STATE;
                        } else if (c0::isdigit(ch)) {
                            unreadLast();
                            current_state = FLOAT_WITH_EXPONENT_AND_OPTIONAL_SIGN_STATE;
                        } else // no +/-/digit after e
                            return std::make_pair(std::optional<Token>(),
                                                  std::make_optional<CompilationError>(pos,
                                                                                       ErrorCode::ErrInvalidInput));
                    }
                    break;
                }

                case FLOAT_WITH_EXPONENT_AND_OPTIONAL_SIGN_STATE: { // after optional sign, now we need at least one digit
                    if (!current_char.has_value()) { // EOF after optional sign
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(pos, ErrorCode::ErrInvalidInput));
                    } else {
                        auto ch = current_char.value();
                        if (c0::isdigit(ch)) {
                            ss << ch;
                            current_state = FLOAT_WITH_EXPONENT_OPTIONAL_SIGN_AND_DIGIT_SEQUENCE_STATE;
                        } else // no digit after optional sign
                            return std::make_pair(std::optional<Token>(),
                                                  std::make_optional<CompilationError>(pos,
                                                                                       ErrorCode::ErrInvalidInput));
                    }
                    break;
                }

                case FLOAT_WITH_EXPONENT_OPTIONAL_SIGN_AND_DIGIT_SEQUENCE_STATE: {
                    if (!current_char.has_value())
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::FLOATING_LITERAL, ss.str(), pos,
                                                          currentPos()),
                                std::optional<CompilationError>());
                    else {
                        auto ch = current_char.value();
                        if (c0::isdigit(ch))
                            ss << ch;
                        else {
                            unreadLast();
                            return std::make_pair(
                                    std::make_optional<Token>(TokenType::FLOATING_LITERAL, ss.str(), pos,
                                                              currentPos()),
                                    std::optional<CompilationError>());
                        }
                    }
                    break;
                }

                case CHAR_LITERAL_STATE: {
                    if (!current_char.has_value()) { // EOF after '
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(pos,
                                                                                   ErrorCode::ErrInvalidCharLiteral));
                    } else {
                        auto ch = current_char.value();
                        if (!c0::isprint(ch))
                            return std::make_pair(std::optional<Token>(),
                                                  std::make_optional<CompilationError>(pos,
                                                                                       ErrorCode::ErrInvalidCharLiteral));
                        if (!escape.empty()) { // in escape mode
                            if (escape.length() >= 2 && escape[1] == 'x') { // \x
                                if (c0::isxdigit(ch)) {
                                    escape += ch; // \xA
                                    if (escape.length() >= 4) { // store the result into ss
                                        std::string s = "0x" + escape.substr(2);
                                        unsigned int x = std::stoul(s, nullptr, 16);
                                        ss << (char) x;
                                        escape = ""; // exit escape mode
                                    }
                                } else // \xG
                                    return std::make_pair(std::optional<Token>(),
                                                          std::make_optional<CompilationError>(pos,
                                                                                               ErrorCode::ErrInvalidCharLiteral));
                            } else if (escape.back() == '\\' && ch == 'x')
                                escape += ch; // \x
                            else {
                                auto escaped = getEscaped(ch);
                                if (!escaped.has_value())
                                    return std::make_pair(std::optional<Token>(),
                                                          std::make_optional<CompilationError>(pos,
                                                                                               ErrorCode::ErrInvalidEscape));
                                else {
                                    escape = ""; // exit escape mode
                                    ss << escaped.value();
                                }
                            }
                        } else {
                            if (ch == '\\') // don't write to ss
                                escape += ch; // \ ,
                            else if (ch == '\'') {
                                ss << ch;
                                std::string str = ss.str();
                                if (str.length() != 3) // 有一个char
                                    return std::make_pair(std::optional<Token>(),
                                                          std::make_optional<CompilationError>(pos,
                                                                                               ErrorCode::ErrInvalidCharLiteral));
                                else
                                    return std::make_pair(
                                            std::make_optional<Token>(TokenType::CHAR_LITERAL, ss.str().substr(1, 1), pos,
                                                                      currentPos()),
                                            std::optional<CompilationError>()); // should not include '"', '\\'...
                            } else ss << ch;
                        }
                    }
                    break;
                }

                case STRING_LITERAL_STATE: {
                    if (!current_char.has_value()) { // EOF after "
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(pos,
                                                                                   ErrorCode::ErrInvalidStringLiteral));
                    } else {
                        auto ch = current_char.value();
                        if (!c0::isprint(ch))
                            return std::make_pair(std::optional<Token>(),
                                                  std::make_optional<CompilationError>(pos,
                                                                                       ErrorCode::ErrInvalidStringLiteral));
                        if (!escape.empty()) {
                            if (escape.length() >= 2 && escape[1] == 'x') { // \x
                                if (c0::isxdigit(ch)) {
                                    escape += ch; // \xA
                                    if (escape.length() >= 4) { // store the result into ss
                                        std::string s = "0x" + escape.substr(2);
                                        unsigned int x = std::stoul(s, nullptr, 16);
                                        ss << (char) x;
                                        escape = ""; // exit escape mode
                                    }
                                } else // \xG
                                    return std::make_pair(std::optional<Token>(),
                                                          std::make_optional<CompilationError>(pos,
                                                                                               ErrorCode::ErrInvalidCharLiteral));
                            } else if (escape.back() == '\\' && ch == 'x')
                                escape += ch; // \x
                            else {
                                auto escaped = getEscaped(ch);
                                if (!escaped.has_value())
                                    return std::make_pair(std::optional<Token>(),
                                                          std::make_optional<CompilationError>(pos,
                                                                                               ErrorCode::ErrInvalidEscape));
                                else {
                                    escape = ""; // exit escape mode
                                    ss << escaped.value();
                                }
                            }
                        } else {
                            if (ch == '\\') // don't write to ss
                                escape += ch;
                            else if (ch == '"') {
                                return std::make_pair(
                                        std::make_optional<Token>(TokenType::STRING_LITERAL, ss.str().substr(1), pos,
                                                                  currentPos()),
                                        std::optional<CompilationError>()); // should not include '"', '\\'...
                            } else ss << ch;
                        }
                    }
                    break;
                }


                case IDENTIFIER_STATE: {
                    // 请填空：
                    // 如果当前已经读到了文件尾，则解析已经读到的字符串
                    if (!current_char.has_value()) {
                        auto str = ss.str();
                        //     如果解析结果是关键字，那么返回对应关键字的token，否则返回标识符的token
                        return std::make_pair(
                                std::make_optional<Token>(getStringTokenType(str), str, pos, currentPos()),
                                std::optional<CompilationError>());
                    } else {
                        // 如果读到的是字符或字母，则存储读到的字符
                        auto ch = current_char.value();
                        if (c0::isalpha(ch) || c0::isdigit(ch))
                            ss << ch;
                        else {
                            // 如果读到的字符不是上述情况之一，则回退读到的字符，并解析已经读到的字符串
                            //     如果解析结果是关键字，那么返回对应关键字的token，否则返回标识符的token
                            unreadLast();
                            auto str = ss.str();
                            return std::make_pair(
                                    std::make_optional<Token>(getStringTokenType(str), str, pos, currentPos()),
                                    std::optional<CompilationError>());
                        }
                    }
                    break;
                }

                    // 如果当前状态是加号 TODO 跳转到数字
                case PLUS_SIGN_STATE: {
                    // 请思考这里为什么要回退，在其他地方会不会需要
                    // 因为break出去了，指向后面一位
                    unreadLast(); // Yes, we unread last char even if it's an EOF.
                    return std::make_pair(std::make_optional<Token>(TokenType::PLUS_SIGN, '+', pos, currentPos()),
                                          std::optional<CompilationError>());
                }
                    // 当前状态为减号的状态
                case MINUS_SIGN_STATE: {
                    // 请填空：回退，并返回减号token
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::MINUS_SIGN, '-', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case DIVISION_SIGN_STATE: {
                    // 请填空：回退，并返回减号token
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::DIVISION_SIGN, '/', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case MULTIPLICATION_SIGN_STATE: {
                    // 请填空：回退，并返回减号token
                    unreadLast();
                    return std::make_pair(
                            std::make_optional<Token>(TokenType::MULTIPLICATION_SIGN, '*', pos, currentPos()),
                            std::optional<CompilationError>());
                }

                case EQUAL_SIGN_STATE: {
                    if (current_char.value() && current_char.value() == '=') {
                        ss << current_char.value();
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::EQUAL_SIGN, ss.str(), pos, currentPos()),
                                std::optional<CompilationError>());
                    } else {
                        unreadLast();
                        return std::make_pair(std::make_optional<Token>(TokenType::ASSIGNMENT_OPERATOR, '=', pos, currentPos()),
                                              std::optional<CompilationError>());
                    }
                }

                case COLON_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::COLON, ':', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case SEMICOLON_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::SEMICOLON, ';', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case LEFTBRACKET_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::LEFT_BRACKET, '(', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case RIGHTBRACKET_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::RIGHT_BRACKET, ')', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case COMMA_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::COMMA, ',', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case LEFTBRACE_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::LEFT_BRACE, '{', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case RIGHTBRACE_STATE: {
                    unreadLast();
                    return std::make_pair(std::make_optional<Token>(TokenType::RIGHT_BRACE, '}', pos, currentPos()),
                                          std::optional<CompilationError>());
                }

                case LESS_SIGN_STATE: {
                    if (current_char.value() && current_char.value() == '=') {
                        ss << current_char.value();
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::LESS_OR_EQUAL_SIGN, ss.str(), pos, currentPos()),
                                std::optional<CompilationError>());
                    } else {
                        unreadLast();
                        return std::make_pair(std::make_optional<Token>(TokenType::LESS_SIGN, '<', pos, currentPos()),
                                              std::optional<CompilationError>());
                    }
                }

                case GREATER_SIGN_STATE: {
                    if (current_char.has_value() && current_char.value() == '=') {
                        ss << current_char.value();
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::GREATER_OR_EQUAL_SIGN, ss.str(), pos,
                                                          currentPos()),
                                std::optional<CompilationError>());
                    } else {
                        unreadLast();
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::GREATER_SIGN, '>', pos, currentPos()),
                                std::optional<CompilationError>());
                    }
                }

                case EXCLAMATION_SIGN_STATE: {
                    if (current_char.value() && current_char.value() == '=') {
                        ss << current_char.value();
                        return std::make_pair(
                                std::make_optional<Token>(TokenType::NOT_EQUAL_SIGN, ss.str(), pos, currentPos()),
                                std::optional<CompilationError>());
                    } else
                        return std::make_pair(std::optional<Token>(),
                                              std::make_optional<CompilationError>(pos, ErrorCode::ErrInvalidInput));
                }


                    // 请填空：
                    // 对于其他的合法状态，进行合适的操作
                    // 比如进行解析、返回token、返回编译错误

                    // 预料之外的状态，如果执行到了这里，说明程序异常
                default:
                    DieAndPrint("unhandled state.");
                    break;
            }
        }
        // 预料之外的状态，如果执行到了这里，说明程序异常
        return

                std::make_pair(std::optional<Token>(), std::optional<CompilationError>()

                );
    }

    std::optional<CompilationError> Tokenizer::checkToken(const Token &t) {
        switch (t.GetType()) {
            case IDENTIFIER: {
                auto val = t.GetValueString();
                if (c0::isdigit(val[0]))
                    return std::make_optional<CompilationError>(t.GetStartPos().first, t.GetStartPos().second,
                                                                ErrorCode::ErrInvalidIdentifier);
                break;
            }
            default:
                break;
        }
        return {};
    }

    std::string removeComments(std::string prgm) {
        int n = prgm.length();
        std::string res;

        // Flags to indicate that single line and multiple line comments
        // have started or not.
        bool s_cmt = false;
        bool m_cmt = false;


        // Traverse the given program
        for (int i = 0; i < n; i++) {
            // If single line comment flag is on, then check for end of it
            if (s_cmt == true && prgm[i] == '\n') {
                s_cmt = false;
                res += '\n';
            }
                // If multiple line comment is on, then check for end of it
            else if (m_cmt == true && prgm[i] == '*' && prgm[i + 1] == '/')
                m_cmt = false, i++;

                // If this character is in a comment, ignore it
            else if (s_cmt || m_cmt)
                continue;

                // Check for beginning of comments and set the approproate flags
            else if (prgm[i] == '/' && prgm[i + 1] == '/')
                s_cmt = true, i++;
            else if (prgm[i] == '/' && prgm[i + 1] == '*')
                m_cmt = true, i++;

                // If current character is a non-comment character, append it to res
            else res += prgm[i];
        }
        return res;
    }

    void Tokenizer::readAll() {
        if (_initialized)
            return;

        std::string content;
        for (std::string tp; std::getline(_rdr, tp);)
            content = content + tp + "\n";

        std::stringstream ss(removeComments(content));
        std::string to;
        while (std::getline(ss, to, '\n'))
            _lines_buffer.emplace_back(std::move(to + "\n"));

        _initialized = true;
        _ptr = std::make_pair<int64_t, int64_t>(0, 0);
        return;
    }

// Note: We allow this function to return a postion which is out of bound according to the design like std::vector::end().
    std::pair<uint64_t, uint64_t> Tokenizer::nextPos() {
        if (_ptr.first >= _lines_buffer.size())
            DieAndPrint("advance after EOF");
        if (_ptr.second == _lines_buffer[_ptr.first].size() - 1)
            return std::make_pair(_ptr.first + 1, 0);
        else
            return std::make_pair(_ptr.first, _ptr.second + 1);
    }

    std::pair<uint64_t, uint64_t> Tokenizer::currentPos() {
        return _ptr;
    }

    std::pair<uint64_t, uint64_t> Tokenizer::previousPos() {
        if (_ptr.first == 0 && _ptr.second == 0)
            DieAndPrint("previous position from beginning");
        if (_ptr.second == 0)
            return std::make_pair(_ptr.first - 1, _lines_buffer[_ptr.first - 1].size() - 1);
        else
            return std::make_pair(_ptr.first, _ptr.second - 1);
    }

    std::optional<char> Tokenizer::nextChar() {
        if (isEOF())
            return {}; // EOF
        auto result = _lines_buffer[_ptr.first][_ptr.second];
        _ptr = nextPos();
        return result;
    }

    bool Tokenizer::isEOF() {
        return _ptr.first >= _lines_buffer.size();
    }

// Note: Is it evil to unread a buffer?
    void Tokenizer::unreadLast() {
        _ptr = previousPos();
    }

}