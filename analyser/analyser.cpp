#include "analyser.h"

#include <climits>

namespace c0 {
    std::pair<std::vector<Instruction>, std::optional<CompilationError>> Analyser::Analyse() {
        auto err = analyseProgram();
        if (err.has_value())
            return std::make_pair(std::vector<Instruction>(), err);
        else
            return std::make_pair(_instructions, std::optional<CompilationError>());
    }

    // <程序> ::= 'begin'<主过程>'end'
    std::optional<CompilationError> Analyser::analyseProgram() {
        auto err = analyseVariableDeclaration();
        if (err.has_value())
            return err;
        err = analyseFunctionDefinition();
        if (err.has_value())
            return err;
        // 示例函数，示例如何调用子程序

//        // 'begin'
//        auto bg = nextToken();
//        if (!bg.has_value() || bg.value().GetType() != TokenType::BEGIN)
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBegin);
//
//        // <主过程>
//        auto err = analyseMain();
//        if (err.has_value())
//            return err;
//
//        // 'end'
//        auto ed = nextToken();
//        if (!ed.has_value() || ed.value().GetType() != TokenType::END)
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoEnd);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseVariableDeclaration() {
        //<variable-declaration> ::=
        //    [<const-qualifier>]<type-specifier><init-declarator-list>';'
        //<init-declarator-list> ::=
        //    <init-declarator>{','<init-declarator>}
        //<init-declarator> ::=
        //    <identifier>[<initializer>]
        //<initializer> ::=
        //    '='<expression>

        while (true) {
            auto isConst = false;
            auto next = nextToken();
            if (!next.has_value())
                return {};
            if (!next.has_value() || // type-specifier
                (next.value().GetType() != TokenType::CONST && next.value().GetType() != TokenType::VOID &&
                 next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE)) {
                unreadToken();
                return {}; // if there's no const and type-specifier, we suppose no variable is declared
            }

            if (next.value().GetType() == TokenType::CONST) { // optional const
                next = nextToken();
                isConst = true;
            }

            if (!next.has_value() || // type-specifier
                (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);

            while (true) { // init declarator list
                next = nextToken(); // at least one identifier
                if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
                // TODO check the identifier

                // optional initializer
                next = nextToken();
                if (next.has_value() && next.value().GetType() == TokenType::ASSIGN_SIGN) {
                    auto err = analyseExpression();
                    if (err.has_value())
                        return err;
                    // TODO declare an initialized variable
                }

                if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                    unreadToken();
                    break;
                }
            }
            // ';'
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
        }
    }

    std::optional<CompilationError> Analyser::analyseFunctionDefinition() {
        /*<function-definition> ::=
        <type-specifier><identifier><parameter-clause><compound-statement>

         <parameter-clause> ::=
                 '(' [<parameter-declaration-list>] ')'
                                                    <parameter-declaration-list> ::=
        <parameter-declaration>{','<parameter-declaration>}
         <parameter-declaration> ::=
        [<const-qualifier>]<type-specifier><identifier>

                            <function-call> ::=
        <identifier> '(' [<expression-list>] ')'
                                             <expression-list> ::=
        <expression>{','<expression>}*/
        auto next = nextToken();
        if (!next.has_value())
            return {};
        if (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT) {
            unreadToken();
            return {};
        }

        auto type = next.value().GetType();

        next = nextToken();
        if (!next.has_value())
            return {};
        if (next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

        // TODO check function

        auto err = analyseParameterClause();
        if (err.has_value())
            return err;

        err = analyseCompoundStatment();
        if (err.has_value())
            return err;
        return {};
    }

    std::optional<CompilationError> Analyser::analyseParameterClause() {
//        <parameter-clause> ::=
//            '(' [<parameter-declaration-list>] ')'
//        <parameter-declaration-list> ::=
//            <parameter-declaration>{','<parameter-declaration>}
//        <parameter-declaration> ::=
//            [<const-qualifier>]<type-specifier><identifier>

        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

        while (true) {
            auto isConst = false;
            next = nextToken();
            if (!next.has_value() ||
                (next.value().GetType() != TokenType::CONST && next.value().GetType() != TokenType::VOID &&
                 next.value().GetType() != TokenType::INT)) { // not legitimate start of a <parameter-declaration>
                unreadToken(); // )
                break;
            }

            if (next.value().GetType() == TokenType::CONST) {// optional <const-qualifier>
                next = nextToken();
                isConst = true;
            }

            if (!next.has_value() || // type-specifier
                (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);

            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

            // TODO do something with the parameter

            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                unreadToken();
                break;
            }
        }

        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCompoundStatment() {
        //<compound-statement> ::=
        //    '{' {<variable-declaration>} <statement-seq> '}'

        // {
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

        auto err = analyseVariableDeclaration();
        if (err.has_value())
            return err;

        err = analyseStatementSequence();
        if (err.has_value())
            return err;

        // }
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseStatementSequence() {
        while (true) {
            // 预读
            auto next = nextToken();
            if (!next.has_value())
                return {};
            unreadToken();

            if (next.value().GetType() != TokenType::LEFT_BRACE && // '{' <statement-seq> '}'
                next.value().GetType() != TokenType::IF && // <condition-statement>
                next.value().GetType() != TokenType::SWITCH && // <condition-statement>
                next.value().GetType() != TokenType::WHILE && // <loop-statement>
                next.value().GetType() != TokenType::DO && // <loop-statement>
                next.value().GetType() != TokenType::FOR && // <loop-statement>
                next.value().GetType() != TokenType::BREAK && // <jump-statement>
                next.value().GetType() != TokenType::CONTINUE && // <jump-statement>
                next.value().GetType() != TokenType::RETURN && // <jump-statement>
                next.value().GetType() != TokenType::PRINT &&
                next.value().GetType() != TokenType::SCAN &&
                next.value().GetType() != TokenType::IDENTIFIER &&
                // <assignment-expression>';' <function-call> ::= <identifier> '(' [<expression-list>] ')'
                next.value().GetType() != TokenType::SEMICOLON // ';'
                    )
                return {}; // empty

            std::optional<CompilationError> err;
            switch (next.value().GetType()) {
                // 这里需要你针对不同的预读结果来调用不同的子程序
                // 注意我们没有针对空语句单独声明一个函数，因此可以直接在这里返回
                case TokenType::LEFT_BRACE: { // '{' <statement-seq> '}'
                    err = analyseCompoundStatment();
                    if (err.has_value())
                        return err;
                    break;
                }
                case TokenType::IF:
                case TokenType::SWITCH: {
                    // TODO WIP <condition-statement>
                    break;
                }
                case TokenType::WHILE:
                case TokenType::DO:
                case TokenType::FOR: {
                    // TODO WIP <loop-statement>
                    break;
                }
                case TokenType::BREAK:
                case TokenType::CONTINUE:
                case TokenType::RETURN: {
                    // TODO WIP <jump-statement>
                    break;
                }
                case TokenType::PRINT: {
                    // TODO WIP <print-statement>
                    break;
                }
                case TokenType::SCAN: {
                    // TODO WIP <scan-statement>
                    break;
                }
                case TokenType::IDENTIFIER: {
                    // TODO WIP <assignment-expression>';' <function-call> ::= <identifier> '(' [<expression-list>] ')'
                    break;
                }
                case TokenType::SEMICOLON:
                    nextToken(); // 之前被unread了，再read一遍
                default:
                    break;
            }
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseStatement() { // single
        auto next = nextToken();
        if (!next.has_value()) // need at least one statement
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        unreadToken();

        if (next.value().GetType() != TokenType::LEFT_BRACE && // '{' <statement-seq> '}'
            next.value().GetType() != TokenType::IF && // <condition-statement>
            next.value().GetType() != TokenType::SWITCH && // <condition-statement>
            next.value().GetType() != TokenType::WHILE && // <loop-statement>
            next.value().GetType() != TokenType::DO && // <loop-statement>
            next.value().GetType() != TokenType::FOR && // <loop-statement>
            next.value().GetType() != TokenType::BREAK && // <jump-statement>
            next.value().GetType() != TokenType::CONTINUE && // <jump-statement>
            next.value().GetType() != TokenType::RETURN && // <jump-statement>
            next.value().GetType() != TokenType::PRINT &&
            next.value().GetType() != TokenType::SCAN &&
            next.value().GetType() != TokenType::IDENTIFIER &&
            // <assignment-expression>';' <function-call> ::= <identifier> '(' [<expression-list>] ')'
            next.value().GetType() != TokenType::SEMICOLON // ';'
                )
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement); // empty

        std::optional<CompilationError> err;
        switch (next.value().GetType()) {
            // 这里需要你针对不同的预读结果来调用不同的子程序
            // 注意我们没有针对空语句单独声明一个函数，因此可以直接在这里返回
            case TokenType::LEFT_BRACE: { // '{' <statement-seq> '}'
                err = analyseCompoundStatment();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::IF:
            case TokenType::SWITCH: {
                // TODO WIP <condition-statement>
                break;
            }
            case TokenType::WHILE:
            case TokenType::DO:
            case TokenType::FOR: {
                // TODO WIP <loop-statement>
                break;
            }
            case TokenType::BREAK:
            case TokenType::CONTINUE:
            case TokenType::RETURN: {
                // TODO WIP <jump-statement>
                break;
            }
            case TokenType::PRINT: {
                // TODO WIP <print-statement>
                break;
            }
            case TokenType::SCAN: {
                // TODO WIP <scan-statement>
                break;
            }
            case TokenType::IDENTIFIER: {
                // TODO WIP <assignment-expression>';' <function-call> ::= <identifier> '(' [<expression-list>] ')'
                break;
            }
            case TokenType::SEMICOLON:
                nextToken(); // 之前被unread了，再read一遍
            default:
                break;
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseConditionStatement() {
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::IF)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseLoopStatement() {
        return {};
    }

    std::optional<CompilationError> Analyser::analyseJumpStatement() {
        return {};
    }

    std::optional<CompilationError> Analyser::analysePrintStatement() {
        return {};
    }

    std::optional<CompilationError> Analyser::analyseScanStatement() {
        return {};
    }

    // <主过程> ::= <常量声明><变量声明><语句序列> DEPRECATED
    // 需要补全
    /*std::optional<CompilationError> Analyser::analyseMain() {
        // 完全可以参照 <程序> 编写

        // <常量声明>
        auto err = analyseConstantDeclaration();
        if (err.has_value())
            return err;

        // <变量声明>
        err = analyseVariableDeclaration();
        if (err.has_value())
            return err;

        // <语句序列>
        err = analyseStatementSequence();
        if (err.has_value())
            return err;
        return {};
    }*/

    // <常量声明> ::= {<常量声明语句>}
    // <常量声明语句> ::= 'const'<标识符>'='<常表达式>';'
    /*std::optional<CompilationError> Analyser::analyseConstantDeclaration() {
        // 示例函数，示例如何分析常量声明

        // 常量声明语句可能有 0 或无数个
        while (true) {
            // 预读一个 token，不然不知道是否应该用 <常量声明> 推导
            auto next = nextToken();
            if (!next.has_value())
                return {};
            // 如果是 const 那么说明应该推导 <常量声明> 否则直接返回
            if (next.value().GetType() != TokenType::CONST) {
                unreadToken();
                return {};
            }

            // <常量声明语句>
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
            if (isDeclared(next.value().GetValueString()))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
            addConstant(next.value());

            // '='
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::EQUAL_SIGN)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrConstantNeedValue);

            // <常表达式>
            int32_t val;
            auto err = analyseConstantExpression(val);
            if (err.has_value())
                return err;

            // ';'
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
            // 生成一次 LIT 指令加载常量
            _instructions.emplace_back(Operation::LIT, val);
        }
        return {};
    }*/

    // <变量声明> ::= {<变量声明语句>}
    // <变量声明语句> ::= 'var'<标识符>['='<表达式>]';'
    // 需要补全
    /*std::optional<CompilationError> Analyser::analyseVariableDeclaration() {
        // 变量声明语句可能有一个或者多个
        while (true) {
            // 预读？
            auto next = nextToken();
            if (!next.has_value())
                return {};
            // 'var'
            if (next.value().GetType() != TokenType::VAR) {
                unreadToken();
                return {};
            }
            // <标识符>
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
            //已经声明过 报错
            if (isDeclared(next.value().GetValueString()))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
            auto var = next.value();
            // 变量可能没有初始化，仍然需要一次预读
            next = nextToken();
            if (!next.has_value())
                return {};
            //初值0
            _instructions.emplace_back(Operation::LIT, 0);
            // '='
            if (next.value().GetType() != TokenType::EQUAL_SIGN) {
                unreadToken();
                addUninitializedVariable(var);
                // return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrConstantNeedValue);
            } else {
                // '<表达式>'
                auto err = analyseExpression();
                if (err.has_value())
                    return err;
                addVariable(var);
                //给变量赋值
                int32_t index = getIndex(var.GetValueString());
                _instructions.emplace_back(Operation::STO, index);
            }
            // ';'
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
        }
        return {};
    }*/
    // <常表达式> ::= [<符号>]<无符号整数>
    // 需要补全
    /*std::optional<CompilationError> Analyser::analyseConstantExpression(int32_t &out) {
        // out 是常表达式的结果
        // 这里你要分析常表达式并且计算结果
        // 注意以下均为常表达式
        // +1 -1 1
        // 同时要注意是否溢出

        auto next = nextToken();
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        auto prefix = "";
        if (next.value().GetType() == TokenType::PLUS_SIGN);
        else if (next.value().GetType() == TokenType::MINUS_SIGN)
            prefix = "-";
        else
            unreadToken();

        next = nextToken();
        if (next.has_value() && next.value().GetType() == TokenType::UNSIGNED_INTEGER) {
            try {
                out = std::stoi(prefix + next.value().GetValueString());
            } catch (...) {
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIntegerOverflow);
            }
        } else return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        return {};
    }*/

    // <表达式> ::= <项>{<加法型运算符><项>}
    std::optional<CompilationError> Analyser::analyseExpression() {
        // <项>
        auto err = analyseItem();
        if (err.has_value())
            return err;

        // {<加法型运算符><项>}
        while (true) {
            // 预读
            auto next = nextToken();
            if (!next.has_value())
                return {};
            auto type = next.value().GetType();
            if (type != TokenType::PLUS_SIGN && type != TokenType::MINUS_SIGN) {
                unreadToken();
                return {};
            }

            // <项>
            err = analyseItem();
            if (err.has_value())
                return err;

            // 根据结果生成指令
            if (type == TokenType::PLUS_SIGN)
                _instructions.emplace_back(Operation::ADD, 0);
            else if (type == TokenType::MINUS_SIGN)
                _instructions.emplace_back(Operation::SUB, 0);
        }
        return {};
    }

    // <赋值语句> ::= <标识符>'='<表达式>';'
    // 需要补全
    std::optional<CompilationError> Analyser::analyseAssignmentStatement() {
        // 这里除了语法分析以外还要留意
        // 标识符声明过吗？
        // 标识符是常量吗？
        // 需要生成指令吗？
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
        auto str = next.value().GetValueString();
//        if (isDeclared(next.value().GetValueString()))
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
        if (!isDeclared(str))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
        if (isConstant(str))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrAssignToConstant);
        if (isUninitializedVariable(str)) {
            _vars[str] = _uninitialized_vars[str];
            _uninitialized_vars.erase(str);
        }

        next = nextToken(); // '='
        if (!next.has_value() || next.value().GetType() != TokenType::EQUAL_SIGN)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidAssignment);

        auto err = analyseExpression();
        if (err.has_value())
            return err;
        // ';'
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
        _instructions.emplace_back(Operation::STO, getIndex(str));
        return {};
    }

    // <输出语句> ::= 'print' '(' <表达式> ')' ';'
    std::optional<CompilationError> Analyser::analyseOutputStatement() {
        // 如果之前 <语句序列> 的实现正确，这里第一个 next 一定是 TokenType::PRINT
        auto next = nextToken(); // 忽略PRINT

        // '('
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidPrint);

        // <表达式>
        auto err = analyseExpression();
        if (err.has_value())
            return err;

        // ')'
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidPrint);

        // ';'
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);

        // 生成相应的指令 WRT
        _instructions.emplace_back(Operation::WRT, 0);
        return {};
    }

    // <项> :: = <因子>{ <乘法型运算符><因子> }
    // 需要补全
    std::optional<CompilationError> Analyser::analyseItem() {
        // 可以参考 <表达式> 实现
        auto err = analyseFactor();
        if (err.has_value())
            return err;
        while (true) {
            auto next = nextToken();
            if (!next.has_value())
                return {};
            auto type = next.value().GetType();
            if (type != TokenType::MULTIPLICATION_SIGN && type != TokenType::DIVISION_SIGN) {
                unreadToken();
                return {};
            }
            err = analyseFactor();
            if (err.has_value())
                return err;

            // 根据结果生成指令
            if (type == TokenType::MULTIPLICATION_SIGN)
                _instructions.emplace_back(Operation::MUL, 0);
            else if (type == TokenType::DIVISION_SIGN)
                _instructions.emplace_back(Operation::DIV, 0);
        }
        return {};
    }

    // <因子> ::= [<符号>]( <标识符> | <无符号整数> | '('<表达式>')' )
    // 需要补全
    std::optional<CompilationError> Analyser::analyseFactor() {
        // [<符号>]
        auto next = nextToken();
        auto prefix = 1;
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        if (next.value().GetType() == TokenType::PLUS_SIGN)
            prefix = 1;
        else if (next.value().GetType() == TokenType::MINUS_SIGN) {
            prefix = -1;
            _instructions.emplace_back(Operation::LIT, 0);
        } else
            unreadToken();

        // 预读
        next = nextToken();
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        switch (next.value().GetType()) {
            // 这里和 <语句序列> 类似，需要根据预读结果调用不同的子程序
            // 但是要注意 default 返回的是一个编译错误
            case TokenType::IDENTIFIER: {
                if (!isDeclared(next.value().GetValueString()))
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
                if (isUninitializedVariable(next.value().GetValueString()))
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotInitialized);
                _instructions.emplace_back(Operation::LOD, getIndex(next.value().GetValueString()));
                break;
            }
            case TokenType::UNSIGNED_INTEGER: {
                _instructions.emplace_back(Operation::LIT, std::stoi(next.value().GetValueString()));
                break;
            }
            case TokenType::LEFT_BRACKET: {
                auto err = analyseExpression();
                if (err.has_value())
                    return err;
                next = nextToken();
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
                break;
            }
            default:
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        }
        if (prefix == -1)
            _instructions.emplace_back(Operation::SUB, 0);
        return {};
    }

    std::optional<Token> Analyser::nextToken() {
        if (_offset == _tokens.size())
            return {};
        // 考虑到 _tokens[0..._offset-1] 已经被分析过了
        // 所以我们选择 _tokens[0..._offset-1] 的 EndPos 作为当前位置
        _current_pos = _tokens[_offset].GetEndPos();
        return _tokens[_offset++];
    }

    void Analyser::unreadToken() {
        if (_offset == 0)
            DieAndPrint("analyser unreads token from the begining.");
        _current_pos = _tokens[_offset - 1].GetEndPos();
        _offset--;
    }

    void Analyser::_add(const Token &tk, std::map<std::string, int32_t> &mp) {
        if (tk.GetType() != TokenType::IDENTIFIER)
            DieAndPrint("only identifier can be added to the table.");
        mp[tk.GetValueString()] = _nextTokenIndex;
        _nextTokenIndex++;
    }

    void Analyser::addVariable(const Token &tk) {
        _add(tk, _vars);
    }

    void Analyser::addConstant(const Token &tk) {
        _add(tk, _consts);
    }

    void Analyser::addUninitializedVariable(const Token &tk) {
        _add(tk, _uninitialized_vars);
    }

    int32_t Analyser::getIndex(const std::string &s) {
        if (_uninitialized_vars.find(s) != _uninitialized_vars.end())
            return _uninitialized_vars[s];
        else if (_vars.find(s) != _vars.end())
            return _vars[s];
        else
            return _consts[s];
    }

    bool Analyser::isDeclared(const std::string &s) {
        return isConstant(s) || isUninitializedVariable(s) || isInitializedVariable(s);
    }

    bool Analyser::isUninitializedVariable(const std::string &s) {
        return _uninitialized_vars.find(s) != _uninitialized_vars.end();
    }

    bool Analyser::isInitializedVariable(const std::string &s) {
        return _vars.find(s) != _vars.end();
    }

    bool Analyser::isConstant(const std::string &s) {
        return _consts.find(s) != _consts.end();
    }
}