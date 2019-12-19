#include "analyser.h"

#include <climits>

namespace c0 {
    bool isMainDefined;

    std::tuple<std::vector<Instruction>, std::vector<GlobalConstant>, std::vector<GlobalFunction>, std::optional<CompilationError>> Analyser::Analyse() {
        isMainDefined = false;
        addInstruction(NOP, 0, 0);
        auto err = analyseProgram();
        if (err.has_value())
            // return std::make_pair(std::vector<Instruction>(), err);
            return {std::vector<Instruction>(), std::vector<GlobalConstant>(), std::vector<GlobalFunction>(), err};
        else
            return {_instructions, table._global_constants, table._global_functions, std::optional<CompilationError>()};
            // return std::make_pair(_instructions, std::optional<CompilationError>());
    }

    std::optional<CompilationError> Analyser::analyseProgram() {
        //<C0-program> ::=
        //    {<variable-declaration>}{<function-definition>}
        //<variable-declaration> ::=
        //    [<const-qualifier>]<type-specifier><identifier>['='<expression>]{',' ... }';'
        //<function-definition> ::=
        //    <type-specifier><identifier>'(' [[<const-qualifier>]<type-specifier><identifier>{',' ... }] ')'<compound-statement>
        auto err = analyseMultipleVariableDeclaration();
        if (err.has_value())
            return err;

        while (true) {
            auto next = nextToken();
            if (!next.has_value())
                break;
            unreadToken();
            if (next.value().GetType() == TokenType::VOID || next.value().GetType() == TokenType::INT ||
                next.value().GetType() == TokenType::CHAR || next.value().GetType() == TokenType::DOUBLE) {
                err = analyseFunctionDefinition();
                if (err.has_value())
                    return err;
            } else break;
            // return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);
        }
        if (!isMainDefined)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoMain);
        auto next = nextToken();
        if (next.has_value())
            return std::make_optional<CompilationError>(_current_pos,
                                                        ErrorCode::ErrSurplusTokenAfterFunctionDefinition);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseMultipleVariableDeclaration() {
        while (true) {
            auto finish = false;
            auto next = nextToken();
            if (!next.has_value())
                break;
            if (next.value().GetType() == TokenType::CONST) {
                unreadToken();
                auto err = analyseVariableDeclaration();
                if (err.has_value())
                    return err;
            } else if (next.value().GetType() == TokenType::VOID || next.value().GetType() == TokenType::INT ||
                       next.value().GetType() == TokenType::CHAR || next.value().GetType() == TokenType::DOUBLE) {
                next = nextToken(); // identifier
                if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
                next = nextToken(); // (, =, ',', ;
                unreadToken();
                if (!next.has_value())
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                switch (next.value().GetType()) {
                    case TokenType::LEFT_BRACKET:
                        unreadToken(); // identifier
                        unreadToken(); // type specifier
                        finish = true;
                        break;
                    case TokenType::ASSIGNMENT_OPERATOR:
                    case TokenType::COMMA:
                    case TokenType::SEMICOLON: {
                        unreadToken(); // identifier
                        unreadToken(); // type specifier
                        auto err = analyseVariableDeclaration();
                        if (err.has_value())
                            return err;
                        break;
                    }
                    default:
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                }
            } else {
                unreadToken();
                break;
            }// return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);
            if (finish)
                break;
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseVariableDeclaration() {
        //<variable-declaration> ::=
        //    [<const-qualifier>]<type-specifier><identifier>['='<expression>]{',' ... }';'
        auto isConst = false;
        // TokenType type;
        ValueType type;

        auto next = nextToken();
        if (!next.has_value() || // type-specifier
            (next.value().GetType() != TokenType::CONST && next.value().GetType() != TokenType::VOID &&
             next.value().GetType() != TokenType::INT &&
             next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);

        if (next.value().GetType() == TokenType::CONST) { // optional <const-qualifier>
            isConst = true;
            next = nextToken();
            if (!next.has_value() || // <type-specifier> after const is not checked
                (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);
        }

        type = tokenTypeToValueType(next.value().GetType());

        while (true) { // <init-declarator-list>
            next = nextToken(); // at least one <init-declarator>, <init-declarator> ::= <identifier>[<initializer>]
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

            auto name = next.value().GetValueString();
            if (table.IsDuplicate(name))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
            // optional <initializer>, <initializer> ::= '='<expression>
            next = nextToken();
            if (next.has_value() && next.value().GetType() == TokenType::ASSIGNMENT_OPERATOR) {
                // transInsert(name, type, isConst)
                table.UpdateSymbol(name, 0, isConst ? CONSTANT : VARIABLE, type);
                auto entry = table.FindSymbol(name);
                // auto level = entry.value().GetLevel();
                // auto offset = getOffset(entry.value());

                auto err = analyseExpression();
                if (err.has_value())
                    return err;
            } else { // not initialized
                unreadToken();
                if (isConst)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrConstantNeedValue);
                addInstruction(IPUSH, 0, 0);
                table.UpdateSymbol(name, 0, VARIABLE, type);
            }

            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                unreadToken();
                break;
            }
        }
        // ';'
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseFunctionDefinition() {
        //<function-definition> ::=
        //<type-specifier><identifier>'(' [[<const-qualifier>]<type-specifier><identifier>{',' ... }] ')'<compound-statement>
        //<function-call> ::=
        //<identifier> '(' [<expression-list>] ')'
        //                                     <expression-list> ::=
        //<expression>{','<expression>}
        _code_offset = 0;
        auto next = nextToken(); // <type-specifier>
        if (!next.has_value() ||
            (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
             next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);

        // auto type = next.value().GetType();
        ValueType type = tokenTypeToValueType(next.value().GetType());

        next = nextToken(); // <identifier>
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

        auto name = next.value().GetValueString();
        isMainDefined = name == "main";
        table.AddGlobalConstant(name, name, STRING_TYPE);
        if (table.IsDuplicate(name))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
        table.NextLevel();

        //<parameter-clause> ::=
        //    '(' [<parameter-declaration-list>] ')'
        //<parameter-declaration-list> ::=
        //    <parameter-declaration>{','<parameter-declaration>}
        //<parameter-declaration> ::=
        //    [<const-qualifier>]<type-specifier><identifier>

        next = nextToken(); // (
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

        std::vector<Arg> args;
        while (true) { // [[<const-qualifier>]<type-specifier><identifier>{','[<const-qualifier>]<type-specifier><identifier>}]
            auto isConst = false;
            next = nextToken();
            if (!next.has_value() ||
                (next.value().GetType() != TokenType::CONST && next.value().GetType() != TokenType::VOID &&
                 next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() !=
                                                              TokenType::DOUBLE)) { // not legitimate start of a <parameter-declaration>
                unreadToken(); // )
                break;
            }

            if (next.value().GetType() == TokenType::CONST) { // optional <const-qualifier>
                isConst = true;
                next = nextToken();
                if (!next.has_value() || // <type-specifier> after const is not checked
                    (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
                     next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);
            }

            // now next must be a <type-specifier>
            type = tokenTypeToValueType(next.value().GetType());

            next = nextToken(); // <identifier>
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

            auto val = next.value().GetValueString();
            if (table.IsDuplicate(val))
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
            args.emplace_back(isConst, val, type);
            table.UpdateSymbol(val, 0, isConst ? CONSTANT_ARGUMENT : ARGUMENT, type);

            next = nextToken(); // ，
            if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                unreadToken(); // finish
                break;
            }
        }

        next = nextToken(); // )
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

//        auto err = analyseCompoundStatement(); maybe need this
//        if (err.has_value())
//            return err;
        next = nextToken(); // {
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

        auto err = analyseMultipleVariableDeclaration();
        if (err.has_value())
            return err;

        table.AddGlobalFunction(name, args.size(), table.GetCurrentLevel(), args, 0, type);
        _context = table.GetGlobalFunction(name).value();

        err = analyseStatementSequence();
        if (err.has_value())
            return err;

        next = nextToken(); // }
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

        err = table.PreviousLevel();
        if (err.has_value())
            return err;
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCompoundStatement() {
        //<compound-statement> ::=
        //    '{' {<variable-declaration>} <statement-seq> '}'

        auto next = nextToken(); // {
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

        auto err = analyseMultipleVariableDeclaration();
        if (err.has_value())
            return err;

        err = analyseStatementSequence();
        if (err.has_value())
            return err;

        next = nextToken(); // }
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseStatementSequence() {
        //<statement-seq> ::=
        //	{<statement>}
        while (true) {
            auto next = nextToken();
            unreadToken();

            if (!next.has_value() || (next.value().GetType() != TokenType::LEFT_BRACE && // '{' <statement-seq> '}'
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
            ))
                break;

            auto err = analyseStatement();
            if (err.has_value())
                return err;
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseStatement() {
        //<statement> ::=
        //     <compound-statement>
        //    |<condition-statement>
        //    |<loop-statement>
        //    |<jump-statement>
        //    |<print-statement>
        //    |<scan-statement>
        //    |<assignment-expression>';'
        //    |<function-call>';'
        //    |';'
        auto next = nextToken();
        unreadToken();

        if (!next.has_value() || (next.value().GetType() != TokenType::LEFT_BRACE && // '{' <statement-seq> '}'
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
        ))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement); // empty

        std::optional<CompilationError> err;
        switch (next.value().GetType()) {
            case TokenType::LEFT_BRACE: { // <compound-statement>
                table.NextLevel();

                next = nextToken(); // {
                if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

                err = analyseCompoundStatement();
                if (err.has_value())
                    return err;

                next = nextToken(); // }
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACE)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

                for (int i = 0; i < table.GetNextOffset(); i++)
                    addInstruction(POP, 0, 0);

                err = table.PreviousLevel();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::IF:
            case TokenType::SWITCH: {
                err = analyseConditionStatement();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::WHILE:
            case TokenType::DO:
            case TokenType::FOR: {
                err = analyseLoopStatement();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::BREAK:
            case TokenType::CONTINUE:
            case TokenType::RETURN: {
                err = analyseJumpStatement();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::PRINT: {
                err = analysePrintStatement();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::SCAN: {
                err = analyseScanStatement();
                if (err.has_value())
                    return err;
                break;
            }
            case TokenType::IDENTIFIER: {
                // <assignment-expression>';' <function-call> ::= <identifier> '(' [<expression-list>] ')'
                //<assignment-expression> ::=
                //    <identifier><assignment-operator><expression>
                nextToken(); // the identifier
                next = nextToken(); // = or (
                unreadToken();
                unreadToken();
                if (!next.has_value())
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                switch (next.value().GetType()) {
                    case TokenType::ASSIGNMENT_OPERATOR: {
                        err = analyseAssignmentExpression();
                        if (err.has_value())
                            return err;
                        break;
                    }
                    case TokenType::LEFT_BRACKET: {
                        err = analyseFunctionCall();
                        if (err.has_value())
                            return err;
                        break;
                    }
                    default:
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                }
                next = nextToken(); // ;
                if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                break;
            }
            case TokenType::SEMICOLON:
                nextToken();
            default:
                break;
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCondition(int32_t &falseJumpAddress) {
        // <condition> ::= <expression>[<relational-operator><expression>]
        // <relational-operator>     ::= '<' | '<=' | '>' | '>=' | '!=' | '=='
        auto err = analyseExpression();
        if (err.has_value())
            return err;

        auto next = nextToken(); // <relational-operator>
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

        // auto falseJpc = -1;
        Operation type;
        switch (next.value().GetType()) {
            case LESS_SIGN: // <
                type = JL;
                break;
            case LESS_OR_EQUAL_SIGN: // <=
                type = JLE;
                break;
            case GREATER_SIGN: // >
                type = JG;
                break;
            case GREATER_OR_EQUAL_SIGN: // >=
                type = JGE;
                break;
            case EQUAL_SIGN:
                type = JE;
                break;
            case NOT_EQUAL_SIGN:
                type = JNE;
                break;
            default:
                unreadToken();
                addInstruction(ICMP, 0, 0);
                falseJumpAddress = addInstruction(JNE, 0, 0) + 1;
                return {};
        }
        err = analyseExpression();
        if (err.has_value())
            return err;
        addInstruction(ICMP, 0, 0);
        falseJumpAddress = addInstruction(type, 0, 0);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseConditionStatement() {
        /*<condition-statement> ::=
                 'if' '(' <condition> ')' <statement> ['else' <statement>]
        |'switch' '(' <expression> ')' '{' {<labeled-statement>} '}'*/
        auto next = nextToken(); // 'IF'
        if (!next.has_value() ||
            (next.value().GetType() != TokenType::IF && next.value().GetType() != TokenType::SWITCH))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

        if (next.value().GetType() == TokenType::IF) {
            next = nextToken(); // (
            if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

            int32_t falseJumpAddress = -1;
            auto err = analyseCondition(falseJumpAddress);
            if (err.has_value())
                return err;

            next = nextToken(); // )
            if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

            err = analyseStatement(); // statement
            if (err.has_value())
                return err;

            int32_t trueJumpAddress = _code_offset + 1;
            int32_t noCondition = addInstruction(JMP, 0, 0);
            int32_t finalJumpAddress = -1;

            next = nextToken(); // optional else
            if (next.has_value() && next.value().GetType() == TokenType::ELSE) {
                err = analyseStatement(); // statement
                if (err.has_value())
                    return err;
                finalJumpAddress = _code_offset;
            } else unreadToken();
            updateInstruction(noCondition, finalJumpAddress, 0);
            updateInstruction(falseJumpAddress, trueJumpAddress, 0);

        } else { // switch
            next = nextToken(); // (
            if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

            auto err = analyseExpression();
            if (err.has_value())
                return err;

            next = nextToken(); // )
            if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

            next = nextToken(); // {
            if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

            while (true) { // {<labeled-statement>}
                // <labeled-statement> ::= 'case' (<integer-literal>|<char-literal>) ':' <statement> |'default' ':' <statement>
                next = nextToken(); // case or default
                if (!next.has_value() || (next.value().GetType() != TokenType::CASE &&
                                          next.value().GetType() != TokenType::DEFAULT)) // finished
                    break;
                if (next.value().GetType() == TokenType::CASE) {
                    next = nextToken(); // (<integer-literal>|<char-literal>)
                    if (!next.has_value() || (next.value().GetType() != TokenType::DECIMAL_LITERAL &&
                                              next.value().GetType() != TokenType::CHAR_LITERAL))
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                    next = nextToken(); // :
                    if (!next.has_value() || next.value().GetType() != TokenType::COLON)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                    err = analyseStatement();
                    if (err.has_value())
                        return err;
                } else {
                    next = nextToken(); // :
                    if (!next.has_value() || next.value().GetType() != TokenType::COLON)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                    err = analyseStatement();
                    if (err.has_value())
                        return err;
                }
            }

            next = nextToken(); // }
            if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACE)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseLoopStatement() {
        /*
        <loop-statement> ::=
            'while' '(' <condition> ')' <statement>
           |'do' <statement> 'while' '(' <condition> ')' ';'
           |'for' '('<for-init-statement> [<condition>]';' [<for-update-expression>]')' <statement>
        */
        auto next = nextToken();
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        switch (next.value().GetType()) {
            case TokenType::WHILE: {
                next = nextToken(); // (
                if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

                auto startAddress = _code_offset;
                int32_t falseJumpAddress = -1;
                auto err = analyseCondition(falseJumpAddress);
                if (err.has_value())
                    return err;

                next = nextToken(); // )
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

                err = analyseStatement();
                if (err.has_value())
                    return err;
                addInstruction(JMP, startAddress, 0);
                auto finishAddress = _code_offset;
                updateInstruction(falseJumpAddress, finishAddress, 0);
                break;
            }
            case TokenType::DO: {
                // 'do' <statement> 'while' '(' <condition> ')' ';'
                auto err = analyseStatement();
                if (err.has_value())
                    return err;
                next = nextToken(); // while
                if (!next.has_value() || next.value().GetType() != TokenType::WHILE)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                next = nextToken(); // (
                if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                int32_t falseJumpAddress = -1;
                err = analyseCondition(falseJumpAddress);
                if (err.has_value())
                    return err;
                next = nextToken(); // )
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                next = nextToken(); // ;
                if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                break;
            }
            case TokenType::FOR: {
                next = nextToken(); // (
                if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                /*'for' '('<for-init-statement> [<condition>]';' [<for-update-expression>]')' <statement>
                <for-init-statement> ::=
                    [<assignment-expression>{','<assignment-expression>}]';'
                <for-update-expression> ::=
                    (<assignment-expression>|<function-call>){','(<assignment-expression>|<function-call>)}
                */
                next = nextToken(); // <for-init-statement>
                if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON) {
                    unreadToken();
                    while (true) {
                        auto err = analyseAssignmentExpression();
                        if (err.has_value())
                            return err;
                        next = nextToken();
                        if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {// ','
                            unreadToken();
                            break; // finish
                        }
                    }
                    next = nextToken(); // ;
                    if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                }

                next = nextToken(); // [<condition>]
                if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON) {
                    unreadToken();
                    int32_t falseJumpAddress = -1;
                    auto err = analyseCondition(falseJumpAddress);
                    if (err.has_value())
                        return err;
                    next = nextToken(); // ;
                    if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                }

                next = nextToken(); // [<for-update-expression>]')'
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET) {
                    // (<assignment-expression>|<function-call>){','(<assignment-expression>|<function-call>)}
                    // <assignment-expression> ::= <identifier><assignment-operator><expression>
                    // <function-call> ::= <identifier> '(' [<expression-list>] ')'
                    // <expression-list> ::=
                    //    <expression>{','<expression>}
                    while (true) {
                        if (!next.has_value() && next.value().GetType() != TokenType::IDENTIFIER)
                            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

                        next = nextToken();
                        if (!next.has_value())
                            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                        else if (next.value().GetType() ==
                                 TokenType::ASSIGNMENT_OPERATOR) { // <assignment-expression> ::= <identifier><assignment-operator><expression>
                            unreadToken();
                            unreadToken();
                            auto err = analyseExpression();
                            if (err.has_value())
                                return err;
                        } else if (next.value().GetType() ==
                                   TokenType::LEFT_BRACKET) { // <function-call> ::= <identifier> '(' [<expression-list>] ')'
                            unreadToken();
                            unreadToken();
                            auto err = analyseFunctionCall();
                            if (err.has_value())
                                return err;
                        }

                        next = nextToken();
                        if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                            unreadToken();
                            break;
                        }
                    }
                    next = nextToken(); // )
                    if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                }
                auto err = analyseStatement();
                if (err.has_value())
                    return err;
                break;
            }
            default:
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseJumpStatement() {
        // <jump-statement> ::=
        //     'break' ';'
        //    |'continue' ';'
        //    |<return-statement>
        // <return-statement> ::= 'return' [<expression>] ';'
        auto next = nextToken();
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        switch (next.value().GetType()) {
            case TokenType::BREAK:
            case TokenType::CONTINUE: {
                next = nextToken(); // ;
                if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                break;
            }
            case TokenType::RETURN: {
                next = nextToken(); // ;
                if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON) {
                    unreadToken();
                    auto err = analyseExpression();
                    if (err.has_value())
                        return err;
                    if (_context.GetReturnType() == VOID_TYPE)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoReturnValue);
                    addInstruction(IRET, 0, 0);
                    next = nextToken(); // ;
                    if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
                } else {
                    if (_context.GetReturnType() == VOID_TYPE)
                        addInstruction(RET, 0, 0);
                    else
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoReturnValue);
                }
                break;
            }
            default:
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analysePrintStatement() {
        // <print-statement> ::=
        //     'print' '(' [<printable-list>] ')' ';'
        // <printable-list>  ::=
        //     <printable> {',' <printable>}
        // <printable> ::=
        //     <expression> | <string-literal>
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::PRINT)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        next = nextToken(); // (
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

        next = nextToken(); // )
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET) {
            unreadToken();
            // <printable-list>  ::=
            //     <printable> {',' <printable>}
            // <printable> ::=
            //     <expression> | <string-literal>

            // printable
            while (true) {
                next = nextToken();
                unreadToken();
                if (!next.has_value())
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                if (next.value().GetType() == TokenType::STRING_LITERAL) {
                    // do something
                    auto val = next.value().GetValueString();
                    auto global = table.GetGlobalConstant(val);
                    if (!global.has_value()) { // remove "" from string literal
                        val = val.substr(1, val.length() - 1);
                        table.AddGlobalConstant(val, val, STRING_TYPE);
                        global = table.GetGlobalConstant(val);
                    }
                    addInstruction(LOADC, global.value().GetIndex(), 0);
                    addInstruction(SPRINT, 0, 0);
                    next = nextToken();
                } else if (next.value().GetType() == TokenType::CHAR_LITERAL) {
                    addInstruction(IPUSH, (int) next.value().GetValueString()[1],
                                   0); // remove '' from char literal
                    addInstruction(CPRINT, 0, 0);
                } else { // must be expression
                    auto err = analyseExpression();
                    if (err.has_value())
                        return err;
                    addInstruction(IPRINT, 0, 0);
                }
                next = nextToken(); // ,
                if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                    unreadToken();
                    break;
                }
            }
            next = nextToken(); // )
            if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
        }

        next = nextToken(); // ;
        if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);

        return {};
    }

    std::optional<CompilationError> Analyser::analyseScanStatement() {
        // <scan-statement> ::= 'scan' '(' <identifier> ')' ';'
        auto next = nextToken(); // scan
        if (!next.has_value() || next.value().GetType() != TokenType::SCAN)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
        next = nextToken(); // (
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
        next = nextToken(); // identifier
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

        // auto type = next.value().GetType();
        auto entry = table.FindSymbol(next.value().GetValueString());
        if (!entry.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
        else if (entry.value().GetType() == CONSTANT || entry.value().GetType() == CONSTANT_ARGUMENT ||
                 entry.value().GetType() == FUNCTION)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrAssignToConstant);

        next = nextToken(); // )
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
        next = nextToken(); // ;
        if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);

        auto offset = getOffset(entry.value());
        auto level = entry.value().GetLevel();
        addInstruction(LOADA, level != 0, offset);
        if (entry.value().GetValueType() == CHAR_TYPE)
            addInstruction(CSCAN, 0, 0);
        else if (entry.value().GetValueType() == INTEGER_TYPE)
            addInstruction(ISCAN, 0, 0);
        addInstruction(ISTORE, 0, 0);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseExpression() {
        //<expression> ::=
        //    <additive-expression>
        //<additive-expression> ::=
        //     <multiplicative-expression>{<additive-operator><multiplicative-expression>}
        auto err = analyseMultiplicativeExpression();
        if (err.has_value())
            return err;
        while (true) {
            auto next = nextToken();
            if (!next.has_value() || (next.value().GetType() != TokenType::PLUS_SIGN &&
                                      next.value().GetType() != TokenType::MINUS_SIGN)) {
                unreadToken();
                break;
            }
            Operation type;
            if (next.value().GetType() == TokenType::PLUS_SIGN)
                type = IADD;
            else if (next.value().GetType() == TokenType::MINUS_SIGN)
                type = ISUB;
            else return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
            err = analyseMultiplicativeExpression();
            if (err.has_value())
                return err;
            addInstruction(type, 0, 0);
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseMultiplicativeExpression() {
        //<multiplicative-expression> ::=
        //     <cast-expression>{<multiplicative-operator><cast-expression>}
        auto err = analyseCastExpression();
        if (err.has_value())
            return err;
        while (true) {
            auto next = nextToken();
            if (!next.has_value() || (next.value().GetType() != TokenType::MULTIPLICATION_SIGN &&
                                      next.value().GetType() != TokenType::DIVISION_SIGN)) {
                unreadToken();
                break;
            }
            Operation type;
            if (next.value().GetType() == TokenType::MULTIPLICATION_SIGN)
                type = IMUL;
            else if (next.value().GetType() == TokenType::DIVISION_SIGN)
                type = IDIV;
            else return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
            auto err = analyseCastExpression();
            if (err.has_value())
                return err;
            addInstruction(type, 0, 0);
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCastExpression() {
        //<cast-expression> ::=
        //    {'('<type-specifier>')'}<unary-expression>
        while (true) {
            auto finish = false;
            auto next = nextToken();
            if (next.has_value() && next.value().GetType() == TokenType::LEFT_BRACKET) {
                next = nextToken();
                if (!next.has_value())
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
                switch (next.value().GetType()) {
                    case TokenType::VOID:
                    case TokenType::INT:
                    case TokenType::CHAR:
                    case TokenType::DOUBLE: // TODO do something here
                        next = nextToken();
                        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
                        break;
                    default: { // may be '('<expression>')'
                        unreadToken();
                        unreadToken();
                        finish = true;
                        break;
                    }
                }
            } else {
                unreadToken();
                break;
            }
            if (finish) break;
        }
        auto err = analyseUnaryExpression();
        if (err.has_value())
            return err;
        return {};
    }

    std::optional<CompilationError> Analyser::analyseUnaryExpression() {
        //<unary-expression> ::=
        //    [<unary-operator>]<primary-expression>
        //<unary-operator>          ::= '+' | '-'
        auto next = nextToken();
        auto prefix = 1;
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        if (next.value().GetType() == TokenType::PLUS_SIGN)
            prefix = 1;
        else if (next.value().GetType() == TokenType::MINUS_SIGN) {
            prefix = -1;
            // _instructions.emplace_back(Operation::LIT, 0);
            // TODO prefix
        } else
            unreadToken();

        auto err = analysePrimaryExpression();
        if (err.has_value())
            return err;
        return {};
    }

    std::optional<CompilationError> Analyser::analysePrimaryExpression() {
        //<primary-expression> ::=
        //     '('<expression>')'
        //    |<identifier>
        //    |<integer-literal>
        //    |<char-literal>
        //    |<floating-literal>
        //    |<function-call>
        auto next = nextToken();
        if (!next.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        switch (next.value().GetType()) {
            case TokenType::LEFT_BRACKET: { // '('<expression>')'
                auto err = analyseExpression();
                if (err.has_value())
                    return err;
                next = nextToken();
                if (!next.has_value() || next.value().GetType() != RIGHT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
                break;
            }
            case TokenType::IDENTIFIER: { // <identifier>, <function-call>
                next = nextToken();
                unreadToken();
                unreadToken();
                if (next.has_value() && next.value().GetType() == LEFT_BRACKET) {
                    auto err = analyseFunctionCall();
                    if (err.has_value())
                        return err;
                } else {
                    next = nextToken();
                    auto val = next.value().GetValueString();
                    auto entry = table.FindSymbol(val);
                    if (!entry.has_value())
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
                    if (entry.value().GetType() == CONSTANT || entry.value().GetType() == CONSTANT_ARGUMENT)
                        addInstruction(IPUSH, std::any_cast<int32_t>(entry.value().GetValue()), 0);
                    else if (entry.value().GetType() == VARIABLE || entry.value().GetType() == ARGUMENT) {
                        auto level = entry.value().GetLevel();
                        auto offset = getOffset(entry.value());
                        addInstruction(LOADA, level == 0, offset);
                        addInstruction(ILOAD, 0, 0);
                    }
                }
                break;
            }
            case TokenType::DECIMAL_LITERAL:
                addInstruction(IPUSH, std::stoul(next.value().GetValueString()), 0);
                break;
            case TokenType::HEXADECIMAL_LITERAL:
                addInstruction(IPUSH, std::stoul(next.value().GetValueString(), nullptr, 16), 0);
                break;
            case TokenType::CHAR_LITERAL:
                addInstruction(IPUSH, next.value().GetValueString()[1], 0);
                break;
            case TokenType::FLOATING_LITERAL:
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrWIP);
            default:
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseFunctionCall() {
        // <function-call> ::= <identifier> '(' [<expression-list>] ')'
        // <expression-list> ::= <expression>{','<expression>}
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

        auto name = next.value().GetValueString();
        auto function = table.GetGlobalFunction(name);
        if (!function.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);

        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

        int argc = 0;
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET) {
            unreadToken();
            while (true) {
                auto err = analyseExpression();
                if (err.has_value())
                    return err;
                argc++;
                next = nextToken();
                if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                    unreadToken();
                    break;
                }
            }
            next = nextToken();
            if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos,
                                                            ErrorCode::ErrNoBracket);
            if (argc != function.value().GetParamsSize())
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrParamsSizeNotIdentical);
            addInstruction(CALL, function.value().GetIndex(), 0);
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseAssignmentExpression() {
        // <assignment-expression> ::= <identifier><assignment-operator><expression>
        // 这里除了语法分析以外还要留意 标识符声明过吗？ 标识符是常量吗？ 需要生成指令吗？
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
        auto name = next.value().GetValueString();
//        if (isDeclared(next.value().GetValueString()))
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
        //if (!isDeclared(str))
        //    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
        //if (isConstant(str))
        //    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrAssignToConstant);
//        if (isUninitializedVariable(str)) {
//            _vars[str] = _uninitialized_vars[str];
//            _uninitialized_vars.erase(str);
//        }
        auto entry = table.FindSymbol(name);
        if (!entry.has_value())
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
        if (entry.value().GetType() == FUNCTION || entry.value().GetType() == CONSTANT ||
            entry.value().GetType() == CONSTANT_ARGUMENT)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrAssignToConstant);

        next = nextToken(); // '='
        if (!next.has_value() ||
            next.value().GetType() !=
            TokenType::ASSIGNMENT_OPERATOR) // NOTE that ASSIGNMENT_OPERATOR='=', EQUAL_SIGN='=='
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidAssignment);

        auto err = analyseExpression();
        if (err.has_value())
            return err;
        // no semicolon
        // _instructions.emplace_back(Operation::STO, getIndex(str));
        addInstruction(ISTORE, 0, 0);
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

    size_t Analyser::addInstruction(Operation type, int x, int y) {
        _instructions.emplace_back(type, x, y, _code_offset++);
        return _instructions.size() - 1;
    }

    void Analyser::updateInstruction(size_t index, int32_t x, int32_t y) {
        _instructions[index].SetX(x);
        _instructions[index].SetY(y);
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