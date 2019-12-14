#include "analyser.h"

#include <climits>

namespace c0 {
    bool isMainDefined;

    std::pair<std::vector<Instruction>, std::optional<CompilationError>> Analyser::Analyse() {
        isMainDefined = false;
        auto err = analyseProgram();
        if (err.has_value())
            return std::make_pair(std::vector<Instruction>(), err);
        else
            return std::make_pair(_instructions, std::optional<CompilationError>());
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
                auto err = analyseFunctionDefinition();
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

        while (true) { // <init-declarator-list>
            next = nextToken(); // at least one <init-declarator>, <init-declarator> ::= <identifier>[<initializer>]
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
            // TODO check the identifier

            // optional <initializer>, <initializer> ::= '='<expression>
            next = nextToken();
            if (next.has_value() && next.value().GetType() == TokenType::ASSIGNMENT_OPERATOR) {
                auto err = analyseExpression();
                if (err.has_value())
                    return err;
                // TODO declare an initialized variable
            } else unreadToken();

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
        auto next = nextToken(); // <type-specifier>
        if (!next.has_value() ||
            (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
             next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE))
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoTypeSpecifier);

        // auto type = next.value().GetType();

        next = nextToken(); // <identifier>
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

        isMainDefined = next.value().GetValueString() == "main";

        //<parameter-clause> ::=
        //    '(' [<parameter-declaration-list>] ')'
        //<parameter-declaration-list> ::=
        //    <parameter-declaration>{','<parameter-declaration>}
        //<parameter-declaration> ::=
        //    [<const-qualifier>]<type-specifier><identifier>

        next = nextToken(); // (
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

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

            next = nextToken(); // <identifier>
            if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);

            next = nextToken(); // ，
            if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
                unreadToken(); // finish
                break;
            }
        }

        next = nextToken(); // )
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);

        auto err = analyseCompoundStatement();
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
            case TokenType::LEFT_BRACE: { // '{' <statement-seq> '}'
                next = nextToken(); // {
                if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);
                // err = analyseCompoundStatement();
                err = analyseStatementSequence();
                if (err.has_value())
                    return err;
                next = nextToken(); // }
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACE)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);
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
                break;
            }
            case TokenType::SEMICOLON:
                nextToken();
            default:
                break;
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCondition() {
        // <condition> ::= <expression>[<relational-operator><expression>]
        // <relational-operator>     ::= '<' | '<=' | '>' | '>=' | '!=' | '=='
        auto err = analyseExpression();
        if (err.has_value())
            return err;
        auto next = nextToken(); // <relational-operator>
        if (!next.has_value() || (next.value().GetType() != TokenType::LESS_SIGN &&
                                  next.value().GetType() != TokenType::LESS_OR_EQUAL_SIGN &&
                                  next.value().GetType() != TokenType::GREATER_SIGN &&
                                  next.value().GetType() != TokenType::GREATER_OR_EQUAL_SIGN &&
                                  next.value().GetType() != TokenType::NOT_EQUAL_SIGN &&
                                  next.value().GetType() != TokenType::EQUAL_SIGN))
            unreadToken();
        else {
            err = analyseExpression();
            if (err.has_value())
                return err;
        }
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

            auto err = analyseCondition();
            if (err.has_value())
                return err;

            next = nextToken(); // )
            if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

            err = analyseStatement(); // statement
            if (err.has_value())
                return err;

            next = nextToken(); // optional else
            if (next.has_value() && next.value().GetType() == TokenType::ELSE) {
                err = analyseStatement(); // statement
                if (err.has_value())
                    return err;
            } else unreadToken();
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
                auto err = analyseCondition();
                if (err.has_value())
                    return err;
                next = nextToken(); // )
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);
                err = analyseStatement();
                if (err.has_value())
                    return err;
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
                err = analyseCondition();
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
                    auto err = analyseCondition();
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
                        } else
                            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

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
                    next = nextToken(); // ;
                    if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
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
                    next = nextToken();
                } else { // must be expression
                    auto err = analyseExpression();
                    if (err.has_value())
                        return err;
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
        next = nextToken(); // )
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
        next = nextToken(); // ;
        if (!next.has_value() || next.value().GetType() != TokenType::SEMICOLON)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoSemicolon);
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
            // if (next.value().GetType() == TokenType::PLUS_SIGN);
            // else if (next.value().GetType() == TokenType::MINUS_SIGN); // do something
            err = analyseMultiplicativeExpression();
            if (err.has_value())
                return err;
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
            // if (next.value().GetType() == TokenType::MULTIPLICATION_SIGN);
            // else if (next.value().GetType() == TokenType::DIVISION_SIGN); // do something
            auto err = analyseCastExpression();
            if (err.has_value())
                return err;
        }
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCastExpression() {
        //<cast-expression> ::=
        //    {'('<type-specifier>')'}<unary-expression>
        while (true) {
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
                        break;
                    }
                }
            } else {
                unreadToken();
                break;
            }
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
            _instructions.emplace_back(Operation::LIT, 0);
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
                if (next.has_value() && next.value().GetType() == LEFT_BRACKET) {
                    unreadToken();
                    auto err = analyseFunctionCall();
                    if (err.has_value())
                        return err;
                }
                break;
            }
            case TokenType::DECIMAL_LITERAL:
            case TokenType::HEXADECIMAL_LITERAL:
            case TokenType::CHAR_LITERAL:
            case TokenType::FLOATING_LITERAL:
                break;
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
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACKET)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
        next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET) {
            unreadToken();
            while (true) {
                auto err = analyseExpression();
                if (err.has_value())
                    return err;
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
        }
    }

    std::optional<CompilationError> Analyser::analyseAssignmentExpression() {
        // <assignment-expression> ::= <identifier><assignment-operator><expression>
        // 这里除了语法分析以外还要留意 标识符声明过吗？ 标识符是常量吗？ 需要生成指令吗？
        auto next = nextToken();
        if (!next.has_value() || next.value().GetType() != TokenType::IDENTIFIER)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNeedIdentifier);
        auto str = next.value().GetValueString();
//        if (isDeclared(next.value().GetValueString()))
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrDuplicateDeclaration);
        //if (!isDeclared(str))
        //    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
        //if (isConstant(str))
        //    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrAssignToConstant);
        if (isUninitializedVariable(str)) {
            _vars[str] = _uninitialized_vars[str];
            _uninitialized_vars.erase(str);
        }

        next = nextToken(); // '='
        if (!next.has_value() ||
            next.value().GetType() !=
            TokenType::ASSIGNMENT_OPERATOR) // NOTE that ASSIGNMENT_OPERATOR='=', EQUAL_SIGN='=='
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidAssignment);

        auto err = analyseExpression();
        if (err.has_value())
            return err;
        // no semicolon
        _instructions.emplace_back(Operation::STO, getIndex(str));
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