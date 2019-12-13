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

    std::optional<CompilationError> Analyser::analyseProgram() {
        //<C0-program> ::=
        //    {<variable-declaration>}{<function-definition>}
        auto err = analyseVariableDeclaration();
        if (err.has_value())
            return err;
        err = analyseFunctionDefinition();
        if (err.has_value())
            return err;
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
            if (!next.has_value() || // type-specifier
                (next.value().GetType() != TokenType::CONST && next.value().GetType() != TokenType::VOID &&
                 next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE)) {
                unreadToken();
                break; // if there's no const or type-specifier, we suppose no variable is declared
            }

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
        return {};
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
        auto isMainDefined = false;
        while (true) {
            auto next = nextToken(); // <type-specifier>
            if (!next.has_value() ||
                (next.value().GetType() != TokenType::VOID && next.value().GetType() != TokenType::INT &&
                 next.value().GetType() != TokenType::CHAR && next.value().GetType() != TokenType::DOUBLE)) {
                unreadToken(); // no function definition
                break;
            }

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

            auto err = analyseCompoundStatment();
            if (err.has_value())
                return err;
        }
        if (!isMainDefined)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoMain);
        return {};
    }

    std::optional<CompilationError> Analyser::analyseCompoundStatment() {
        //<compound-statement> ::=
        //    '{' {<variable-declaration>} <statement-seq> '}'

        auto next = nextToken(); // {
        if (!next.has_value() || next.value().GetType() != TokenType::LEFT_BRACE)
            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBrace);

        auto err = analyseVariableDeclaration();
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
                return {}; // empty

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
                err = analyseCompoundStatment();
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
                next = nextToken(); // the identifier
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
            auto err = analyseExpression();
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
                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrInvalidStatement);

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
//                            next = nextToken();
//                            if (!next.has_value())
//                                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
//                            else if (next.value().GetType() == TokenType::RIGHT_BRACKET)
//                                break;
//                            else {
//                                while (true) {
//                                    auto err = analyseExpression();
//                                    if (err.has_value())
//                                        return err;
//                                    next = nextToken();
//                                    if (!next.has_value() || next.value().GetType() != TokenType::COMMA) {
//                                        unreadToken();
//                                        break;
//                                    }
//                                }
//                                next = nextToken(); // )
//                                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
//                                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
//                            }
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
            if (next.value().GetType() == TokenType::PLUS_SIGN);
            else if (next.value().GetType() == TokenType::MINUS_SIGN); // do something
            auto err = analyseMultiplicativeExpression();
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
            if (next.value().GetType() == TokenType::MULTIPLICATION_SIGN);
            else if (next.value().GetType() == TokenType::DIVISION_SIGN); // do something
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
                        break;
                    default:
                        return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
                }
                next = nextToken();
                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNoBracket);
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

    /*std::optional<CompilationError> Analyser::analyseExpressionOld() {
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
*/
    std::optional<CompilationError> Analyser::analyseAssignmentExpression() {
        // <assignment-expression> ::= <identifier><assignment-operator><expression>
        // 这里除了语法分析以外还要留意 标识符声明过吗？ 标识符是常量吗？ 需要生成指令吗？
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

    // <输出语句> ::= 'print' '(' <表达式> ')' ';'
    /*std::optional<CompilationError> Analyser::analyseOutputStatement() {
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
    }*/

    // <项> :: = <因子>{ <乘法型运算符><因子> }
    // 需要补全
//    std::optional<CompilationError> Analyser::analyseItem() {
//        // 可以参考 <表达式> 实现
//        auto err = analyseFactor();
//        if (err.has_value())
//            return err;
//        while (true) {
//            auto next = nextToken();
//            if (!next.has_value())
//                return {};
//            auto type = next.value().GetType();
//            if (type != TokenType::MULTIPLICATION_SIGN && type != TokenType::DIVISION_SIGN) {
//                unreadToken();
//                return {};
//            }
//            err = analyseFactor();
//            if (err.has_value())
//                return err;
//
//            // 根据结果生成指令
//            if (type == TokenType::MULTIPLICATION_SIGN)
//                _instructions.emplace_back(Operation::MUL, 0);
//            else if (type == TokenType::DIVISION_SIGN)
//                _instructions.emplace_back(Operation::DIV, 0);
//        }
//        return {};
//    }

    // <因子> ::= [<符号>]( <标识符> | <无符号整数> | '('<表达式>')' )
    // 需要补全
//    std::optional<CompilationError> Analyser::analyseFactor() {
//        // [<符号>]
//        auto next = nextToken();
//        auto prefix = 1;
//        if (!next.has_value())
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
//        if (next.value().GetType() == TokenType::PLUS_SIGN)
//            prefix = 1;
//        else if (next.value().GetType() == TokenType::MINUS_SIGN) {
//            prefix = -1;
//            _instructions.emplace_back(Operation::LIT, 0);
//        } else
//            unreadToken();
//
//        // 预读
//        next = nextToken();
//        if (!next.has_value())
//            return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
//        switch (next.value().GetType()) {
//            // 这里和 <语句序列> 类似，需要根据预读结果调用不同的子程序
//            // 但是要注意 default 返回的是一个编译错误
//            case TokenType::IDENTIFIER: {
//                if (!isDeclared(next.value().GetValueString()))
//                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotDeclared);
//                if (isUninitializedVariable(next.value().GetValueString()))
//                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrNotInitialized);
//                _instructions.emplace_back(Operation::LOD, getIndex(next.value().GetValueString()));
//                break;
//            }
//            case TokenType::DECIMAL_LITERAL: {
//                _instructions.emplace_back(Operation::LIT, std::stoi(next.value().GetValueString()));
//                break;
//            }
//            case TokenType::LEFT_BRACKET: {
//                auto err = analyseExpression();
//                if (err.has_value())
//                    return err;
//                next = nextToken();
//                if (!next.has_value() || next.value().GetType() != TokenType::RIGHT_BRACKET)
//                    return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
//                break;
//            }
//            default:
//                return std::make_optional<CompilationError>(_current_pos, ErrorCode::ErrIncompleteExpression);
//        }
//        if (prefix == -1)
//            _instructions.emplace_back(Operation::SUB, 0);
//        return {};
//    }

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