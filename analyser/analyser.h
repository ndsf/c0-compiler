#pragma once

#include "error/error.h"
#include "instruction/instruction.h"
#include "tokenizer/token.h"
#include "symbol_table.h"

#include <vector>
#include <optional>
#include <utility>
#include <map>
#include <cstdint>
#include <cstddef> // for std::size_t

namespace c0 {

    class Analyser final {
    private:
        using uint64_t = std::uint64_t;
        using int64_t = std::int64_t;
        using uint32_t = std::uint32_t;
        using int32_t = std::int32_t;
    public:
        Analyser(std::vector<Token> v)
                : _tokens(std::move(v)), _offset(0), _instructions({}), _code_offset(0), _current_pos(0, 0),
                  _uninitialized_vars({}), _vars({}), _consts({}), _nextTokenIndex(0),
                  _context(GlobalFunction(0, 0, 0, 0, 0, VOID_TYPE, {})) {}

        Analyser(Analyser &&) = delete;

        Analyser(const Analyser &) = delete;

        Analyser &operator=(Analyser) = delete;

        // 唯一接口
        std::tuple<std::vector<Instruction>, std::vector<GlobalConstant>, std::vector<GlobalFunction>, std::optional<CompilationError>> Analyse();

    private:
        // 所有的递归子程序

        // <程序>
        std::optional<CompilationError> analyseProgram();

        std::optional<CompilationError> analyseFunctionDefinition();

        std::optional<CompilationError> analyseCompoundStatement();

        std::optional<CompilationError> analyseStatement();

        std::optional<CompilationError> analyseCondition(int32_t &falseJumpAddress);

        std::optional<CompilationError> analyseConditionStatement();

        std::optional<CompilationError> analyseLoopStatement();

        std::optional<CompilationError> analyseJumpStatement();

        std::optional<CompilationError> analysePrintStatement();

        std::optional<CompilationError> analyseScanStatement();

        std::optional<CompilationError> analyseMultiplicativeExpression();

        std::optional<CompilationError> analyseCastExpression();

        std::optional<CompilationError> analyseUnaryExpression();

        std::optional<CompilationError> analysePrimaryExpression();

        std::optional<CompilationError> analyseFunctionCall();

        // <主过程>
        // std::optional<CompilationError> analyseMain();
        // <常量声明>
        // <变量声明>
        std::optional<CompilationError> analyseMultipleVariableDeclaration();

        std::optional<CompilationError> analyseVariableDeclaration();

        // <语句序列>
        std::optional<CompilationError> analyseStatementSequence();

        // <常表达式>
        // 这里的 out 是常表达式的值
        // std::optional<CompilationError> analyseConstantExpression(int32_t& out);
        // <表达式>
        std::optional<CompilationError> analyseExpression();

        // <赋值语句>
        std::optional<CompilationError> analyseAssignmentExpression();
        // <输出语句>
        // std::optional<CompilationError> analyseOutputStatement();
        // <项>
        // std::optional<CompilationError> analyseItem();
        // <因子>
        // std::optional<CompilationError> analyseFactor();

        // Token 缓冲区相关操作

        // 返回下一个 token
        std::optional<Token> nextToken();

        // 回退一个 token
        void unreadToken();

        // 下面是符号表相关操作

        // helper function
        void _add(const Token &, std::map<std::string, int32_t> &);

        // 添加变量、常量、未初始化的变量
        size_t addInstruction(Operation, int32_t, int32_t);

        void updateInstruction(size_t, int32_t, int32_t);

    private:
        std::vector<Token> _tokens;
        std::size_t _offset;
        std::vector<Instruction> _instructions;
        int32_t _code_offset;
        std::pair<uint64_t, uint64_t> _current_pos;

        // 为了简单处理，我们直接把符号表耦合在语法分析里
        // 变量                   示例
        // _uninitialized_vars    var a;
        // _vars                  var a=1;
        // _consts                const a=1;
        std::map<std::string, int32_t> _uninitialized_vars;
        std::map<std::string, int32_t> _vars;
        std::map<std::string, int32_t> _consts;
        // 下一个 token 在栈的偏移
        int32_t _nextTokenIndex;

        SymbolTables table;

        int32_t getOffset(SymbolEntry entry) {
            auto level = entry.GetLevel();
            auto offset = entry.GetOffset();
            if (level) {
                for (int i = 1; i < level; i++) {
                    table.PreviousLevelForSum();
                    offset += table.GetNextOffset(i);
                }
                for (int i = 1; i < level; i++)
                    table.NextLevel();
            }
            return offset;
        }

        GlobalFunction _context;

        ValueType tokenTypeToValueType(TokenType tokenType) {
            switch (tokenType) {
                case CHAR:
                    return CHAR_TYPE;
                case INT:
                    return INTEGER_TYPE;
                case DOUBLE:
                    return FLOAT_TYPE;
                case STRING_LITERAL:
                    return STRING_TYPE;
                case VOID:
                default:
                    return VOID_TYPE;
            }
        }
    };
}
