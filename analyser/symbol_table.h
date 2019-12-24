#pragma once

#include "error/error.h"

#include <string>
#include <cstdint>
#include <any>
#include <utility>
#include <map>
#include <vector>
#include <optional>

namespace c0 {
    enum SymbolType {
        CONSTANT,
        VARIABLE,
        ARGUMENT,
        CONSTANT_ARGUMENT,
        FUNCTION
    };

    enum ValueType {
        VOID_TYPE,
        CHAR_TYPE,
        STRING_TYPE,
        INTEGER_TYPE,
        FLOAT_TYPE
    };

    class GlobalConstant final {
    private:
        using int32_t = std::int32_t;
    private:
        int32_t _index;
        std::any _value;
        ValueType _type;
    public:
        GlobalConstant(int32_t index, const std::any &value, ValueType type) : _index(index), _value(value),
                                                                               _type(type) {}

        int32_t GetIndex() const {
            return _index;
        }

        void SetIndex(int32_t index) {
            _index = index;
        }

        const std::any &GetValue() const {
            return _value;
        }

        void SetValue(const std::any &value) {
            _value = value;
        }

        ValueType GetType() const {
            return _type;
        }

        void SetType(ValueType type) {
            _type = type;
        }
    };

    class Arg final {
    public:
        Arg(bool isConstant, const std::string &name, ValueType type) : _is_constant(isConstant), _name(name),
                                                                        _type(type) {}

        bool IsConstant() const {
            return _is_constant;
        }

        void SetConstant(bool isConstant) {
            _is_constant = isConstant;
        }

        const std::string &GetName() const {
            return _name;
        }

        void SetName(const std::string &name) {
            _name = name;
        }

        ValueType GetType() const {
            return _type;
        }

        void SetType(ValueType type) {
            _type = type;
        }

    private:
        bool _is_constant;
        std::string _name;
        ValueType _type;
    };

    class GlobalFunction {
    private:
        using int32_t = std::int32_t;
    public:
        GlobalFunction(int32_t index, int32_t nameIndex, int32_t paramsSize, int32_t level, int32_t localsLen,
                       ValueType returnType, const std::vector<Arg> &args) : _index(index), _name_index(nameIndex),
                                                                             _params_size(paramsSize), _level(level),
                                                                             _locals_len(localsLen),
                                                                             _return_type(returnType), _args(args) {}

        int32_t GetIndex() const {
            return _index;
        }

        void SetIndex(int32_t index) {
            _index = index;
        }

        int32_t GetNameIndex() const {
            return _name_index;
        }

        void SetNameIndex(int32_t nameIndex) {
            _name_index = nameIndex;
        }

        int32_t GetParamsSize() const {
            return _params_size;
        }

        void SetParamsSize(int32_t paramsSize) {
            _params_size = paramsSize;
        }

        int32_t GetLevel() const {
            return _level;
        }

        void SetLevel(int32_t level) {
            _level = level;
        }

        int32_t GetLocalsLen() const {
            return _locals_len;
        }

        void SetLocalsLen(int32_t localsLen) {
            _locals_len = localsLen;
        }

        ValueType GetReturnType() const {
            return _return_type;
        }

        void SetReturnType(ValueType returnType) {
            _return_type = returnType;
        }

        const std::vector<Arg> &GetArgs() const {
            return _args;
        }

        void SetArgs(const std::vector<Arg> &args) {
            _args = args;
        }

    private:
        int32_t _index;
        int32_t _name_index;
        int32_t _params_size;
        int32_t _level;
        int32_t _locals_len;
        ValueType _return_type;
        std::vector<Arg> _args;
    };

    class SymbolEntry final {
    private:
        using int32_t = std::int32_t;
    public:
        SymbolEntry() : _type(FUNCTION), _value_type(VOID_TYPE), _name("main"), _value("main"), _level(0), _offset(0) {}

        SymbolEntry(SymbolType type, ValueType valueType, const std::string &name, const std::any &value, int32_t level,
                    int32_t offset) : _type(type), _value_type(valueType), _name(name), _value(value), _level(level),
                                      _offset(offset) {}

        SymbolType GetType() const {
            return _type;
        }

        void SetType(SymbolType type) {
            _type = type;
        }

        ValueType GetValueType() const {
            return _value_type;
        }

        void SetValueType(ValueType valueType) {
            _value_type = valueType;
        }

        const std::string &GetName() const {
            return _name;
        }

        void SetName(const std::string &name) {
            _name = name;
        }

        const std::any &GetValue() const {
            return _value;
        }

        void SetValue(const std::any &value) {
            _value = value;
        }

        int32_t GetLevel() const {
            return _level;
        }

        void SetLevel(int32_t level) {
            _level = level;
        }

        int32_t GetOffset() const {
            return _offset;
        }

        void SetOffset(int32_t offset) {
            _offset = offset;
        }

        std::string GetValueString() const {
            try {
                return std::any_cast<std::string>(_value);
            }
            catch (const std::bad_any_cast &) {}
            try {
                return std::string(1, std::any_cast<char>(_value));
            }
            catch (const std::bad_any_cast &) {}
            try {
                return std::to_string(std::any_cast<int32_t>(_value));
            }
            catch (const std::bad_any_cast &) {
                DieAndPrint("No suitable cast for token value.");
            }
            return "Invalid";
        }

    private:
        SymbolType _type;
        ValueType _value_type;
        std::string _name;
        std::any _value;
        int32_t _level;
        int32_t _offset;
    };

    class SymbolTable final {
    public:
        SymbolTable() : symbols({}), _level(0), _next_offset(0) {}

        int32_t GetLevel() const {
            return _level;
        }

        void SetLevel(int32_t level) {
            _level = level;
        }

        int32_t GetNextOffset() const {
            return _next_offset;
        }

        void SetNextOffset(int32_t nextOffset) {
            _next_offset = nextOffset;
        }

        bool IsDuplicate(std::string name) {
            auto it = symbols.find(name);
            return it != symbols.end();
        }

        std::map<std::string, SymbolEntry> symbols;

    private:
        int32_t _level;
        int32_t _next_offset;
    };

    class SymbolTables final {
    private:
        using int32_t = std::int32_t;
    public:
        SymbolTables() : _current_level(0), _symbol_table({SymbolTable()}), _iterator(_symbol_table.begin()),
                         _global_constants({}), _global_functions({}) {}

        void UpdateSymbol(std::string name, std::any value, SymbolType symbolType, ValueType valueType) {
            int32_t offset = -1;
            if (symbolType != FUNCTION) {
                offset = _symbol_table[_current_level].GetNextOffset();
                _symbol_table[_current_level].SetNextOffset(offset + 1);
            }
            _symbol_table[_current_level].symbols[name] = SymbolEntry(symbolType, valueType, name, value, _current_level, offset);
        }

        std::optional<SymbolEntry> FindSymbol(std::string name) {
            auto i = _current_level; // _iterator;
            do {
                auto table = _symbol_table[i].symbols; // iter->symbols;
                auto it = table.find(name);
                if (it != table.end())
                    return it->second;
                i--;
            } while (i != -1);
            return {};
        }

        std::optional<CompilationError> NextLevel() {
            if (_current_level + 1 == (int32_t) _symbol_table.size())
                _symbol_table.push_back(SymbolTable());
            _iterator++;
            _current_level++;
            return {};
        }

        std::optional<CompilationError> PreviousLevel() {
            if (_current_level == 0)
                return std::make_optional<CompilationError>(0, 0, ErrorCode::ErrNoPreviousLevel);
            _iterator--;
            _current_level--;
            _symbol_table.pop_back();
            return {};
        }

        std::optional<CompilationError> PreviousLevelForSum() {
            if (_current_level == 0)
                return std::make_optional<CompilationError>(0, 0, ErrorCode::ErrNoPreviousLevel);
            _iterator--;
            _current_level--;
            return {};
        }

        void AddGlobalConstant(std::string name, std::any value, ValueType type) {
            auto index = _global_constants.size();
            _global_constants.push_back(GlobalConstant(index, value, type));
            _global_constants_index[name] = index;
        }

        std::optional<GlobalConstant> GetGlobalConstant(std::string name) {
            auto it = _global_constants_index.find(name);
            if (it != _global_constants_index.end())
                return _global_constants[it->second];
            return {};
        }

        void AddGlobalFunction(std::string name, int params_size, int level, std::vector<Arg> args, int locals_len,
                               ValueType return_type) {
            auto index = _global_functions.size();
            auto name_index = _global_constants_index[name];
            _global_functions_index[name] = index;
            _global_functions.push_back(
                    GlobalFunction(index, name_index, params_size, level, locals_len, return_type, args));
        }

        std::optional<GlobalFunction> GetGlobalFunction(std::string name) {
            auto it = _global_functions_index.find(name);
            if (it != _global_functions_index.end())
                return _global_functions[it->second];
            return {};
        }

        int32_t GetCurrentLevel() const {
            return _current_level;
        }

        bool IsDuplicate(std::string name) {
            return _symbol_table[_current_level].IsDuplicate(name);
        }

        int32_t GetNextOffset(int32_t level) const {
            return _symbol_table[level].GetNextOffset();
        }

        int32_t _current_level;
        std::vector<SymbolTable> _symbol_table;
        std::vector<SymbolTable>::iterator _iterator;
        std::vector<GlobalConstant> _global_constants;
        std::vector<GlobalFunction> _global_functions;
        std::map<std::string, int32_t> _global_constants_index;
        std::map<std::string, int32_t> _global_functions_index;
    };

}