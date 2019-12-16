#pragma once

#include "error/error.h"

#include <string>
#include <cstdint>
#include <any>
#include <utility>
#include <map>
#include <list>
#include <vector>

namespace c0 {
    enum SymbolType {
        CONSTANT,
        VARIABLE,
        ARGUMENT,
        CONSTANT_ARGUMENT,
        FUNCTION
    };

    class SymbolEntry final {
    private:
        using int32_t = std::int32_t;
    public:
        SymbolEntry(SymbolType type, std::string name, std::any value, int32_t level, int32_t offset) : _type(type),
                                                                                                          _name(std::move(
                                                                                                                  name)),
                                                                                                          _value(std::move(
                                                                                                                  value)),
                                                                                                          _level(level),
                                                                                                          _offset(offset) {}

        SymbolType GetType() const { return _type; };

        std::string GetName() const { return _name; };

        std::any GetValue() const { return _value; };

        int32_t GetLevel() const { return _level; };

        int32_t GetOffset() const { return _offset; };

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
        std::string _name;
        std::any _value;
        int32_t _level;
        int32_t _offset;
    };

    class SymbolTable final {
    public:
        SymbolTable() : level(0), next_offset(0), symbols({}) {}

        int32_t level;
        int32_t next_offset;
        std::map<std::string, SymbolEntry> symbols;
    };

    class SymbolTables final {
    private:
        using int32_t = std::int32_t;
    public:
        SymbolTables() : _current_level(0), _symbol_table({new SymbolTable()}), _iterator(_symbol_table.begin()) {}

        bool IsDuplicate(std::string name) {
            auto table = _iterator->symbols;
            auto it = table.find(name);
            return it != table.end();
        }

        void UpdateSymbol(std::string name, std::any value, SymbolType symbolType) {
            int32_t offset = -1;
            if (symbolType != FUNCTION)
                offset = _iterator->next_offset++;
            _iterator->symbols[name] = new SymbolEntry(symbolType, name, value, _current_level, offset);
        }

        SymbolEntry FindSymbol(std::string name) {
            auto iter = _iterator;
            do {
                auto table = _iterator->symbols;
                auto it = table.find(name);
                if (it != table.end())
                    return *it;
            } while (--iter != _symbol_table.begin());
        }

        std::optional<CompilationError> NextLevel() {
            if (_iterator == _symbol_table.end() - 1)
                _symbol_table.push_back(new SymbolTable());
            _iterator++;
            _current_level++;
            return {};
        }

        std::optional<CompilationError> PreviousLevel() {
            if (_iterator == _symbol_table.begin())
                return std::make_optional<CompilationError>(ErrorCode::ErrNoPreviousLevel);
            _iterator--;
            _current_level--;
            return {};
        }

    private:
        int32_t _current_level;
        std::list<SymbolTable> _symbol_table;
        std::list<SymbolTable>::iterator _iterator;
    };

}