#pragma once

#include "error/error.h"

#include <string>
#include <cstdint>
#include <any>
#include <utility>
#include <map>

namespace c0 {
    enum SymbolType {
        INT,
        DOUBLE,
        CHAR,
        VOID,
        STRING
    };

    class SymbolEntry final {
    private:
        using uint64_t = std::uint64_t;
        using int32_t = std::int32_t;
    public:
        SymbolEntry(SymbolType type, std::string name, std::any value, uint64_t level, uint64_t offset) : _type(type),
                                                                                                          _name(std::move(
                                                                                                                  name)),
                                                                                                          _value(std::move(
                                                                                                                  value)),
                                                                                                          _level(level),
                                                                                                          _offset(offset) {}

        SymbolType GetType() const { return _type; };

        std::string GetName() const { return _name; };

        std::any GetValue() const { return _value; };

        uint64_t GetLevel() const { return _level; };

        uint64_t GetOffset() const { return _offset; };

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
        uint64_t _level;
        uint64_t _offset;
    };

    class SymbolTable final {
    private:
        using uint64_t = std::uint64_t;
    public:
        SymbolTable(): _level(0)

    private:
        uint64_t _level;
        uint64_t _next_offset;
        std::map<std::string, SymbolEntry> _symbol_table;
        SymbolTable *prev;
        SymbolTable *next;
    };

}