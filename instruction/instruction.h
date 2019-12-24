#pragma once

#include <cstdint>
#include <utility>

namespace c0 {

    enum Operation {
        ILL = 0,
        LIT,
        LOD,
        STO,
        ADD,
        SUB,
        MUL,
        DIV,
        WRT,

        IPUSH,
        LOADA,
        ILOAD,
        IMUL,
        IDIV,
        IADD,
        ISUB,
        RET,
        IRET,
        JE,
        JNE,
        JL,
        JGE,
        JG,
        JLE,
        ICMP,
        JMP,
        CALL,
        ISTORE,
        SPRINT,
        IPRINT,
        CPRINT,
        LOADC,
        ISCAN,
        CSCAN,
        NOP,
        POP,
        INEG,
        BIPUSH,
        PRINTL,
        I2C
    };

    class Instruction final {
    private:
        using int32_t = std::int32_t;
    public:
        friend void swap(Instruction &lhs, Instruction &rhs);

    public:
        Instruction(Operation opr, int32_t x, int32_t y, int32_t index) : _opr(opr), _x(x), _y(y), _index(index) {}

        Instruction() : Instruction(Operation::ILL, 0, 0, 0) {}

        Instruction(const Instruction &i) {
            _opr = i._opr;
            _x = i._x;
            _y = i._y;
            _index = i._index;
        }

        Instruction(Instruction &&i) : Instruction() { swap(*this, i); }

        Instruction &operator=(Instruction i) {
            swap(*this, i);
            return *this;
        }

        bool operator==(const Instruction &i) const {
            return _opr == i._opr && _x == i._x && _y == i._y && _index == i._index;
        }

        Operation GetOperation() const { return _opr; }

        int32_t GetX() const { return _x; }

        int32_t GetY() const { return _y; }

        void SetX(int32_t x) {
            _x = x;
        }

        void SetY(int32_t y) {
            _y = y;
        }

        int32_t GetIndex() const { return _index; };

    private:
        Operation _opr;
        int32_t _x;
        int32_t _y;
        int32_t _index;
    };

    inline void swap(Instruction &lhs, Instruction &rhs) {
        using std::swap;
        swap(lhs._opr, rhs._opr);
        swap(lhs._x, rhs._x);
        swap(lhs._y, rhs._y);
        swap(lhs._index, rhs._index);
    }
}