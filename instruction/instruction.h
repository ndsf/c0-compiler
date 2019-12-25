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

        bool IsJumpInstruction() const {
            return (_opr == c0::JMP || _opr == c0::JNE || _opr == c0::JE || _opr == c0::JG || _opr == c0::JL ||
                    _opr == c0::JLE || _opr == c0::JGE);
        }

        int32_t ParameterNum() const {
            if (_opr == c0::LOADA)
                return 2;
            else if (_opr == c0::IPUSH || _opr == c0::LOADC || IsJumpInstruction() || _opr == c0::CALL)
                return 1;
            else
                return 0;
        }

        char GetByte() const {
            switch (_opr) {
                case c0::IPUSH:
                    return 0x02;
                case c0::LOADA:
                    return 0x0a;
                case c0::ILOAD:
                    return 0x10;
                case c0::IMUL:
                    return 0x38;
                case c0::IDIV:
                    return 0x3c;
                case c0::IADD:
                    return 0x30;
                case c0::ISUB:
                    return 0x34;
                case c0::RET:
                    return 0x88;
                case c0::IRET:
                    return 0x89;
                case c0::JE:
                    return 0x71;
                case c0::JNE:
                    return 0x72;
                case c0::JL:
                    return 0x73;
                case c0::JGE:
                    return 0x74;
                case c0::JG:
                    return 0x75;
                case c0::JLE:
                    return 0x76;
                case c0::ICMP:
                    return 0x44;
                case c0::JMP:
                    return 0x70;
                case c0::CALL:
                    return 0x80;
                case c0::ISTORE:
                    return 0x20;
                case c0::SPRINT:
                    return 0xa3;
                case c0::IPRINT:
                    return 0xa0;
                case c0::CPRINT:
                    return 0xa2;
                case c0::LOADC:
                    return 0x09;
                case c0::ISCAN:
                    return 0xb0;
                case c0::CSCAN:
                    return 0xb2;
                case c0::NOP:
                    return 0x00;
                case c0::POP:
                    return 0x04;
                case c0::PRINTL:
                    return 0xaf;
                case c0::BIPUSH:
                    return 0x01;
                case c0::I2C:
                    return 0x62;
                case c0::INEG:
                    return 0x40;
                default:
                    return 'X';
            }
        }

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