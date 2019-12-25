#include "argparse.hpp"
#include "fmt/core.h"

#include "tokenizer/tokenizer.h"
#include "analyser/analyser.h"
#include "fmts.hpp"

#include <iostream>
#include <fstream>
#include <regex>

std::vector<c0::Token> _tokenize(std::istream &input) {
    c0::Tokenizer tkz(input);
    auto p = tkz.AllTokens();
    if (p.second.has_value()) {
        fmt::print(stderr, "Tokenization error: {}\n", p.second.value());
        exit(2);
    }
    return p.first;
}

void Tokenize(std::istream &input, std::ostream &output) {
    auto v = _tokenize(input);
    for (auto &it : v)
        output << fmt::format("{}\n", it);
    return;
}

std::string getLow(int n, int in) {
    std::string bt;
    for (int i = 1; i <= n; i++) {
        int shiftNum = 8 * (n - i);
        bt.push_back((char) ((in >> shiftNum) & 0xff));
    }
    return bt;
}

char getByte(c0::Operation op) {
    switch (op) {
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

bool isJump(c0::Operation type) { // need removing
    return (type == c0::JMP || type == c0::JNE || type == c0::JE
            || type == c0::JG || type == c0::JL || type == c0::JLE || type == c0::JGE
    );
}

int32_t needParameterNum(c0::Operation type) {
    if (type == c0::LOADA)
        return 2;
    else if (type == c0::IPUSH || type == c0::LOADC || isJump(type) ||
             type == c0::CALL)
        return 1;
    else
        return 0;
}

void getObjByType(c0::Operation type, std::ostream &output, c0::Instruction instruction) {
    std::string op;
    switch (needParameterNum(type)) {
        case 1:
            if (instruction.GetOperation() == c0::IPUSH) {
                op = getLow(4, instruction.GetX());
                output << op;
            } else if (instruction.GetOperation() == c0::LOADC) {
                op = getLow(2, instruction.GetX());
                output << op;
            } else if (instruction.GetOperation() == c0::CALL) {
                op = getLow(2, instruction.GetX());
                output << op;
            } else if (isJump(instruction.GetOperation())) {
                op = getLow(2, instruction.GetX());
                output << op;
            } else if (instruction.GetOperation() == c0::BIPUSH) {
                op = getLow(1, instruction.GetX());
                output << op;
            }
            break;
        case 2:
            op = getLow(2, instruction.GetX());
            output << op;
            op = getLow(4, instruction.GetY());
            output << op;
            break;
        case 0:
            break;
    }
}

void Analyse(std::istream &input, std::ostream &output, bool generateText) {
    auto tks = _tokenize(input);
    c0::Analyser analyser(tks);

    // unpack the tuple into individual variables declared at the call site
    auto[instructions, constants, functions, err] = analyser.Analyse();

    if (err.has_value()) {
        fmt::print(stderr, "Syntactic analysis error: {}\n", err.value());
        exit(2);
    }
    if (generateText) {
        output << ".constants:\n";
        for (auto constant : constants) {
            std::string result;
            switch (constant.GetType()) {
                case c0::STRING_TYPE:
                    result = std::any_cast<std::string>(constant.GetValue());
                    result = regex_replace(result, std::regex("\t"), "\\x09");
                    result = regex_replace(result, std::regex("\n"), "\\x0A");
                    result = regex_replace(result, std::regex("\r"), "\\x0D");
                    output << fmt::format("{} S \"{}\"\n", constant.GetIndex(), result);
                    break;
                case c0::INTEGER_TYPE:
                    output << fmt::format("{} I {}\n", constant.GetIndex(),
                                          std::any_cast<std::int32_t>(constant.GetValue()));
                    break;
                case c0::FLOAT_TYPE: // WIP
                default:
                    output << fmt::format("{} D {}\n", constant.GetIndex(),
                                          std::any_cast<std::string>(constant.GetValue()));
                    break; // cannot handle with other types
            }
        }

        output << ".start:\n";
        bool sm = false;
        int funcIndex = 0;
        int j = 0;
        for (int i = 0; i < instructions.size(); i++) {
            auto instruction = instructions[i];
            if (instructions[i].GetIndex() == 0) {
                if (sm) {
                    j = i;
                    break;
                }
                sm = true;
            }
            output << fmt::format("{}\n", instruction);
        }

        output << ".functions:\n";
        for (auto function : functions)
            output << fmt::format("{} {} {} {}\n", function.GetIndex(), function.GetNameIndex(),
                                  function.GetParamsSize(),
                                  function.GetLevel());
        sm = false;
        funcIndex = 0;
        for (int i = j; i < instructions.size(); i++) {
            auto instruction = instructions[i];
            if (instructions[i].GetIndex() == 0) {
                output << fmt::format(".F{}:\n", funcIndex);
                funcIndex++;
            }
            output << fmt::format("{}\n", instruction);
            if (funcIndex > functions.size())
                break;
        }
    } else {
//        output.put(0x43);
//        output.put(0x30);
//        output.put(0x3a);
//        output.put(0x29);
//        output.put(0x00);
//        output.put(0x00);
//        output.put(0x00);
//        output.put(0x01);

        // magic
        output.write("\x43\x30\x3A\x29", 4);
        // version
        output.write("\x00\x00\x00\x01", 4);

        output.put((char) ((constants.size() >> 8) & 0xff));
        output.put((char) (constants.size() & 0xff));
        for (auto constant : constants) {
            std::string result;
            int32_t integer_value;
            switch (constant.GetType()) {
                case c0::STRING_TYPE:
                    result = std::any_cast<std::string>(constant.GetValue());
                    output.put(0x00); // type=STRING
                    output.put((char) ((result.length() >> 8) & 0xff));
                    output.put((char) (result.length() & 0xff));
                    output << result;
                    break;
                case c0::INTEGER_TYPE:
                    integer_value = std::any_cast<std::int32_t>(constant.GetValue());
                    output.put(0x01); // type=INTEGER
                    output.put((char) ((integer_value >> 8) & 0xff));
                    output.put((char) (integer_value & 0xff));
                    break;
                default:
                    break; // WIP
            }
        }
        bool sm = false;
        int funcIndex = 0;
        int j = 0;

        std::vector<int> arr;
        int count = 0;
        for (auto instruction: instructions) {
            if (instruction.GetIndex() == 0) {
                arr.push_back(count);
                count = 0;
            }
            count++;
        }
        arr.push_back(count);


        output << getLow(2, arr[1]);

        for (int i = 0; i < instructions.size(); i++) {
            auto instruction = instructions[i];
            if (instructions[i].GetIndex() == 0) {
                if (sm) {
                    j = i;
                    break;
                }
                sm = true;
            }
            // output << fmt::format("{}\n", instruction);
            output.put(getByte(instruction.GetOperation()));
            getObjByType(instruction.GetOperation(), output, instruction);
        }

        output << getLow(2, functions.size());
        int cnt = 0;
        for (int i = 2; i < arr.size(); i++) {
            auto fun = functions[i - 2];

            output << getLow(2, fun.GetNameIndex());
            output << getLow(2, fun.GetParamsSize());
            output << getLow(2, fun.GetLevel());
            output << getLow(2, arr[i]);
            for (int k = 0; k < arr[i]; k++) {
                auto instruction = instructions[cnt + k + arr[1]];
                output << getByte(instruction.GetOperation());
                getObjByType(instruction.GetOperation(), output, instruction);
            }
            cnt += arr[i];
        }
    }
    return;
}

int main(int argc, char **argv) {
    argparse::ArgumentParser program("c0");
    program.add_argument("input")
            .help("specify the file to be compiled.");
    program.add_argument("-t")
            .default_value(false)
            .implicit_value(true)
            .help("perform tokenization for the input file.");
    program.add_argument("-s")
            .default_value(false)
            .implicit_value(true)
            .help("perform syntactic analysis for the input file.");
    program.add_argument("-c")
            .default_value(false)
            .implicit_value(true)
            .help("generate some random awesome code.");
    program.add_argument("-o", "--output")
            .required()
            .default_value(std::string("-"))
            .help("specify the output file.");

    try {
        program.parse_args(argc, argv);
    }
    catch (const std::runtime_error &err) {
        fmt::print(stderr, "{}\n\n", err.what());
        program.print_help();
        exit(2);
    }

    auto input_file = program.get<std::string>("input");
    auto output_file = program.get<std::string>("--output");
    std::istream *input;
    std::ostream *output;
    std::ifstream inf;
    std::ofstream outf;
    if (input_file != "-") {
        inf.open(input_file, std::ios::in);
        if (!inf) {
            fmt::print(stderr, "Fail to open {} for reading.\n", input_file);
            exit(2);
        }
        input = &inf;
    } else
        input = &std::cin;
    if (output_file != "-") {
        outf.open(output_file, std::ios::out | std::ios::trunc);
        if (!outf) {
            fmt::print(stderr, "Fail to open {} for writing.\n", output_file);
            exit(2);
        }
        output = &outf;
    } else
        output = &std::cout;
    if (program["-t"] == true && program["-s"] == true) {
        fmt::print(stderr, "You can only perform tokenization or syntactic analysis at one time.");
        exit(2);
    }
    if (program["-t"] == true) {
        Tokenize(*input, *output);
    } else if (program["-s"] == true) {
        Analyse(*input, *output, true);
    } else if (program["-c"] == true) {
        Analyse(*input, *output, false);
    } else {
        fmt::print(stderr, "You must choose tokenization or syntactic analysis.");
        exit(2);
    }
    return 0;
}