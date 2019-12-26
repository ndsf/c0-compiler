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

std::string getLowNBytes(int32_t n, int32_t in) {
    std::string bt;
    for (int32_t i = 1; i <= n; i++) {
        auto shiftNum = 8 * (n - i);
        bt.push_back((char) ((in >> shiftNum) & 0xff));
    }
    return bt;
}

void putParameters(std::ostream &output, c0::Instruction instruction) {
    std::string op;
    switch (instruction.ParameterNum()) {
        case 1:
            if (instruction.IsJumpInstruction()) {
                op = getLowNBytes(2, instruction.GetX());
            } else {
                auto opr = instruction.GetOperation();
                switch (opr) {
                    case c0::IPUSH:
                        op = getLowNBytes(4, instruction.GetX());
                        break;
                    case c0::LOADC:
                        op = getLowNBytes(2, instruction.GetX());
                        break;
                    case c0::CALL:
                        op = getLowNBytes(2, instruction.GetX());
                        break;
                    case c0::BIPUSH:
                        op = getLowNBytes(1, instruction.GetX());
                        break;
                }
            }
            output << op;
            break;
        case 2:
            output << getLowNBytes(2, instruction.GetX()) << getLowNBytes(4, instruction.GetY());
            break;
        case 0:
        default:
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
        bool flag = false;
        int funcIndex = 0;
        int j = 0;
        for (int i = 0; i < instructions.size(); i++) {
            auto instruction = instructions[i];
            if (instructions[i].GetIndex() == 0) {
                if (flag) {
                    j = i;
                    break;
                }
                flag = true;
            }
            output << fmt::format("{}\n", instruction);
        }

        output << ".functions:\n";
        for (auto function : functions)
            output << fmt::format("{} {} {} {}\n", function.GetIndex(), function.GetNameIndex(),
                                  function.GetParamsSize(),
                                  function.GetLevel());
        flag = false;
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
        // magic
        output.write("\x43\x30\x3A\x29", 4);
        // version
        output.write("\x00\x00\x00\x01", 4);

        output << getLowNBytes(2, constants.size());
        for (auto constant : constants) {
            std::string result;
            int32_t integer_value;
            switch (constant.GetType()) {
                case c0::STRING_TYPE:
                    result = std::any_cast<std::string>(constant.GetValue());
                    output.put(0x00); // type=STRING
                    output << getLowNBytes(2, result.length());
                    output << result;
                    break;
                case c0::INTEGER_TYPE:
                    integer_value = std::any_cast<std::int32_t>(constant.GetValue());
                    output.put(0x01); // type=INTEGER
                    output << getLowNBytes(2, integer_value);
                    break;
                default:
                    break; // WIP
            }
        }
        bool flag = false;
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


        output << getLowNBytes(2, arr[1]);

        for (int i = 0; i < instructions.size(); i++) {
            auto instruction = instructions[i];
            if (instructions[i].GetIndex() == 0) {
                if (flag) {
                    j = i;
                    break;
                }
                flag = true;
            }
            // output << fmt::format("{}\n", instruction);
            output.put(instruction.GetByte());
            putParameters(output, instruction);
        }

        output << getLowNBytes(2, functions.size());
        int cnt = 0;
        for (int i = 2; i < arr.size(); i++) {
            auto fun = functions[i - 2];

            output << getLowNBytes(2, fun.GetNameIndex());
            output << getLowNBytes(2, fun.GetParamsSize());
            output << getLowNBytes(2, fun.GetLevel());
            output << getLowNBytes(2, arr[i]);
            for (int k = 0; k < arr[i]; k++) {
                auto instruction = instructions[cnt + k + arr[1]];
                output << instruction.GetByte();
                putParameters(output, instruction);
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