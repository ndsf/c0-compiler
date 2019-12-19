#include "argparse.hpp"
#include "fmt/core.h"

#include "tokenizer/tokenizer.h"
#include "analyser/analyser.h"
#include "fmts.hpp"

#include <iostream>
#include <fstream>

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

bool isJump(c0::Operation type) { // need removing
    return (type == c0::JMP || type == c0::JNE || type == c0::JE
            || type == c0::JG || type == c0::JL || type == c0::JLE || type == c0::JGE
    );
}

int32_t needParameterNum(c0::Operation type) {
    if (type == c0::LOADA)
        return 2;
    else if (type == c0::IPUSH || type == c0::LOADC || isJump(type) ||
             type == c0::CALL || type == c0::BIPUSH)
        return 1;
    else
        return 0;
}

void Analyse(std::istream &input, std::ostream &output) {
    auto tks = _tokenize(input);
    c0::Analyser analyser(tks);

    // unpack the tuple into individual variables declared at the call site
    auto[instructions, constants, functions, err] = analyser.Analyse();

    if (err.has_value()) {
        fmt::print(stderr, "Syntactic analysis error: {}\n", err.value());
        exit(2);
    }
    output << ".constants:\n";

    for (auto constant : constants) {
        switch (constant.GetType()) {
            case c0::STRING_TYPE:
                output << fmt::format("{} S \"{}\"\n", constant.GetIndex(),
                                      std::any_cast<std::string>(constant.GetValue()));
                break;
            case c0::INTEGER_TYPE:
                output << fmt::format("{} I {}\n", constant.GetIndex(),
                                      std::any_cast<std::int32_t>(constant.GetValue()));
                break;
            case c0::FLOAT_TYPE: // WIP
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
        switch (needParameterNum(instruction.GetOperation())) {
            case 1: // TODO remove this
                output << fmt::format("{} {} {}\n", instruction.GetIndex(), instruction.GetOperation(), instruction.GetX());
                break;
            case 2:
                output << fmt::format("{} {} {}, {}\n", instruction.GetIndex(), instruction.GetOperation(), instruction.GetX(),
                                      instruction.GetY());
                break;
            case 0:
                output << fmt::format("{} {}\n", instruction.GetIndex(), instruction.GetOperation());
                break;
        }
    }

//	for (auto& it : instructions)
//		output << fmt::format("{}\n", it);

    output << ".functions:\n";
    for(auto function : functions)
        output << fmt::format("{} {} {} {}\n", function.GetIndex(), function.GetNameIndex(), function.GetParamsSize(), function.GetLevel());
    sm = false;
    funcIndex = 0;
    for(int i = j; i < instructions.size(); i++){
        auto instruction = instructions[i];
        if(instructions[i].GetIndex() == 0){
            output << fmt::format(".F{}:\n", funcIndex);
            funcIndex++;
        }
        switch (needParameterNum(instruction.GetOperation())){
            case 1:
                output << fmt::format("{} {} {}\n", instruction.GetIndex(), instruction.GetOperation(), instruction.GetX());
                break;
            case 2:
                output << fmt::format("{} {} {}, {}\n", instruction.GetIndex(), instruction.GetOperation(), instruction.GetX(), instruction.GetY());
                break;
            case 0:
                output << fmt::format("{} {}\n", instruction.GetIndex(), instruction.GetOperation());
                break;
        }
        if(funcIndex > functions.size())
            break;
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
    program.add_argument("-l")
            .default_value(false)
            .implicit_value(true)
            .help("perform syntactic analysis for the input file.");
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
    if (program["-t"] == true && program["-l"] == true) {
        fmt::print(stderr, "You can only perform tokenization or syntactic analysis at one time.");
        exit(2);
    }
    if (program["-t"] == true) {
        Tokenize(*input, *output);
    } else if (program["-l"] == true) {
        Analyse(*input, *output);
    } else {
        fmt::print(stderr, "You must choose tokenization or syntactic analysis.");
        exit(2);
    }
    return 0;
}