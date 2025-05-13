#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cmath>
#include <memory>
#include <cctype>
#include <stdexcept>

// Token types
enum class TokenType {
    NUMBER, IDENTIFIER, ASSIGN,
    PLUS, MINUS, MUL, DIV,
    LPAREN, RPAREN, COMMA,
    END
};

// Token structure
struct Token {
    TokenType type;
    std::string text;
    double value;

    Token(TokenType type, const std::string& text = "", double value = 0)
        : type(type), text(text), value(value) {}
};

// Lexer
class Lexer {
    std::string input;
    size_t pos;
    char currentChar;

    void advance() {
        if (++pos < input.size()) currentChar = input[pos];
        else currentChar = '\0';
    }

    void skipWhitespace() {
        while (std::isspace(currentChar)) advance();
    }

    Token number() {
        std::string result;
        while (std::isdigit(currentChar) || currentChar == '.') {
            result += currentChar;
            advance();
        }
        return Token(TokenType::NUMBER, result, std::stod(result));
    }

    Token identifier() {
        std::string result;
        while (std::isalnum(currentChar) || currentChar == '_') {
            result += currentChar;
            advance();
        }
        return Token(TokenType::IDENTIFIER, result);
    }

public:
    Lexer(const std::string& input) : input(input), pos(0) {
        currentChar = input.empty() ? '\0' : input[pos];
    }

    Token getNextToken() {
        skipWhitespace();
        if (std::isdigit(currentChar)) return number();
        if (std::isalpha(currentChar)) return identifier();

        switch (currentChar) {
            case '+': advance(); return Token(TokenType::PLUS);
            case '-': advance(); return Token(TokenType::MINUS);
            case '*': advance(); return Token(TokenType::MUL);
            case '/': advance(); return Token(TokenType::DIV);
            case '=': advance(); return Token(TokenType::ASSIGN);
            case '(': advance(); return Token(TokenType::LPAREN);
            case ')': advance(); return Token(TokenType::RPAREN);
            case ',': advance(); return Token(TokenType::COMMA);
            case '\0': return Token(TokenType::END);
            default:
                throw std::runtime_error(std::string("Invalid character: ") + currentChar);
        }
    }
};

// Symbol table
class SymbolTable {
    std::map<std::string, double> table;

public:
    void set(const std::string& name, double value) { table[name] = value; }
    double get(const std::string& name) const {
        auto it = table.find(name);
        if (it == table.end())
            throw std::runtime_error("Undefined variable: " + name);
        return it->second;
    }

    void print() const {
for (const auto& entry : table) {
    const auto& name = entry.first;
    const auto& value = entry.second;
    std::cout << name << " = " << value << std::endl;
}

    }
};

// TAC Instruction
struct TACInstruction {
    std::string result, op, arg1, arg2;

    std::string toString() const {
        if (op == "=")
            return result + " = " + arg1;
        else if (op == "call")
            return result + " = call " + arg1 + ", " + arg2;
        else
            return result + " = " + arg1 + " " + op + " " + arg2;
    }
};

// AST Base
class ASTNode {
public:
    virtual ~ASTNode() = default;
    virtual double evaluate(SymbolTable& symbols) const = 0;
    virtual std::string generateTAC(std::vector<TACInstruction>& code) const = 0;
    virtual std::string toString() const = 0;
};

// Number
class NumberNode : public ASTNode {
    double value;

public:
    NumberNode(double value) : value(value) {}

    double evaluate(SymbolTable&) const override { return value; }

    std::string generateTAC(std::vector<TACInstruction>&) const override {
        return std::to_string(value);
    }

    std::string toString() const override {
        return std::to_string(value);
    }
};

// Variable
class VariableNode : public ASTNode {
    std::string name;

public:
    VariableNode(const std::string& name) : name(name) {}

    double evaluate(SymbolTable& symbols) const override {
        return symbols.get(name);
    }

    std::string generateTAC(std::vector<TACInstruction>&) const override {
        return name;
    }

    std::string toString() const override {
        return name;
    }
};

// Binary Operator
class BinaryOpNode : public ASTNode {
    std::string op;
    std::unique_ptr<ASTNode> left, right;

public:
    BinaryOpNode(const std::string& op, std::unique_ptr<ASTNode> left, std::unique_ptr<ASTNode> right)
        : op(op), left(std::move(left)), right(std::move(right)) {}

    double evaluate(SymbolTable& symbols) const override {
        double l = left->evaluate(symbols);
        double r = right->evaluate(symbols);
        if (op == "+") return l + r;
        if (op == "-") return l - r;
        if (op == "*") return l * r;
        if (op == "/") return l / r;
        throw std::runtime_error("Unknown operator: " + op);
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string l = left->generateTAC(code);
        std::string r = right->generateTAC(code);
        std::string temp = "t" + std::to_string(code.size());
        code.push_back({temp, op, l, r});
        return temp;
    }

    std::string toString() const override {
        return "(" + left->toString() + " " + op + " " + right->toString() + ")";
    }
};

// Unary Operator
class UnaryOpNode : public ASTNode {
    std::string op;
    std::unique_ptr<ASTNode> operand;

public:
    UnaryOpNode(const std::string& op, std::unique_ptr<ASTNode> operand)
        : op(op), operand(std::move(operand)) {}

    double evaluate(SymbolTable& symbols) const override {
        double val = operand->evaluate(symbols);
        return (op == "-") ? -val : val;
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string val = operand->generateTAC(code);
        std::string temp = "t" + std::to_string(code.size());
        code.push_back({temp, op, "0", val});
        return temp;
    }

    std::string toString() const override {
        return op + operand->toString();
    }
};

// Function Call
class FunctionNode : public ASTNode {
    std::string name;
    std::vector<std::unique_ptr<ASTNode>> args;

public:
    FunctionNode(const std::string& name, std::vector<std::unique_ptr<ASTNode>> args)
        : name(name), args(std::move(args)) {}

    double evaluate(SymbolTable& symbols) const override {
        if (name == "sin") return std::sin(args[0]->evaluate(symbols));
        if (name == "cos") return std::cos(args[0]->evaluate(symbols));
        if (name == "tan") return std::tan(args[0]->evaluate(symbols));
        if (name == "log") return std::log(args[0]->evaluate(symbols));
        if (name == "sqrt") return std::sqrt(args[0]->evaluate(symbols));
        if (name == "pow") return std::pow(args[0]->evaluate(symbols), args[1]->evaluate(symbols));
        throw std::runtime_error("Unknown function: " + name);
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::vector<std::string> argNames;
        for (const auto& arg : args)
            argNames.push_back(arg->generateTAC(code));

        std::string temp = "t" + std::to_string(code.size());
        std::string argsStr = argNames.size() == 2 ? argNames[1] : "";
        code.push_back({temp, "call", name + "(" + argNames[0] + ")", argsStr});
        return temp;
    }

    std::string toString() const override {
        std::string res = name + "(";
        for (size_t i = 0; i < args.size(); ++i) {
            res += args[i]->toString();
            if (i + 1 < args.size()) res += ", ";
        }
        return res + ")";
    }
};

// Assignment
class AssignmentNode : public ASTNode {
    std::string varName;
    std::unique_ptr<ASTNode> expr;

public:
    AssignmentNode(const std::string& name, std::unique_ptr<ASTNode> expr)
        : varName(name), expr(std::move(expr)) {}

    double evaluate(SymbolTable& symbols) const override {
        double val = expr->evaluate(symbols);
        symbols.set(varName, val);
        return val;
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string rhs = expr->generateTAC(code);
        code.push_back({varName, "=", rhs, ""});
        return varName;
    }

    std::string toString() const override {
        return varName + " = " + expr->toString();
    }
};

// Parser
class Parser {
    Lexer lexer;
    Token currentToken;

    void eat(TokenType type) {
        if (currentToken.type == type)
            currentToken = lexer.getNextToken();
        else
            throw std::runtime_error("Unexpected token: " + currentToken.text);
    }

    std::unique_ptr<ASTNode> factor() {
        if (currentToken.type == TokenType::PLUS) {
            eat(TokenType::PLUS);
            return factor();
        }
        if (currentToken.type == TokenType::MINUS) {
            eat(TokenType::MINUS);
            return std::make_unique<UnaryOpNode>("-", factor());
        }

        if (currentToken.type == TokenType::NUMBER) {
            double val = currentToken.value;
            eat(TokenType::NUMBER);
            return std::make_unique<NumberNode>(val);
        }

        if (currentToken.type == TokenType::IDENTIFIER) {
            std::string name = currentToken.text;
            eat(TokenType::IDENTIFIER);

            if (currentToken.type == TokenType::LPAREN) {
                eat(TokenType::LPAREN);
                std::vector<std::unique_ptr<ASTNode>> args;
                if (currentToken.type != TokenType::RPAREN) {
                    args.push_back(expr());
                    while (currentToken.type == TokenType::COMMA) {
                        eat(TokenType::COMMA);
                        args.push_back(expr());
                    }
                }
                eat(TokenType::RPAREN);
                return std::make_unique<FunctionNode>(name, std::move(args));
            } else {
                return std::make_unique<VariableNode>(name);
            }
        }

        if (currentToken.type == TokenType::LPAREN) {
            eat(TokenType::LPAREN);
            auto node = expr();
            eat(TokenType::RPAREN);
            return node;
        }

        throw std::runtime_error("Unexpected token in factor: " + currentToken.text);
    }

    std::unique_ptr<ASTNode> term() {
        auto node = factor();
        while (currentToken.type == TokenType::MUL || currentToken.type == TokenType::DIV) {
            std::string op = (currentToken.type == TokenType::MUL) ? "*" : "/";
            eat(currentToken.type);
            node = std::make_unique<BinaryOpNode>(op, std::move(node), factor());
        }
        return node;
    }

    std::unique_ptr<ASTNode> expr() {
        auto node = term();
        while (currentToken.type == TokenType::PLUS || currentToken.type == TokenType::MINUS) {
            std::string op = (currentToken.type == TokenType::PLUS) ? "+" : "-";
            eat(currentToken.type);
            node = std::make_unique<BinaryOpNode>(op, std::move(node), term());
        }
        return node;
    }

public:
    Parser(const std::string& input) : lexer(input), currentToken(lexer.getNextToken()) {}

    std::unique_ptr<ASTNode> parse() {
        if (currentToken.type == TokenType::IDENTIFIER) {
            std::string name = currentToken.text;
            eat(TokenType::IDENTIFIER);
            if (currentToken.type == TokenType::ASSIGN) {
                eat(TokenType::ASSIGN);
                return std::make_unique<AssignmentNode>(name, expr());
            } else {
                throw std::runtime_error("Expected '=' after identifier");
            }
        }
        return expr();
    }
};

// Main

int main() {
    std::string input;
    std::cout << "Enter an expression: ";
    std::getline(std::cin, input);

    try {
       // Token debug: Print all tokens
Lexer debugLexer(input);
std::cout << "Tokens:" << std::endl;
while (true) {
    Token tok = debugLexer.getNextToken();
    std::cout << "  Type: ";

    switch (tok.type) {
        case TokenType::NUMBER: std::cout << "NUMBER"; break;
        case TokenType::IDENTIFIER: std::cout << "IDENTIFIER"; break;
        case TokenType::ASSIGN: std::cout << "ASSIGN"; break;
        case TokenType::PLUS: std::cout << "PLUS"; break;
        case TokenType::MINUS: std::cout << "MINUS"; break;
        case TokenType::MUL: std::cout << "MUL"; break;
        case TokenType::DIV: std::cout << "DIV"; break;
        case TokenType::LPAREN: std::cout << "LPAREN"; break;
        case TokenType::RPAREN: std::cout << "RPAREN"; break;
        case TokenType::COMMA: std::cout << "COMMA"; break;
        case TokenType::END: std::cout << "END"; break;
    }

    std::cout << ", Text: \"" << tok.text << "\"";
    if (tok.type == TokenType::NUMBER) {
        std::cout << ", Value: " << tok.value;
    }
    std::cout << std::endl;

    if (tok.type == TokenType::END) break;
}

        Parser parser(input);
        std::cout << "\n--- PARSING ---\n";
        auto root = parser.parse();

        std::cout << "\n--- PARSE TREE ---\n";
        std::cout << root->toString() << "\n";

        SymbolTable symbols;
        std::cout << "\n--- EVALUATION ---\n";
        double result = root->evaluate(symbols);
        std::cout << "Result: " << result << "\n";
        symbols.print();

        std::cout << "\n--- THREE-ADDRESS CODE (TAC) ---\n";
        std::vector<TACInstruction> code;
        std::string finalResult = root->generateTAC(code);
        for (const auto& instr : code)
            std::cout << instr.toString() << "\n";
        std::cout << "Final result stored in: " << finalResult << "\n";

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
    }

    return 0;
}
