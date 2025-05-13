
#include <regex>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cmath>
#include <memory>
#include <cctype>
#include <stdexcept>
#include <algorithm>

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
    std::string value;
    double numberValue;

    Token() : type(TokenType::END), value(""), numberValue(0) {}

    Token(TokenType type, const std::string& value = "", double numberValue = 0)
        : type(type), value(value), numberValue(numberValue) {}
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

// AST Node
class ASTNode {
public:
    virtual ~ASTNode() = default;
    virtual double evaluate(SymbolTable& symbols) const = 0;
    virtual std::string toString() const = 0;
};

class NumberNode : public ASTNode {
    double value;

public:
    NumberNode(double value) : value(value) {}
    double evaluate(SymbolTable&) const override { return value; }
    std::string toString() const override { return std::to_string(value); }
};

class VariableNode : public ASTNode {
    std::string name;

public:
    VariableNode(const std::string& name) : name(name) {}
    double evaluate(SymbolTable& symbols) const override { return symbols.get(name); }
    std::string toString() const override { return name; }
};

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

    std::string toString() const override {
        return "(" + left->toString() + " " + op + " " + right->toString() + ")";
    }
};

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

    std::string toString() const override { return op + operand->toString(); }
};

// Parser (basic)
class Parser {
    Lexer lexer;
    Token currentToken;

    void eat(TokenType type) {
        if (currentToken.type == type)
            currentToken = lexer.getNextToken();
        else
            throw std::runtime_error("Unexpected token: " + currentToken.value);
    }

    std::unique_ptr<ASTNode> factor() {
        if (currentToken.type == TokenType::NUMBER) {
            double val = currentToken.numberValue;
            eat(TokenType::NUMBER);
            return std::make_unique<NumberNode>(val);
        }

        if (currentToken.type == TokenType::IDENTIFIER) {
            std::string varName = currentToken.value;
            eat(TokenType::IDENTIFIER);
            return std::make_unique<VariableNode>(varName);
        }

        if (currentToken.type == TokenType::LPAREN) {
            eat(TokenType::LPAREN);
            auto node = expression();
            eat(TokenType::RPAREN);
            return node;
        }

        throw std::runtime_error("Unexpected token in factor: " + currentToken.value);
    }

    std::unique_ptr<ASTNode> term() {
        auto node = factor();
        while (currentToken.type == TokenType::MUL || currentToken.type == TokenType::DIV) {
            std::string op = currentToken.type == TokenType::MUL ? "*" : "/";
            eat(currentToken.type);
            node = std::make_unique<BinaryOpNode>(op, std::move(node), factor());
        }
        return node;
    }

    std::unique_ptr<ASTNode> expression() {
        auto node = term();
        while (currentToken.type == TokenType::PLUS || currentToken.type == TokenType::MINUS) {
            std::string op = currentToken.type == TokenType::PLUS ? "+" : "-";
            eat(currentToken.type);
            node = std::make_unique<BinaryOpNode>(op, std::move(node), term());
        }
        return node;
    }

public:
    Parser(Lexer lexer) : lexer(lexer), currentToken(lexer.getNextToken()) {}
    std::unique_ptr<ASTNode> parse() { return expression(); }
};

// Helper function to calculate determinant for 2x2 and 3x3 matrices
double determinant(const std::vector<std::vector<double>>& matrix) {
    if (matrix.size() == 2) {
        return matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
    } else if (matrix.size() == 3) {
        return matrix[0][0] * (matrix[1][1] * matrix[2][2] - matrix[1][2] * matrix[2][1]) -
               matrix[0][1] * (matrix[1][0] * matrix[2][2] - matrix[1][2] * matrix[2][0]) +
               matrix[0][2] * (matrix[1][0] * matrix[2][1] - matrix[1][1] * matrix[2][0]);
    }
    throw std::runtime_error("Unsupported matrix size for determinant calculation.");
}

// Function to solve the system using Cramer's Rule for 2-variable or 3-variable systems
std::vector<double> solveSystem(const std::vector<std::vector<double>>& coefficients,
                                const std::vector<double>& constants) {
    double det = determinant(coefficients);

    if (det == 0) {
        throw std::runtime_error("No unique solution exists (determinant is zero).");
    }

    std::vector<std::vector<double>> Dx = coefficients;
    std::vector<std::vector<double>> Dy = coefficients;
    std::vector<std::vector<double>> Dz = coefficients;

    for (int i = 0; i < coefficients.size(); ++i) {
        Dx[i][0] = constants[i];
        Dy[i][1] = constants[i];
        Dz[i][2] = constants[i];
    }

    double Dx_val = determinant(Dx);
    double Dy_val = determinant(Dy);
    double Dz_val = determinant(Dz);

    return {Dx_val / det, Dy_val / det, Dz_val / det};
}

// Function to parse an equation like "2x + 3y = 13"
bool parseEquation(const std::string& equation, std::vector<double>& coeffs, double& constant) {
    coeffs = {0, 0, 0}; // Default: x, y, z = 0

    // Remove all spaces
    std::string expr;
    for (char ch : equation) {
        if (ch != ' ') expr += ch;
    }

    // Find position of '='
    size_t eqPos = expr.find('=');
    if (eqPos == std::string::npos) return false;

    std::string lhs = expr.substr(0, eqPos);
    std::string rhs = expr.substr(eqPos + 1);

    // Extract constant on RHS
    try {
        constant = std::stod(rhs);
    } catch (...) {
        return false;
    }

    // Regex to extract terms like 2x, -y, +3.5z
    std::regex termRegex(R"(([+-]?[\d\.]*)([xyz]))");
    auto begin = std::sregex_iterator(lhs.begin(), lhs.end(), termRegex);
    auto end = std::sregex_iterator();

    for (auto it = begin; it != end; ++it) {
        std::string coeffStr = (*it)[1];
        char var = (*it)[2].str()[0];

        double coeff = 1;
        if (!coeffStr.empty() && coeffStr != "+" && coeffStr != "-")
            coeff = std::stod(coeffStr);
        else if (coeffStr == "-")
            coeff = -1;

        switch (var) {
            case 'x': coeffs[0] = coeff; break;
            case 'y': coeffs[1] = coeff; break;
            case 'z': coeffs[2] = coeff; break;
        }
    }

    return true;
}


// Main function
int main() {
    std::string input;
    std::cout << "Enter expressions or 2/3 linear equations separated by commas (e.g., 2x + 3y = 13, 3x - y = 5): ";
    std::getline(std::cin, input);

    // Parse the input into equations
    std::vector<std::string> equations;
    std::stringstream ss(input);
    std::string equation;
    while (std::getline(ss, equation, ',')) {
        equations.push_back(equation);
    }

    if (equations.size() == 2 || equations.size() == 3) {
        std::vector<std::vector<double>> coeffs;
        std::vector<double> constants;

        // Parse each equation
        for (const auto& eq : equations) {
    std::vector<double> equationCoeffs;
    double constant;
    if (parseEquation(eq, equationCoeffs, constant)) {
        coeffs.push_back(equationCoeffs);
        constants.push_back(constant);
        std::cout << "Parsed equation: " << eq << std::endl;
        std::cout << "Coefficients: ";
        for (double coeff : equationCoeffs) {
            std::cout << coeff << " ";
        }
        std::cout << "Constant: " << constant << std::endl;
    } else {
        std::cerr << "Failed to parse equation: " << eq << std::endl;
    }
}


        // Solve using the helper function
        try {
            std::vector<double> solution = solveSystem(coeffs, constants);

            std::cout << "Solution: " << std::endl;
            for (size_t i = 0; i < solution.size(); ++i) {
                std::cout << "Variable " << (char)('x' + i) << " = " << solution[i] << std::endl;
            }
        } catch (const std::runtime_error& e) {
            std::cerr << "Error: " << e.what() << std::endl;
        }
    } else {
        std::cerr << "Error: Please enter exactly 2 or 3 equations." << std::endl;
    }

    return 0;
}
