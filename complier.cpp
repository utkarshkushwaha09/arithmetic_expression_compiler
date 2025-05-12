#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <cmath>
#include <sstream>
#include <cctype>
#include <stdexcept>
#include <unordered_map>

// ---------------------------- TOKENIZER ----------------------------

enum class TokenType {
    NUMBER, IDENTIFIER,
    PLUS, MINUS, MULTIPLY, DIVIDE,
    LPAREN, RPAREN, COMMA, END,
    SIN, COS, TAN, LOG, SQRT, POW
};

struct Token {
    TokenType type;
    double value = 0;
    std::string text;

    Token(TokenType type) : type(type) {}
    Token(TokenType type, double value) : type(type), value(value) {}
    Token(TokenType type, const std::string& text)
        : type(type), text(text) {}
};

class Lexer {
    std::string text;
    size_t pos = 0;
    char currentChar;

public:
    Lexer(const std::string& input) : text(input) {
        currentChar = pos < text.length() ? text[pos] : '\0';
    }

    void advance() {
        pos++;
        currentChar = (pos < text.length()) ? text[pos] : '\0';
    }

    void skipWhitespace() {
        while (std::isspace(currentChar)) advance();
    }

    double number() {
        std::string result;
        while (std::isdigit(currentChar) || currentChar == '.') {
            result += currentChar;
            advance();
        }
        return std::stod(result);
    }

    std::string identifier() {
        std::string result;
        while (std::isalnum(currentChar) || currentChar == '_') {
            result += currentChar;
            advance();
        }
        return result;
    }

    Token getNextToken() {
        while (currentChar != '\0') {
            if (std::isspace(currentChar)) {
                skipWhitespace();
                continue;
            }

            if (std::isdigit(currentChar) || currentChar == '.') {
                return Token(TokenType::NUMBER, number());
            }

            if (std::isalpha(currentChar)) {
                std::string id = identifier();
                if (id == "sin") return Token(TokenType::SIN, id);
                if (id == "cos") return Token(TokenType::COS, id);
                if (id == "tan") return Token(TokenType::TAN, id);
                if (id == "log") return Token(TokenType::LOG, id);
                if (id == "sqrt") return Token(TokenType::SQRT, id);
                if (id == "pow") return Token(TokenType::POW, id);
                return Token(TokenType::IDENTIFIER, id);
            }

            switch (currentChar) {
                case '+': advance(); return Token(TokenType::PLUS);
                case '-': advance(); return Token(TokenType::MINUS);
                case '*': advance(); return Token(TokenType::MULTIPLY);
                case '/': advance(); return Token(TokenType::DIVIDE);
                case '(': advance(); return Token(TokenType::LPAREN);
                case ')': advance(); return Token(TokenType::RPAREN);
                case ',': advance(); return Token(TokenType::COMMA);
                default:
                    throw std::runtime_error(std::string("Unknown character: ") + currentChar);
            }
        }
        return Token(TokenType::END);
    }
};

// ---------------------------- SYMBOL TABLE ----------------------------

class SymbolTable {
    std::unordered_map<std::string, double> table;

public:
    void set(const std::string& name, double value) {
        table[name] = value;
    }

    double get(const std::string& name) const {
        if (table.find(name) == table.end()) {
            throw std::runtime_error("Variable not defined: " + name);
        }
        return table.at(name);
    }

    void print() const {
        std::cout << "\n--- SYMBOL TABLE ---\n";
        for (const auto& pair : table) {
            std::cout << pair.first << " = " << pair.second << "\n";
        }
    }
};

// ---------------------------- TAC ----------------------------

struct TACInstruction {
    std::string result;
    std::string op;
    std::string arg1;
    std::string arg2;

    std::string toString() const {
        if (op == "number") return result + " = " + arg1;
        if (op == "call") return result + " = " + arg1 + "(" + arg2 + ")";
        return result + " = " + arg1 + " " + op + " " + arg2;
    }
};

int tempVarCounter = 0;
std::string newTemp() {
    return "t" + std::to_string(tempVarCounter++);
}

// ---------------------------- AST ----------------------------

class ASTNode {
public:
    virtual double evaluate(SymbolTable& symbolTable) const = 0;
    virtual std::string generateTAC(std::vector<TACInstruction>& code) const = 0;
    virtual std::string toString() const = 0;
    virtual ~ASTNode() = default;
};

class NumberNode : public ASTNode {
    double value;
public:
    NumberNode(double value) : value(value) {}
    double evaluate(SymbolTable&) const override { return value; }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string temp = newTemp();
        code.push_back({temp, "number", std::to_string(value), ""});
        return temp;
    }

    std::string toString() const override {
        return std::to_string(value);
    }
};

class IdentifierNode : public ASTNode {
    std::string name;
public:
    IdentifierNode(const std::string& name) : name(name) {}
    double evaluate(SymbolTable& symbolTable) const override {
        return symbolTable.get(name);
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string temp = newTemp();
        code.push_back({temp, "number", name, ""});
        return temp;
    }

    std::string toString() const override {
        return name;
    }
};

class BinaryOpNode : public ASTNode {
    std::unique_ptr<ASTNode> left, right;
    TokenType op;
public:
    BinaryOpNode(std::unique_ptr<ASTNode> left, TokenType op, std::unique_ptr<ASTNode> right)
        : left(std::move(left)), op(op), right(std::move(right)) {}

    double evaluate(SymbolTable& symbolTable) const override {
        double l = left->evaluate(symbolTable);
        double r = right->evaluate(symbolTable);
        switch (op) {
            case TokenType::PLUS: return l + r;
            case TokenType::MINUS: return l - r;
            case TokenType::MULTIPLY: return l * r;
            case TokenType::DIVIDE: return l / r;
            default: throw std::runtime_error("Invalid operator");
        }
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string l = left->generateTAC(code);
        std::string r = right->generateTAC(code);
        std::string temp = newTemp();
        std::string opStr = (op == TokenType::PLUS) ? "+" : (op == TokenType::MINUS) ? "-" :
                            (op == TokenType::MULTIPLY) ? "*" : "/";
        code.push_back({temp, opStr, l, r});
        return temp;
    }

    std::string toString() const override {
        std::string opStr = (op == TokenType::PLUS) ? "+" : (op == TokenType::MINUS) ? "-" :
                            (op == TokenType::MULTIPLY) ? "*" : "/";
        return "(" + left->toString() + " " + opStr + " " + right->toString() + ")";
    }
};
class UnaryOpNode : public ASTNode {
    TokenType op;
    std::unique_ptr<ASTNode> operand;
public:
    UnaryOpNode(TokenType op, std::unique_ptr<ASTNode> operand)
        : op(op), operand(std::move(operand)) {}

    double evaluate(SymbolTable& symbolTable) const override {
        double val = operand->evaluate(symbolTable);
        if (op == TokenType::PLUS) return +val;
        if (op == TokenType::MINUS) return -val;
        throw std::runtime_error("Invalid unary operator");
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string var = operand->generateTAC(code);
        std::string temp = newTemp();
        if (op == TokenType::PLUS) {
            code.push_back({temp, "+", "0", var});
        } else if (op == TokenType::MINUS) {
            code.push_back({temp, "-", "0", var});
        } else {
            throw std::runtime_error("Invalid unary operator");
        }
        return temp;
    }

    std::string toString() const override {
        return (op == TokenType::PLUS ? "+" : "-") + operand->toString();
    }
};


// ---------------------------- PARSER ----------------------------

class Parser {
    Lexer lexer;
    Token currentToken;

    void eat(TokenType type) {
        if (currentToken.type == type) currentToken = lexer.getNextToken();
        else throw std::runtime_error("Unexpected token");
    }

    std::unique_ptr<ASTNode> factor() {
    if (currentToken.type == TokenType::PLUS || currentToken.type == TokenType::MINUS) {
        TokenType op = currentToken.type;
        eat(op);
        return std::make_unique<UnaryOpNode>(op, factor());
    }

    if (currentToken.type == TokenType::NUMBER) {
        double val = currentToken.value;
        eat(TokenType::NUMBER);
        return std::make_unique<NumberNode>(val);
    } 
        if (currentToken.type == TokenType::NUMBER) {
            double val = currentToken.value;
            eat(TokenType::NUMBER);
            return std::make_unique<NumberNode>(val);
        } else if (currentToken.type == TokenType::IDENTIFIER) {
            std::string name = currentToken.text;
            eat(TokenType::IDENTIFIER);
            return std::make_unique<IdentifierNode>(name);
        } else if (currentToken.type == TokenType::SIN || currentToken.type == TokenType::COS ||
                   currentToken.type == TokenType::TAN || currentToken.type == TokenType::LOG ||
                   currentToken.type == TokenType::SQRT || currentToken.type == TokenType::POW) {
            TokenType funcType = currentToken.type;
            eat(funcType);
            eat(TokenType::LPAREN);
            auto arg1 = expr();
            std::unique_ptr<ASTNode> arg2 = nullptr;

            if (funcType == TokenType::POW) {
                eat(TokenType::COMMA);
                arg2 = expr();
            }

            eat(TokenType::RPAREN);

            class FunctionNode : public ASTNode {
                TokenType funcType;
                std::unique_ptr<ASTNode> arg1, arg2;
            public:
                FunctionNode(TokenType funcType, std::unique_ptr<ASTNode> arg1, std::unique_ptr<ASTNode> arg2 = nullptr)
                    : funcType(funcType), arg1(std::move(arg1)), arg2(std::move(arg2)) {}

                double evaluate(SymbolTable& symbolTable) const override {
                    double a = arg1->evaluate(symbolTable);
                    switch (funcType) {
                        case TokenType::SIN: return std::sin(a * M_PI / 180.0);
                        case TokenType::COS: return std::cos(a * M_PI / 180.0);
                        case TokenType::TAN: return std::tan(a * M_PI / 180.0);
                        case TokenType::LOG: return std::log(a);
                        case TokenType::SQRT: return std::sqrt(a);
                        case TokenType::POW: return std::pow(a, arg2->evaluate(symbolTable));
                        default: throw std::runtime_error("Unknown function");
                    }
                }

                std::string generateTAC(std::vector<TACInstruction>& code) const override {
                    std::string a1 = arg1->generateTAC(code);
                    std::string temp = newTemp();
                    if (funcType == TokenType::POW) {
                        std::string a2 = arg2->generateTAC(code);
                        code.push_back({temp, "call", "pow", a1 + ", " + a2});
                    } else {
                        std::string fn = (funcType == TokenType::SIN) ? "sin" : (funcType == TokenType::COS) ? "cos" :
                                         (funcType == TokenType::TAN) ? "tan" : (funcType == TokenType::LOG) ? "log" : "sqrt";
                        code.push_back({temp, "call", fn, a1});
                    }
                    return temp;
                }

                std::string toString() const override {
                    if (funcType == TokenType::POW)
                        return "pow(" + arg1->toString() + ", " + arg2->toString() + ")";
                    std::string fn = (funcType == TokenType::SIN) ? "sin" : (funcType == TokenType::COS) ? "cos" :
                                     (funcType == TokenType::TAN) ? "tan" : (funcType == TokenType::LOG) ? "log" : "sqrt";
                    return fn + "(" + arg1->toString() + ")";
                }
            };

            return std::make_unique<FunctionNode>(funcType, std::move(arg1), std::move(arg2));
        } else if (currentToken.type == TokenType::LPAREN) {
            eat(TokenType::LPAREN);
            auto node = expr();
            eat(TokenType::RPAREN);
            return node;
        } else {
            throw std::runtime_error("Invalid syntax in factor");
        }
    }

    std::unique_ptr<ASTNode> term() {
        auto node = factor();
        while (currentToken.type == TokenType::MULTIPLY || currentToken.type == TokenType::DIVIDE) {
            TokenType op = currentToken.type;
            eat(op);
            node = std::make_unique<BinaryOpNode>(std::move(node), op, factor());
        }
        return node;
    }

    std::unique_ptr<ASTNode> expr() {
        auto node = term();
        while (currentToken.type == TokenType::PLUS || currentToken.type == TokenType::MINUS) {
            TokenType op = currentToken.type;
            eat(op);
            node = std::make_unique<BinaryOpNode>(std::move(node), op, term());
        }
        return node;
    }

public:
    Parser(const std::string& input) : lexer(input), currentToken(lexer.getNextToken()) {}
    std::unique_ptr<ASTNode> parse() { return expr(); }
};

// ---------------------------- MAIN ----------------------------

int main() {
    std::string input;
    std::cout << "Enter an expression: ";
    std::getline(std::cin, input);

    try {
        Lexer lexer(input);
        std::cout << "\n--- TOKENIZATION ---\n";
        Token token = lexer.getNextToken();
        while (token.type != TokenType::END) {
            switch (token.type) {
                case TokenType::NUMBER: std::cout << "NUMBER: " << token.value << "\n"; break;
                case TokenType::IDENTIFIER: std::cout << "IDENTIFIER: " << token.text << "\n"; break;
                case TokenType::SIN: case TokenType::COS: case TokenType::TAN:
                case TokenType::LOG: case TokenType::SQRT: case TokenType::POW:
                    std::cout << "FUNC: " << token.text << "\n"; break;
                case TokenType::PLUS: std::cout << "PLUS\n"; break;
                case TokenType::MINUS: std::cout << "MINUS\n"; break;
                case TokenType::MULTIPLY: std::cout << "MULTIPLY\n"; break;
                case TokenType::DIVIDE: std::cout << "DIVIDE\n"; break;
                case TokenType::LPAREN: std::cout << "LPAREN\n"; break;
                case TokenType::RPAREN: std::cout << "RPAREN\n"; break;
                case TokenType::COMMA: std::cout << "COMMA\n"; break;
                default: std::cout << "UNKNOWN\n";
            }
            token = lexer.getNextToken();
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
