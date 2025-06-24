#include <gtkmm.h>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <cctype>
#include <cmath>
#include <memory>
#include <stdexcept>
#include <sstream>
#include <fstream>      
#include <cstdlib>      
#include <algorithm>    
#include <glibmm/fileutils.h> 

// A exception class
class ParsingError : public std::runtime_error {
public:
    ParsingError(const std::string& message, size_t pos)
        : std::runtime_error(message + " near position " + std::to_string(pos)) {}
    explicit ParsingError(const std::string& message)
        : std::runtime_error(message) {}
};


// ────────── Token Definitions ──────────
enum class TokenType {
    NUMBER, IDENTIFIER, FUNCTION,
    ASSIGN,
    PLUS, MINUS, MUL, DIV, POW,
    LPAREN, RPAREN, COMMA,
    EQUAL, NOT_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL,
    LOGICAL_AND, LOGICAL_OR, LOGICAL_NOT,
    END
};

struct Token {
    TokenType   type;
    std::string text;
    double      value; 

    Token() : type(TokenType::END), text(), value(0) {}
    Token(TokenType t, const std::string& txt, double v = 0)
      : type(t), text(txt), value(v) {}
};

// ────────── Lexer ──────────
class Lexer {
    std::string input;
    size_t      pos = 0;
    char        ch  = '\0';

    void advance() {
        if (++pos < input.size()) ch = input[pos];
        else                       ch = '\0';
    }

    char peek() {
        if (pos + 1 < input.size()) return input[pos + 1];
        return '\0';
    }

    void skipWhitespace() {
        while (std::isspace(static_cast<unsigned char>(ch))) advance();
    }

    Token parseNumber() {
        std::string buf;
        while (std::isdigit(static_cast<unsigned char>(ch)) || ch == '.') {
            buf.push_back(ch);
            advance();
        }
        return Token(TokenType::NUMBER, buf, std::stod(buf));
    }

    Token parseIdentifierOrFunction() {
        std::string buf;
        while (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
            buf.push_back(ch);
            advance();
        }
        if (buf=="sin"||buf=="cos"||buf=="tan"||buf=="sqrt"||buf=="pow"||
            buf=="log"||buf=="log10"||buf=="exp"||buf=="abs"||
            buf=="floor"||buf=="ceil"||buf=="min"||buf=="max")
            return Token(TokenType::FUNCTION, buf);
        return Token(TokenType::IDENTIFIER, buf);
    }

public:
    Lexer(const std::string& in) : input(in) {
        ch = input.empty() ? '\0' : input[0];
    }

    Token getNextToken() {
        skipWhitespace();
        if (std::isdigit(static_cast<unsigned char>(ch)) || ch=='.') {
            return parseNumber();
        }
        if (std::isalpha(static_cast<unsigned char>(ch))) {
            return parseIdentifierOrFunction();
        }
        switch (ch) {
          case '+': advance(); return Token(TokenType::PLUS, "+");
          case '-': advance(); return Token(TokenType::MINUS, "-");
          case '*': advance(); return Token(TokenType::MUL, "*");
          case '/': advance(); return Token(TokenType::DIV, "/");
          case '^': advance(); return Token(TokenType::POW, "^");
          case '(': advance(); return Token(TokenType::LPAREN, "(");
          case ')': advance(); return Token(TokenType::RPAREN, ")");
          case ',': advance(); return Token(TokenType::COMMA, ",");
          case '!':
              if (peek() == '=') { advance(); advance(); return Token(TokenType::NOT_EQUAL, "!="); }
              advance(); return Token(TokenType::LOGICAL_NOT, "!");
          case '=':
              if (peek() == '=') { advance(); advance(); return Token(TokenType::EQUAL, "=="); }
              advance(); return Token(TokenType::ASSIGN, "=");
          case '<':
              if (peek() == '=') { advance(); advance(); return Token(TokenType::LESS_EQUAL, "<="); }
              advance(); return Token(TokenType::LESS, "<");
          case '>':
              if (peek() == '=') { advance(); advance(); return Token(TokenType::GREATER_EQUAL, ">="); }
              advance(); return Token(TokenType::GREATER, ">");
          case '&':
              if (peek() == '&') { advance(); advance(); return Token(TokenType::LOGICAL_AND, "&&"); }
              break;
          case '|':
              if (peek() == '|') { advance(); advance(); return Token(TokenType::LOGICAL_OR, "||"); }
              break;
          case '\0': return Token(TokenType::END, "");
        }
        throw ParsingError(std::string("Invalid character: '") + ch + "'", pos);
    }
    size_t getPosition() const { return pos; }
};

struct TACInstruction {
    std::string result, op, arg1, arg2;
    std::string toString() const {
        if (op=="call") {
            if (arg2.empty())
                return result+" = call "+arg1;
            else
                return result+" = call "+arg1+", "+arg2;
        }
        if (op=="=") return result+" = "+arg1;
        if (arg2.empty()) return result + " = " + op + arg1; 
        return result+" = "+arg1+" "+op+" "+arg2;
    }
};

class SymbolTable {
    std::map<std::string,double> tbl;
public:
    void set(const std::string& name,double v){ tbl[name]=v; }
    double get(const std::string& name)const {
        auto it=tbl.find(name);
        if(it==tbl.end()) throw ParsingError("Undefined variable: "+name);
        return it->second;
    }
    void clear() { tbl.clear(); }
    const std::map<std::string, double>& data() const { return tbl; }
};
// ────────── AST Nodes ──────────
class ASTNode {
public:
    virtual ~ASTNode() {}
    virtual double evaluate(SymbolTable&, std::vector<std::string>& eval_log) const = 0;
    virtual std::string generateTAC(std::vector<TACInstruction>&) const = 0;
    virtual std::string toString() const = 0;
    virtual std::vector<const ASTNode*> children() const = 0;
    virtual std::unique_ptr<ASTNode> simplify() = 0;
};

class NumberNode : public ASTNode {
    double      val;
    std::string lit;
public:
    NumberNode(double v,const std::string& txt="")
      : val(v), lit(txt.empty()?std::to_string(v):txt) {}
    double evaluate(SymbolTable&, std::vector<std::string>& eval_log) const override {
        eval_log.push_back("Evaluating NumberNode: " + lit + " -> " + std::to_string(val));
        return val;
    }
    double getValue() const { return val; }
    std::string generateTAC(std::vector<TACInstruction>&) const override { return lit; }
    std::string toString() const override { return lit; }
    std::unique_ptr<ASTNode> simplify() override { return std::make_unique<NumberNode>(val,lit); }
    std::vector<const ASTNode*> children() const override { return {}; }
};

class SequenceNode : public ASTNode {
    std::vector<std::unique_ptr<ASTNode>> exprs;
public:
    SequenceNode(std::vector<std::unique_ptr<ASTNode>> e) : exprs(std::move(e)) {}
    double evaluate(SymbolTable& S, std::vector<std::string>& eval_log) const override {
        double result = 0;
        eval_log.push_back("Starting SequenceNode evaluation.");
        for (size_t i = 0; i < exprs.size(); ++i) {
            eval_log.push_back("  Evaluating expression " + std::to_string(i + 1) + " in sequence.");
            result = exprs[i]->evaluate(S, eval_log);
            if (i < exprs.size() - 1) {
                eval_log.push_back("  Result of expression " + std::to_string(i + 1) + ": " + std::to_string(result));
            }
        }
        eval_log.push_back("Finished SequenceNode evaluation. Final result: " + std::to_string(result));
        return result;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string result;
        for (const auto& expr : exprs) result = expr->generateTAC(code);
        return result;
    }
    std::string toString() const override {
        std::string s;
        for (size_t i = 0; i < exprs.size(); ++i) {
            s += exprs[i]->toString();
            if (i + 1 < exprs.size()) s += ", ";
        }
        return s;
    }
    std::unique_ptr<ASTNode> simplify() override {
        std::vector<std::unique_ptr<ASTNode>> simplified;
        bool changed = false;
        for (auto& e : exprs) {
            auto simplified_e = e->simplify();
            if (simplified_e.get() != e.get()) changed = true; 
            simplified.push_back(std::move(simplified_e));
        }
        return std::make_unique<SequenceNode>(std::move(simplified));
    }
    std::vector<const ASTNode*> children() const override {
        std::vector<const ASTNode*> out;
        for (const auto& e : exprs) out.push_back(e.get());
        return out;
    }
};

class VariableNode : public ASTNode {
    std::string name;
public:
    VariableNode(const std::string& n): name(n) {}
    double evaluate(SymbolTable& S, std::vector<std::string>& eval_log) const override {
        double v = S.get(name);
        eval_log.push_back("Evaluating VariableNode: Looking up variable '" + name + "' -> " + std::to_string(v));
        return v;
    }
    std::string generateTAC(std::vector<TACInstruction>&) const override { return name; }
    std::string toString() const override { return name; }
    std::unique_ptr<ASTNode> simplify() override { return std::make_unique<VariableNode>(name); }
    std::vector<const ASTNode*> children() const override { return {}; }
};

class UnaryOpNode : public ASTNode {
    std::string op;
    std::unique_ptr<ASTNode> operand;
public:
    UnaryOpNode(const std::string& o,std::unique_ptr<ASTNode> c)
      : op(o), operand(std::move(c)) {}

    double evaluate(SymbolTable& S, std::vector<std::string>& eval_log) const override {
        eval_log.push_back("Evaluating UnaryOpNode: " + op + operand->toString());
        double v = operand->evaluate(S, eval_log);
        double result;
        if (op == "-") {
            result = -v;
            eval_log.push_back("  Applying unary minus to " + std::to_string(v) + " -> " + std::to_string(result));
        } else if (op == "!") {
            result = (v == 0.0) ? 1.0 : 0.0; 
            eval_log.push_back("  Applying logical NOT to " + std::to_string(v) + " -> " + std::to_string(result));
        } else {
            result = v; 
            eval_log.push_back("  Applying unary plus (noop) to " + std::to_string(v) + " -> " + std::to_string(result));
        }
        return result;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string a = operand->generateTAC(code);
        std::string tmp="t"+std::to_string(code.size());
        if (op == "-") { 
            code.push_back({tmp,op,"0",a}); 
        } else { 
             code.push_back({tmp,op,a,""});
        }
        return tmp;
    }
    std::string toString() const override {
        return op + operand->toString();
    }

    std::unique_ptr<ASTNode> simplify() override {
        auto simplified_operand = operand->simplify();

        if (op == "+") { 
            return simplified_operand;
        }

        return std::make_unique<UnaryOpNode>(op, std::move(simplified_operand));
    }
    std::vector<const ASTNode*> children() const override { return { operand.get() }; }
};

class BinaryOpNode : public ASTNode {
    std::string op;
    std::unique_ptr<ASTNode> left, right;
public:
    BinaryOpNode(const std::string& o, std::unique_ptr<ASTNode> l, std::unique_ptr<ASTNode> r)
      : op(o), left(std::move(l)), right(std::move(r)) {}

    double evaluate(SymbolTable& S, std::vector<std::string>& eval_log) const override {
        eval_log.push_back("Evaluating BinaryOpNode: " + left->toString() + " " + op + " " + right->toString());
        double L = left->evaluate(S, eval_log);
        double R = right->evaluate(S, eval_log);
        double result;
        if (op == "+") { result = L + R; }
        else if (op == "-") { result = L - R; }
        else if (op == "*") { result = L * R; }
        else if (op == "/") {
            if (R == 0) throw ParsingError("Division by zero"); // Runtime division by zero
            result = L / R;
        }
        else if (op == "^") { result = std::pow(L, R); }
        else if (op == "==") { result = (L == R) ? 1.0 : 0.0; }
        else if (op == "!=") { result = (L != R) ? 1.0 : 0.0; }
        else if (op == "<")  { result = (L < R)  ? 1.0 : 0.0; }
        else if (op == "<=") { result = (L <= R) ? 1.0 : 0.0; }
        else if (op == ">")  { result = (L > R)  ? 1.0 : 0.0; }
        else if (op == ">=") { result = (L >= R) ? 1.0 : 0.0; }
        else if (op == "&&") { result = (L != 0.0 && R != 0.0) ? 1.0 : 0.0; }
        else if (op == "||") { result = (L != 0.0 || R != 0.0) ? 1.0 : 0.0; }
        else throw ParsingError("Unknown binary operator: " + op);

        eval_log.push_back("  Calculated " + std::to_string(L) + " " + op + " " + std::to_string(R) + " -> " + std::to_string(result));
        return result;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string a=left->generateTAC(code);
        std::string b=right->generateTAC(code);
        std::string tmp="t"+std::to_string(code.size());
        code.push_back({tmp,op,a,b});
        return tmp;
    }
    std::string toString() const override { return "("+left->toString()+" "+op+" "+right->toString()+")"; }
    std::unique_ptr<ASTNode> simplify() override {
        auto simplified_left = left->simplify();
        auto simplified_right = right->simplify();

        NumberNode* num_left = dynamic_cast<NumberNode*>(simplified_left.get());
        NumberNode* num_right = dynamic_cast<NumberNode*>(simplified_right.get());

        // Algebraic Identities & Specific Error Checks
        if (op == "+") {
            if (num_left && num_left->getValue() == 0.0) return simplified_right; 
            if (num_right && num_right->getValue() == 0.0) return simplified_left; 
        } else if (op == "-") {
            if (num_right && num_right->getValue() == 0.0) return simplified_left; 
        } else if (op == "*") {
            // Check for 0 first as it's an absorbing element.
            if (num_left && num_left->getValue() == 0.0) return std::make_unique<NumberNode>(0.0, "0");
            if (num_right && num_right->getValue() == 0.0) return std::make_unique<NumberNode>(0.0, "0"); 
            
            // Check for 1 as it's an identity element.
            if (num_left && num_left->getValue() == 1.0) return simplified_right; 
            if (num_right && num_right->getValue() == 1.0) return simplified_left;
        } else if (op == "/") {
            // Check for division  zero.
            if (num_right && num_right->getValue() == 0.0) {
                throw ParsingError("Division by constant zero: " + simplified_left->toString() + " / " + num_right->toString());
            }
            if (num_right && num_right->getValue() == 1.0) return simplified_left; 
        }
        return std::make_unique<BinaryOpNode>(op, std::move(simplified_left), std::move(simplified_right));
    }
    std::vector<const ASTNode*> children() const override { return { left.get(), right.get() }; }
};

class FunctionNode : public ASTNode {
    std::string name;
    std::vector<std::unique_ptr<ASTNode>> args;
public:
    FunctionNode(const std::string& n, std::vector<std::unique_ptr<ASTNode>> a)
      : name(n), args(std::move(a)) {}

    double evaluate(SymbolTable& S, std::vector<std::string>& eval_log) const override {
        eval_log.push_back("Evaluating FunctionNode: " + name + "(");
        std::vector<double> vals;
        std::string arg_values_str;
        for(size_t i = 0; i < args.size(); ++i) {
            eval_log.push_back("  Evaluating argument " + std::to_string(i + 1) + " for function " + name);
            double arg_val = args[i]->evaluate(S, eval_log);
            vals.push_back(arg_val);
            arg_values_str += std::to_string(arg_val);
            if (i < args.size() - 1) arg_values_str += ", ";
        }
        eval_log.push_back("  Evaluated arguments: " + arg_values_str + ")");

        double result;
        if      (name=="sin")   result = std::sin(vals[0]);
        else if (name=="cos")   result = std::cos(vals[0]);
        else if (name=="tan")   result = std::tan(vals[0]);
        else if (name=="sqrt")  result = std::sqrt(vals[0]);
        else if (name=="log")   result = std::log(vals[0]);
        else if (name=="log10") result = std::log10(vals[0]);
        else if (name=="exp")   result = std::exp(vals[0]);
        else if (name=="abs")   result = std::abs(vals[0]);
        else if (name=="floor") result = std::floor(vals[0]);
        else if (name=="ceil")  result = std::ceil(vals[0]);
        else if (name=="pow")   result = std::pow(vals[0],vals[1]);
        else if (name=="min")   result = std::min(vals[0],vals[1]);
        else if (name=="max")   result = std::max(vals[0],vals[1]);
        else throw ParsingError("Unknown function: "+name);

        eval_log.push_back("  Function '" + name + "' called with (" + arg_values_str + ") -> " + std::to_string(result));
        return result;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string result_arg1;
        std::string result_arg2; 
        std::string args_for_call;

        if (!args.empty()) {
            args_for_call = args[0]->generateTAC(code);
            if (args.size() > 1) { 
                std::string arg2_temp = args[1]->generateTAC(code);
                args_for_call += ", " + arg2_temp; 
            }
        }
        
        std::string tmp = "t" + std::to_string(code.size());
        code.push_back({tmp, "call", name, args_for_call}); 
        return tmp;
    }
    std::string toString() const override {
        std::string s = name + "(";
        for (size_t i = 0; i < args.size(); ++i) {
            s += args[i]->toString();
            if (i + 1 < args.size()) s += ", ";
        }
        s += ")";
        return s;
    }

    std::unique_ptr<ASTNode> simplify() override {
        std::vector<std::unique_ptr<ASTNode>> sargs;
        bool changed = false;
        for(auto& a: args) {
            auto simplified_arg = a->simplify();
            if (simplified_arg.get() != a.get()) changed = true;
            sargs.push_back(std::move(simplified_arg));
        }
        return std::make_unique<FunctionNode>(name, std::move(sargs));
    }
    std::vector<const ASTNode*> children() const override {
        std::vector<const ASTNode*> ptrs;
        for(auto& a: args) ptrs.push_back(a.get());
        return ptrs;
    }
};

class AssignmentNode : public ASTNode {
    std::string var;
    std::unique_ptr<ASTNode> expr;
public:
    AssignmentNode(const std::string& v,std::unique_ptr<ASTNode> e) : var(v), expr(std::move(e)) {}
    double evaluate(SymbolTable& S, std::vector<std::string>& eval_log) const override {
        eval_log.push_back("Evaluating AssignmentNode: " + var + " = " + expr->toString());
        double v = expr->evaluate(S, eval_log);
        S.set(var,v);
        eval_log.push_back("  Assigned value " + std::to_string(v) + " to variable '" + var + "'");
        return v;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        auto r=expr->generateTAC(code);
        code.push_back({var,"=",r,""});
        return var;
    }
    std::string toString() const override { return var+" = "+expr->toString(); }
    std::unique_ptr<ASTNode> simplify() override {
        auto simplified_expr = expr->simplify();
        return std::make_unique<AssignmentNode>(var, std::move(simplified_expr));
    }
    std::vector<const ASTNode*> children() const override { return { expr.get() }; }
};

// ────────── Parser ──────────
class Parser {
    Lexer lexer;
    Token cur;

    void advance() { cur = lexer.getNextToken(); }
    void expect(TokenType t) {
        if (cur.type!=t) throw ParsingError("Unexpected token: '" + cur.text + "' expecting token type " + std::to_string(static_cast<int>(t)), lexer.getPosition());
        advance();
    }
    
    std::unique_ptr<ASTNode> factor();
    std::unique_ptr<ASTNode> power();
    std::unique_ptr<ASTNode> term();
    std::unique_ptr<ASTNode> additive_expr();
    std::unique_ptr<ASTNode> comparison_expr();
    std::unique_ptr<ASTNode> equality_expr();
    std::unique_ptr<ASTNode> logical_and_expr();
    std::unique_ptr<ASTNode> logical_or_expr();
    std::unique_ptr<ASTNode> expr();
    std::unique_ptr<ASTNode> assignment();

public:
    Parser(const std::string& in): lexer(in) { advance(); }
    std::unique_ptr<ASTNode> parse();
};

std::unique_ptr<ASTNode> Parser::factor() {
    if (cur.type==TokenType::PLUS) { 
        advance(); 
        return std::make_unique<UnaryOpNode>("+", factor()); 
    }
    if (cur.type==TokenType::MINUS) { 
        advance(); 
        return std::make_unique<UnaryOpNode>("-", factor()); 
    }
    if (cur.type==TokenType::LOGICAL_NOT) { 
        advance(); 
        return std::make_unique<UnaryOpNode>("!", factor()); 
    }
    if (cur.type==TokenType::NUMBER) {
        auto node = std::make_unique<NumberNode>(cur.value, cur.text);
        advance();
        return node;
    }
    if (cur.type==TokenType::IDENTIFIER) {
        auto node = std::make_unique<VariableNode>(cur.text);
        advance();
        return node;
    }
    if (cur.type==TokenType::FUNCTION) {
        std::string name=cur.text; advance();
        expect(TokenType::LPAREN);
        std::vector<std::unique_ptr<ASTNode>> args;
        if (cur.type != TokenType::RPAREN) { 
            args.push_back(expr());
        
            while(cur.type == TokenType::COMMA) {
                advance();
                args.push_back(expr());
            }
        }
        expect(TokenType::RPAREN);
        return std::make_unique<FunctionNode>(name, std::move(args));
    }
    if (cur.type==TokenType::LPAREN) {
        advance();
        auto node=expr();
        expect(TokenType::RPAREN);
        return node;
    }
    throw ParsingError("Unexpected token in expression: '" + cur.text + "'", lexer.getPosition());
}
std::unique_ptr<ASTNode> Parser::power() {
    auto node = factor();
    while (cur.type == TokenType::POW) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), factor());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::term() {
    auto node = power();
    while (cur.type == TokenType::MUL || cur.type == TokenType::DIV) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), power());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::additive_expr() {
    auto node = term();
    while (cur.type == TokenType::PLUS || cur.type == TokenType::MINUS) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), term());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::comparison_expr() {
    auto node = additive_expr();
    while (cur.type == TokenType::LESS || cur.type == TokenType::LESS_EQUAL ||
           cur.type == TokenType::GREATER || cur.type == TokenType::GREATER_EQUAL) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), additive_expr());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::equality_expr() {
    auto node = comparison_expr();
    while (cur.type == TokenType::EQUAL || cur.type == TokenType::NOT_EQUAL) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), comparison_expr());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::logical_and_expr() {
    auto node = equality_expr();
    while (cur.type == TokenType::LOGICAL_AND) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), equality_expr());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::logical_or_expr() {
    auto node = logical_and_expr();
    while (cur.type == TokenType::LOGICAL_OR) {
        std::string op = cur.text; advance();
        node = std::make_unique<BinaryOpNode>(op, std::move(node), logical_and_expr());
    }
    return node;
}
std::unique_ptr<ASTNode> Parser::expr() {
    return logical_or_expr(); 
}

std::unique_ptr<ASTNode> Parser::assignment() {
    auto node = expr();

    if (cur.type == TokenType::ASSIGN) {
        VariableNode* var_node = dynamic_cast<VariableNode*>(node.get());
        if (var_node) {
            std::string var_name = var_node->toString(); 
            advance();
            auto rhs = assignment();
            return std::make_unique<AssignmentNode>(var_name, std::move(rhs));
        } else {
            throw ParsingError("Invalid assignment target. Must be a variable.", lexer.getPosition());
        }
    }
    return node; 
}


std::unique_ptr<ASTNode> Parser::parse() {
    if (cur.type == TokenType::END) return nullptr;
    std::vector<std::unique_ptr<ASTNode>> expr_list;
    do {
        expr_list.push_back(assignment()); 
        if (cur.type == TokenType::COMMA) {
            advance(); 
            if (cur.type == TokenType::END) { 
                throw ParsingError("Trailing comma at end of expression sequence.", lexer.getPosition());
            }
        } else {
            break; 
        }
    } while (cur.type != TokenType::END); 

    if (expr_list.empty()) return nullptr;
    if (expr_list.size() == 1) return std::move(expr_list[0]); 
    return std::make_unique<SequenceNode>(std::move(expr_list));
}


static int gv_counter = 0;
// write dote fuction to create Ast graph
int writeDOT(const ASTNode* n, std::ostream& out) {
    int id = gv_counter++;
    std::string label = n->toString();
    std::string escaped_label;
    for (char c : label) {
        if (c == '"') {
            escaped_label += "\\\"";
        } else {
            escaped_label += c;
        }
    }
    out << "  node" << id << " [label=\"" << escaped_label << "\"];\n";
    for (const ASTNode* child : n->children()) {
        int child_id = writeDOT(child, out);
        out << "  node" << id << " -> node" << child_id << ";\n";
    }
    return id;
}

void exportASTtoDOT(const ASTNode* root, const std::string& dst) {
    std::ofstream out(dst + ".dot");
    if (!out.is_open()) {
        std::cerr << "Error: Could not open DOT file for writing.\n";
        return;
    }
    gv_counter = 0;
    out << "digraph AST {\n";
    out << "  node [shape=box];\n";
    if (root) {
        writeDOT(root, out);
    }
    out << "}\n";
    out.close();

    std::string command = "dot -Tpng " + dst + ".dot -o " + dst + ".png";
    int result = system(command.c_str());
    if (result != 0) {
        std::cerr << "Warning: 'dot' command failed (exit code " << result << "). Make sure Graphviz is installed and in your PATH.\n";
        std::cerr << "Command executed: " << command << std::endl;
    }
}

struct EvalOutputs {
    std::string tokens, simplifiedAST, evalSteps, finalResult, tac;
};
EvalOutputs processExpression(const std::string& line, SymbolTable& symbols) {
    EvalOutputs outputs;
    std::ostringstream ss_tokens, ss_eval_steps, ss_tac;
    std::vector<std::string> evaluation_steps_log;

    try {
        Lexer lexer_instance(line); 
        Token token_instance; 
        do {
            token_instance = lexer_instance.getNextToken();
            ss_tokens << "Token: " << token_instance.text << " Type: ";
            switch (token_instance.type) {
                case TokenType::NUMBER: ss_tokens << "NUMBER (Value: " << token_instance.value << ")"; break;
                case TokenType::IDENTIFIER: ss_tokens << "IDENTIFIER"; break;
                case TokenType::FUNCTION: ss_tokens << "FUNCTION"; break;
                case TokenType::ASSIGN: ss_tokens << "ASSIGN"; break;
                case TokenType::PLUS: ss_tokens << "PLUS"; break;
                case TokenType::MINUS: ss_tokens << "MINUS"; break;
                case TokenType::MUL: ss_tokens << "MUL"; break;
                case TokenType::DIV: ss_tokens << "DIV"; break;
                case TokenType::POW: ss_tokens << "POW"; break;
                case TokenType::LPAREN: ss_tokens << "LPAREN"; break;
                case TokenType::RPAREN: ss_tokens << "RPAREN"; break;
                case TokenType::COMMA: ss_tokens << "COMMA"; break;
                case TokenType::EQUAL: ss_tokens << "EQUAL"; break;
                case TokenType::NOT_EQUAL: ss_tokens << "NOT_EQUAL"; break;
                case TokenType::LESS: ss_tokens << "LESS"; break;
                case TokenType::LESS_EQUAL: ss_tokens << "LESS_EQUAL"; break;
                case TokenType::GREATER: ss_tokens << "GREATER"; break;
                case TokenType::GREATER_EQUAL: ss_tokens << "GREATER_EQUAL"; break;
                case TokenType::LOGICAL_AND: ss_tokens << "LOGICAL_AND"; break;
                case TokenType::LOGICAL_OR: ss_tokens << "LOGICAL_OR"; break;
                case TokenType::LOGICAL_NOT: ss_tokens << "LOGICAL_NOT"; break;
                case TokenType::END: ss_tokens << "END"; break;
            }
            ss_tokens << "\n";
        } while (token_instance.type != TokenType::END);
        outputs.tokens = ss_tokens.str();

        Parser parser(line);
        std::unique_ptr<ASTNode> raw_ast = parser.parse(); 

        if (raw_ast) {
            std::unique_ptr<ASTNode> simplified_ast = raw_ast->simplify();

            if (simplified_ast) { 
                outputs.simplifiedAST = simplified_ast->toString();
                exportASTtoDOT(simplified_ast.get(), "ast_simplified"); 

                std::vector<TACInstruction> tac_code;
                /*std::string tac_result_var =*/ simplified_ast->generateTAC(tac_code); 
                for (const auto& instr : tac_code) {
                    ss_tac << instr.toString() << "\n";
                }
                outputs.tac = ss_tac.str();
                
                double result = simplified_ast->evaluate(symbols, evaluation_steps_log);

                for (const auto& step : evaluation_steps_log) {
                    ss_eval_steps << step << "\n";
                }
                outputs.evalSteps = ss_eval_steps.str();
                outputs.finalResult = std::to_string(result);
                outputs.evalSteps += "Final Evaluation Result: " + std::to_string(result) + "\n";
            } else { 
                outputs.finalResult = "Expression simplified to nothing or parsing failed initially.";
                outputs.simplifiedAST = "nullptr (expression simplified to nothing or parsing issue)";
                outputs.tac = "No TAC generated.";
                outputs.evalSteps = "No evaluation steps.";
                exportASTtoDOT(nullptr, "ast_simplified"); 
            }
        } else { 
            outputs.finalResult = "No expression to evaluate.";
            outputs.simplifiedAST = "nullptr (parsing failed or empty)";
            outputs.tac = "No TAC generated.";
            outputs.evalSteps = "No evaluation steps.";
            exportASTtoDOT(nullptr, "ast_simplified"); 
        }

    } catch (const ParsingError& e) {
        outputs.tokens = ss_tokens.str(); 
        outputs.finalResult = "Error: " + std::string(e.what());
        outputs.simplifiedAST = "Error during processing.";
        outputs.tac = "Error during processing.";
        outputs.evalSteps = "Error: " + std::string(e.what()) + (outputs.evalSteps.empty() ? "" : "\nPartial steps:\n" + outputs.evalSteps);
        throw; 
    } catch (const std::exception& e) {
        outputs.tokens = ss_tokens.str();
        outputs.finalResult = "Standard Error: " + std::string(e.what());
        outputs.simplifiedAST = "Error during processing.";
        outputs.tac = "Error during processing.";
        outputs.evalSteps = "Standard Error: " + std::string(e.what()) + (outputs.evalSteps.empty() ? "" : "\nPartial steps:\n" + outputs.evalSteps);
        throw; 
    }
    return outputs;
}
// ────────── GTKmm GUI ──────────
class CalculatorWindow : public Gtk::Window {
private:
    SymbolTable symbols_;
    std::vector<std::string> history_;
    Gtk::TextView* tv_symtab_ = nullptr;

public:
    CalculatorWindow() {
        set_title("Expression Compiler GUI");
        set_default_size(900, 750); 
        set_child(vbox_);
        set_name("main-window");
        entry_.set_placeholder_text("Enter expression, e.g., 'x=5, (x > 2) && (x < 10)'");
        entry_.set_hexpand(true);
        entry_.get_style_context()->add_class("custom-entry");

        btn_history_.set_name("history-button");
        btn_history_.set_can_focus(false);
        btn_history_.set_tooltip_text("Show history");
        btn_history_.set_margin_top(3); 
        btn_history_.set_margin_bottom(3);
        btn_history_.set_margin_end(2);
        btn_history_.set_icon_name("view-history-symbolic");

        popover_.set_has_arrow(true);
        popover_.set_child(listbox_history_);
        popover_.set_parent(btn_history_);
        listbox_history_.set_margin(5); 

        btn_history_.signal_clicked().connect([this]() {
            popover_.popup();
        });

        btn_eval_.set_label("Evaluate");
        btn_eval_.get_style_context()->add_class("primary-action-button"); 

        btn_clear_symbols_.set_label("Clear Symbols");
        btn_clear_symbols_.set_name("clear-symbols-button"); 
        btn_clear_symbols_.set_icon_name("edit-clear-symbolic"); 
        btn_clear_symbols_.get_style_context()->add_class("secondary-action-button"); 

        hbox_top_.set_spacing(10); 
        hbox_top_.append(entry_);
        hbox_top_.append(btn_history_);
        hbox_top_.append(btn_eval_);
        hbox_top_.append(btn_clear_symbols_);
        vbox_.append(hbox_top_);

        error_label_.get_style_context()->add_class("error-label");
        error_label_.set_wrap(true);
        error_label_.set_xalign(0.0);
        vbox_.append(error_label_);

        notebook_.set_vexpand(true);
        vbox_.append(notebook_);
        add_tab("Tokens", tv_tok_);
        add_tab("AST (Simplified)", tv_ast_);
        add_tab("Steps", tv_eval_);
        add_tab("Result", tv_res_);
        add_tab("3-Addr Code", tv_tac_);
        add_tab("Symbol Table", tv_symtab_); 
        add_tab_image("AST Graph", tv_graph_);


        update_symbol_table_view(); 
        btn_eval_.signal_clicked().connect(sigc::mem_fun(*this, &CalculatorWindow::on_evaluate));
        entry_.signal_activate().connect(sigc::mem_fun(*this, &CalculatorWindow::on_evaluate));
        btn_clear_symbols_.signal_clicked().connect(sigc::mem_fun(*this, &CalculatorWindow::on_clear_symbols));

        apply_styles();
    }

protected:
    Gtk::Box vbox_{Gtk::Orientation::VERTICAL, 0}; 
    Gtk::Box hbox_top_{Gtk::Orientation::HORIZONTAL, 0}; 
    Gtk::Entry entry_;
    Gtk::Button btn_eval_;
    Gtk::Button btn_clear_symbols_;
    Gtk::Button btn_history_; 
    Gtk::Popover popover_;
    Gtk::ListBox listbox_history_;
    Gtk::Notebook notebook_;
    Gtk::Label error_label_;
    Gtk::TextView* tv_tok_ = nullptr;
    Gtk::TextView* tv_ast_ = nullptr;
    Gtk::TextView* tv_eval_ = nullptr;
    Gtk::TextView* tv_res_ = nullptr;
    Gtk::TextView* tv_tac_ = nullptr;
    Gtk::Image* tv_graph_ = nullptr;

    void add_tab(const Glib::ustring& label, Gtk::TextView*& tv_member_ptr) {
        if (label == "Symbol Table") {
            tv_symtab_ = Gtk::manage(new Gtk::TextView());
            tv_member_ptr = tv_symtab_; 
        } else {
             tv_member_ptr = Gtk::manage(new Gtk::TextView());
        }
        
        tv_member_ptr->set_editable(false);
        tv_member_ptr->set_wrap_mode(Gtk::WrapMode::WORD_CHAR);
        auto scrolled_window = Gtk::manage(new Gtk::ScrolledWindow());
        scrolled_window->set_child(*tv_member_ptr);
        scrolled_window->set_policy(Gtk::PolicyType::AUTOMATIC, Gtk::PolicyType::AUTOMATIC);
        notebook_.append_page(*scrolled_window, label);
    }

    void add_tab_image(const Glib::ustring& label, Gtk::Image*& img) {
        img = Gtk::manage(new Gtk::Image());
        img->set_vexpand(true); 
        img->set_hexpand(true);
        img->set_icon_size(Gtk::IconSize::LARGE); 
        auto scrolled_window = Gtk::manage(new Gtk::ScrolledWindow());
        scrolled_window->set_child(*img);
        scrolled_window->set_policy(Gtk::PolicyType::AUTOMATIC, Gtk::PolicyType::AUTOMATIC);
        notebook_.append_page(*scrolled_window, label);
    }

    void update_symbol_table_view() {
        std::ostringstream os;
        if (symbols_.data().empty()) { os << "[Symbol table is empty]"; }
        for (const auto& [name, value] : symbols_.data()) {
            os << name << " = " << value << "\n";
        }
        if (tv_symtab_) tv_symtab_->get_buffer()->set_text(os.str());
    }

    void on_clear_symbols() {
        symbols_.clear();
        update_symbol_table_view();
        entry_.grab_focus(); 
    }

    void on_evaluate() {
        error_label_.set_text("");
        entry_.get_style_context()->remove_class("input-error");

        const auto current_expression_text = entry_.get_text(); 
        if (current_expression_text.empty()) {
            return;
        }

        EvalOutputs outputs; 
        try {
            update_history(current_expression_text);
            outputs = processExpression(current_expression_text, symbols_); 

            if(tv_tok_) tv_tok_->get_buffer()->set_text(outputs.tokens);
            if(tv_ast_) tv_ast_->get_buffer()->set_text(outputs.simplifiedAST);
            if(tv_eval_) tv_eval_->get_buffer()->set_text(outputs.evalSteps);
            if(tv_res_) tv_res_->get_buffer()->set_text(outputs.finalResult);
            if(tv_tac_) tv_tac_->get_buffer()->set_text(outputs.tac);

            if (tv_graph_) {
                std::string image_path = "ast_simplified.png";
                if (Glib::file_test(image_path, Glib::FileTest::EXISTS) && Glib::file_test(image_path, Glib::FileTest::IS_REGULAR)) {
                     Glib::RefPtr<Gio::File> file = Gio::File::create_for_path(image_path);
                     Glib::RefPtr<Gdk::Pixbuf> pixbuf;
                     Glib::RefPtr<Gdk::Texture> texture;
                     try {
                        pixbuf = Gdk::Pixbuf::create_from_file(image_path);
                        if (pixbuf && pixbuf->get_width() > 0 && pixbuf->get_height() > 0) {
                             texture = Gdk::Texture::create_from_file(file);
                             tv_graph_->set(texture);
                        } else {
                             tv_graph_->set_from_icon_name("image-missing-symbolic"); 
                             tv_graph_->set_pixel_size(64); 
                        }
                     } catch (const Glib::Error& err) {
                        std::cerr << "Error loading image '" << image_path << "': " << err.what() << std::endl;
                        tv_graph_->set_from_icon_name("dialog-warning-symbolic"); 
                        tv_graph_->set_pixel_size(64);
                     }   
                } else {
                    std::cerr << "AST image file not found or not a regular file: " << image_path << std::endl;
                    tv_graph_->set_from_icon_name("image-missing-symbolic");
                    tv_graph_->set_pixel_size(64);
                }
            }
            update_symbol_table_view();
        }
        catch (const ParsingError& e) { 
            error_label_.set_text("Parsing Error: " + Glib::ustring(e.what()));
            entry_.get_style_context()->add_class("input-error");
            if(tv_tok_ && !outputs.tokens.empty()) tv_tok_->get_buffer()->set_text(outputs.tokens); 
            if(tv_ast_) tv_ast_->get_buffer()->set_text(outputs.simplifiedAST.empty() ? "Error occurred during AST generation." : outputs.simplifiedAST);
            if(tv_eval_) tv_eval_->get_buffer()->set_text(outputs.evalSteps.empty() ? Glib::ustring(e.what()) : Glib::ustring(outputs.evalSteps));
            if(tv_res_) tv_res_->get_buffer()->set_text(outputs.finalResult.empty() ? Glib::ustring(e.what()) : Glib::ustring(outputs.finalResult));
            if(tv_tac_) tv_tac_->get_buffer()->set_text(outputs.tac.empty() ? "Error occurred during TAC generation." : outputs.tac);
        }
        catch (const std::exception& e) {
            error_label_.set_text("Standard Error: " + Glib::ustring(e.what()));
            entry_.get_style_context()->add_class("input-error");
            if(tv_tok_ && !outputs.tokens.empty()) tv_tok_->get_buffer()->set_text(outputs.tokens);
            if(tv_ast_) tv_ast_->get_buffer()->set_text(outputs.simplifiedAST.empty() ? "Error occurred." : outputs.simplifiedAST);
            if(tv_eval_) tv_eval_->get_buffer()->set_text(outputs.evalSteps.empty() ? Glib::ustring(e.what()) : Glib::ustring(outputs.evalSteps));
            if(tv_res_) tv_res_->get_buffer()->set_text(outputs.finalResult.empty() ? Glib::ustring(e.what()) : Glib::ustring(outputs.finalResult));
            if(tv_tac_) tv_tac_->get_buffer()->set_text(outputs.tac.empty() ? "Error occurred." : outputs.tac);
        }
    }

    void update_history(const std::string& new_expr_str) { 
        history_.erase(std::remove(history_.begin(), history_.end(), new_expr_str), history_.end());
        history_.insert(history_.begin(), new_expr_str);

        if (history_.size() > 20) history_.pop_back(); 
        while (auto* first_child = listbox_history_.get_first_child()) {
            listbox_history_.remove(*first_child);
        }
        
        for (const auto& item : history_) {
            auto row = Gtk::make_managed<Gtk::ListBoxRow>();
            auto label = Gtk::make_managed<Gtk::Label>(item);
            label->set_margin_start(8); 
            label->set_margin_end(8);
            label->set_margin_top(5);
            label->set_margin_bottom(5);
            label->set_xalign(0.0f);
            row->set_child(*label);
            row->set_activatable(true); 
            listbox_history_.append(*row);
        }
        if (!m_history_signal_connected && !history_.empty()) {
             m_row_activated_connection = listbox_history_.signal_row_activated().connect(
                [this](Gtk::ListBoxRow* row) {
                    if (row) { 
                        if (auto label = dynamic_cast<Gtk::Label*>(row->get_child())) {
                            entry_.set_text(label->get_text());
                            popover_.popdown();
                            entry_.grab_focus(); 
                        }
                    }
            });
            m_history_signal_connected = true;
        } else if (history_.empty() && m_history_signal_connected) {
        }
    }
    bool m_history_signal_connected = false;
    sigc::connection m_row_activated_connection;


    void apply_styles() {
        auto css_provider = Gtk::CssProvider::create();
        try {
            css_provider->load_from_data(R"(
                window#main-window {
                    background-color: #f0f2f5; 
                    font-family: "Inter", "Segoe UI", Cantarell, "Helvetica Neue", sans-serif; 
                    padding: 0px;
                    margin: 0px;
                }
                
                window#main-window > GtkBox > GtkBox:first-child { 
                     padding: 15px; 
                     background-color: #ffffff; 
                     border-bottom: 1px solid #d0d7de;
                     margin-bottom: 5px; 
                }

                .custom-entry {
                    font-size: 15px; 
                    padding: 10px 14px;
                    border: 1px solid #c8d1dc; 
                    border-radius: 8px; 
                    background-color: #fdfdfe;
                    color: #24292f; 
                    box-shadow: inset 0 1px 2px rgba(0,0,0,0.03); 
                    transition: border-color 0.2s ease, box-shadow 0.2s ease;
                }
                .custom-entry:focus {
                    border-color: #0969da; 
                    box-shadow: 0 0 0 3px rgba(9, 105, 218, 0.2); 
                }
                .input-error { 
                    border-color: #d73a49; 
                    background-color: #fff6f6; 
                }
                .input-error:focus {
                     box-shadow: 0 0 0 3px rgba(215, 58, 73, 0.2);
                }
                GtkButton {
                    font-weight: 500; 
                    border-radius: 8px; 
                    padding: 9px 18px; 
                    border: 1px solid #c8d1dc; 
                    background-color: #f6f8fa; 
                    color: #24292f;
                    transition: background-color 0.1s ease-in-out, border-color 0.1s ease-in-out;
                    margin: 0px 4px; 
                }
                GtkButton:hover {
                    background-color: #f0f2f5; 
                    border-color: #b0b8c3;
                }
                GtkButton:active {
                    background-color: #e8ebef;
                    box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
                }
                GtkButton:focus {
                    outline: none;
                    border-color: #0969da; 
                    box-shadow: 0 0 0 3px rgba(9, 105, 218, 0.2); 
                }

                .primary-action-button {
                    background-color: #2da44e; 
                    color: white;
                    font-weight: 600; 
                    border-color: #2c974b; 
                }
                .primary-action-button:hover {
                    background-color: #289144;
                    border-color: #278840;
                }
                .primary-action-button:active {
                    background-color: #23803b;
                }
                .primary-action-button:focus {
                    border-color: #2da44e; 
                    box-shadow: 0 0 0 3px rgba(45, 164, 78, 0.3); 
                }

                .secondary-action-button {
                    background-color: #f6f8fa; 
                    color: #d73a49; 
                    border: 1px solid #d73a49; 
                }
                .secondary-action-button:hover {
                    background-color: #fdedee;
                    border-color: #cb2431;
                }
                 .secondary-action-button:active {
                    background-color: #fbdbe0;
                }
                .secondary-action-button GtkImage { 
                    color: #d73a49;
                }
                GtkButton#history-button {
                    background-color: transparent;
                    border: none;
                    box-shadow: none; 
                    padding: 8px; 
                    min-width: 0;
                    min-height: 0;
                }
                GtkButton#history-button GtkImage { 
                    color: #57606a; 
                }
                GtkButton#history-button:hover {
                    background-color: rgba(0, 0, 0, 0.06); 
                    border-radius: 50%; 
                }
                GtkButton#history-button:focus {
                    outline: none;
                    background-color: rgba(0, 0, 0, 0.08); 
                    border-radius: 50%;
                    box-shadow: 0 0 0 2px rgba(0, 0, 0, 0.1); 
                }
                .error-label {
                    color: #a40e26;
                    font-weight: 500;
                    margin: 10px 15px; 
                    padding: 12px 15px; 
                    background-color: #fde7ea; 
                    border: 1px solid #f5c6cb;
                    border-radius: 6px; 
                }
                .error-label:empty { 
                    padding: 0;
                    margin: 0 15px; 
                    border: none;
                    background-color: transparent;
                    min-height: 0px; 
                }

                GtkNotebook {
                    border: 1px solid #d0d7de; 
                    border-radius: 8px;
                    background-color: #ffffff; 
                    padding: 0px; 
                    margin: 10px 15px 15px 15px; 
                    box-shadow: 0 1px 3px rgba(0,0,0,0.05); 
                }
                GtkNotebook > header { 
                    background-color: #f6f8fa; 
                    padding: 5px 5px 0px 5px; 
                    border-bottom: 1px solid #d0d7de; 
                    border-radius: 8px 8px 0 0; 
                }
                GtkNotebook tab {
                    background-color: transparent; 
                    border: none; 
                    border-bottom: 3px solid transparent; 
                    border-radius: 6px 6px 0 0; 
                    margin-right: 3px;
                    padding: 0; 
                    box-shadow: none;
                    transition: border-color 0.2s ease, background-color 0.2s ease;
                }
                GtkNotebook tab:checked {
                    background-color: #ffffff; 
                    border-bottom: 3px solid #0969da; 
                }
                GtkNotebook tab:hover:not(:checked) {
                    background-color: #e8ebef; 
                    border-bottom-color: #c8d1dc; 
                }
                GtkNotebook tab GtkLabel { 
                    font-size: 14px;
                    padding: 10px 20px; 
                    color: #57606a; 
                }
                GtkNotebook tab:checked GtkLabel {
                    color: #0969da; 
                    font-weight: 600; 
                }
                GtkNotebook > GtkScrolledWindow, GtkNotebook > GtkBox {
                     padding: 12px; 
                     background-color: #ffffff;
                     border-radius: 0 0 8px 8px;
                }
                
                GtkTextView {
                    font-family: "SF Mono", "Consolas", "Liberation Mono", monospace; 
                    font-size: 13.5px; 
                    background-color: #f6f8fa; 
                    color: #24292f;
                    padding: 12px;
                    border: 1px solid #d0d7de; 
                    border-radius: 6px;
                }
                GtkTextView:focus {
                    border-color: #0969da;
                    background-color: #ffffff;
                }
                GtkScrolledWindow {
                    border-radius: 6px; 
                    border: none; 
                }
                GtkImage { 
                    background-color: #f6f8fa;
                    border-radius: 6px;
                }
                GtkPopover {
                    background-color: #ffffff;
                    border: 1px solid #d0d7de;
                    border-radius: 8px;
                    box-shadow: 0 8px 24px rgba(140,149,159,0.2); 
                    padding: 4px; 
                }
                GtkPopover GtkListBox {
                    background-color: transparent;
                }
                GtkPopover GtkListBoxRow {
                    padding: 0; 
                    border-bottom: 1px solid #eaeef2; 
                    transition: background-color 0.15s ease;
                }
                GtkPopover GtkListBoxRow:last-child {
                    border-bottom: none;
                }
                GtkPopover GtkListBoxRow:hover {
                    background-color: #f0f6fc; 
                }
                GtkPopover GtkListBoxRow GtkLabel {
                    padding: 10px 16px; 
                    font-size: 14px;
                    color: #24292f;
                }
            )");
            Gtk::StyleContext::add_provider_for_display(
                Gdk::Display::get_default(), css_provider, GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
        } catch (const Glib::Error& ex) {
            std::cerr << "CSS error: " << ex.what() << std::endl;
        }
    }
};

int main(int argc, char* argv[]) {
    auto app = Gtk::Application::create("org.gtkmm.examples.expressioncompiler");
    return app->make_window_and_run<CalculatorWindow>(argc, argv);
}
