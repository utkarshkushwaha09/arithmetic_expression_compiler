// expr_gui.cpp

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

// ────────── Token Definitions ──────────
enum class TokenType {
    NUMBER, IDENTIFIER, FUNCTION,
    ASSIGN,
    PLUS, MINUS, MUL, DIV,
    LPAREN, RPAREN, COMMA,
    END
};

struct Token {
    TokenType   type;
    std::string text;
    double      value;  // only for NUMBER

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
        else                      ch = '\0';
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
        if (buf=="sin"||buf=="cos"||buf=="tan"||buf=="sqrt"||buf=="pow")
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
          case '(': advance(); return Token(TokenType::LPAREN, "(");
          case ')': advance(); return Token(TokenType::RPAREN, ")");
          case ',': advance(); return Token(TokenType::COMMA, ",");
          case '=': advance(); return Token(TokenType::ASSIGN, "=");
          case '\0':         return Token(TokenType::END, "");
          default:
            throw std::runtime_error(std::string("Invalid character: ") + ch);
        }
    }
};

// ────────── TAC and Symbol Table ──────────
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
        return result+" = "+arg1+" "+op+" "+arg2;
    }
};

class SymbolTable {
    std::map<std::string,double> tbl;
public:
    void set(const std::string& name,double v){ tbl[name]=v; }
    double get(const std::string& name)const {
        auto it=tbl.find(name);
        if(it==tbl.end()) throw std::runtime_error("Undefined var: "+name);
        return it->second;
    }
    void print(std::ostream& os)const {
        for(auto& kv:tbl)
            os<<kv.first<<" = "<<kv.second<<"\n";
    }
};

// ────────── AST Base ──────────
class ASTNode {
public:
    virtual ~ASTNode() {}
    virtual double evaluate(SymbolTable&) const = 0;
    virtual std::string generateTAC(std::vector<TACInstruction>&) const = 0;
    virtual std::string toString() const = 0;
    virtual std::unique_ptr<ASTNode> simplify() = 0;
};

// ────────── AST Leaf & Composite Nodes ──────────
class NumberNode : public ASTNode {
    double      val;
    std::string lit;
public:
    NumberNode(double v,const std::string& txt="")
      : val(v), lit(txt.empty()?std::to_string(v):txt) {}
    double getValue() const { return val; }
    double evaluate(SymbolTable&) const override {
        std::cout<<"Evaluating number: "<<lit<<"\n";
        return val;
    }
    std::string generateTAC(std::vector<TACInstruction>&) const override {
        return lit;
    }
    std::string toString() const override { return lit; }
    std::unique_ptr<ASTNode> simplify() override {
        return std::make_unique<NumberNode>(val,lit);
    }
};

class VariableNode : public ASTNode {
    std::string name;
public:
    VariableNode(const std::string& n): name(n) {}
    double evaluate(SymbolTable& S) const override {
        double v=S.get(name);
        std::cout<<"Evaluating variable: "<<name<<" = "<<v<<"\n";
        return v;
    }
    std::string generateTAC(std::vector<TACInstruction>&) const override {
        return name;
    }
    std::string toString() const override { return name; }
    std::unique_ptr<ASTNode> simplify() override {
        return std::make_unique<VariableNode>(name);
    }
};

class UnaryOpNode : public ASTNode {
    std::string op;
    std::unique_ptr<ASTNode> operand;
public:
    UnaryOpNode(const std::string& o,std::unique_ptr<ASTNode> c)
      : op(o), operand(std::move(c)) {}
    double evaluate(SymbolTable& S) const override {
        double v=operand->evaluate(S);
        double r = (op=="-"?-v:v);
        std::cout<<"Evaluating: "<<op<<v<<" = "<<r<<"\n";
        return r;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string a = operand->generateTAC(code);
        std::string tmp="t"+std::to_string(code.size());
        code.push_back({tmp,op,"0",a});
        return tmp;
    }
    std::string toString() const override {
        return op+operand->toString();
    }
    std::unique_ptr<ASTNode> simplify() override {
        auto s = operand->simplify();
        if(auto n = dynamic_cast<NumberNode*>(s.get())) {
            return std::make_unique<NumberNode>(op=="-"?-n->getValue():n->getValue());
        }
        return std::make_unique<UnaryOpNode>(op,std::move(s));
    }
};

class BinaryOpNode : public ASTNode {
    std::string op;
    std::unique_ptr<ASTNode> left, right;
public:
    BinaryOpNode(const std::string& o,
                 std::unique_ptr<ASTNode> l,
                 std::unique_ptr<ASTNode> r)
      : op(o), left(std::move(l)), right(std::move(r)) {}

    double evaluate(SymbolTable& S) const override {
        double L=left->evaluate(S), R=right->evaluate(S), Rv;
        if      (op=="+") Rv=L+R;
        else if (op=="-") Rv=L-R;
        else if (op=="*") Rv=L*R;
        else if (op=="/") Rv=L/R;
        else throw std::runtime_error("Unknown op: "+op);
        std::cout<<"Evaluating: ("<<L<<" "<<op<<" "<<R<<") = "<<Rv<<"\n";
        return Rv;
    }
    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::string a=left->generateTAC(code),
                    b=right->generateTAC(code),
                    tmp="t"+std::to_string(code.size());
        code.push_back({tmp,op,a,b});
        return tmp;
    }
    std::string toString() const override {
        return "("+left->toString()+" "+op+" "+right->toString()+")";
    }
    std::unique_ptr<ASTNode> simplify() override {
        auto Ls=left->simplify(), Rs=right->simplify();
        auto nL=dynamic_cast<NumberNode*>(Ls.get()),
             nR=dynamic_cast<NumberNode*>(Rs.get());
        if(nL&&nR) { double v=
            (op=="+")?nL->getValue()+nR->getValue():
            (op=="-")?nL->getValue()-nR->getValue():
            (op=="*")?nL->getValue()*nR->getValue():
                       nL->getValue()/nR->getValue();
            return std::make_unique<NumberNode>(v);
        }
        // identity rules (0,1) omitted for brevity—feel free to re-add
        return std::make_unique<BinaryOpNode>(op,std::move(Ls),std::move(Rs));
    }
};

class FunctionNode : public ASTNode {
    std::string name;
    std::vector<std::unique_ptr<ASTNode>> args;
public:
    FunctionNode(const std::string& n,
                 std::vector<std::unique_ptr<ASTNode>> a)
      : name(n), args(std::move(a)) {}

    double evaluate(SymbolTable& S) const override {
        std::vector<double> vals;
        for(auto& a: args) vals.push_back(a->evaluate(S));
        double r;
        if      (name=="sin")  r=std::sin(vals[0]);
        else if (name=="cos")  r=std::cos(vals[0]);
        else if (name=="tan")  r=std::tan(vals[0]);
        else if (name=="sqrt") r=std::sqrt(vals[0]);
        else if (name=="pow")  r=std::pow(vals[0],vals[1]);
        else throw std::runtime_error("Unknown func: "+name);
        std::cout<<"Evaluating func: "<<name<<"(...)= "<<r<<"\n";
        return r;
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        std::vector<std::string> regs;
        for(auto& a: args) regs.push_back(a->generateTAC(code));
        std::string tmp="t"+std::to_string(code.size()),
                    arg2=regs.size()>1?regs[1]:"";
        code.push_back({tmp,"call",name+"("+regs[0]+")",arg2});
        return tmp;
    }

    std::string toString() const override {
        std::ostringstream os;
        os<<name<<"(";
        for(size_t i=0;i<args.size();++i){
          os<<args[i]->toString()<<(i+1<args.size()?", ":"");
        }
        os<<")";
        return os.str();
    }

    std::unique_ptr<ASTNode> simplify() override {
        std::vector<std::unique_ptr<ASTNode>> sargs;
        for(auto& a: args) sargs.push_back(a->simplify());
        return std::make_unique<FunctionNode>(name, std::move(sargs));
    }
};

class AssignmentNode : public ASTNode {
    std::string var;
    std::unique_ptr<ASTNode> expr;
public:
    AssignmentNode(const std::string& v,std::unique_ptr<ASTNode> e)
      : var(v), expr(std::move(e)) {}

    double evaluate(SymbolTable& S) const override {
        double v=expr->evaluate(S);
        S.set(var,v);
        std::cout<<"Assignment: "<<var<<"="<<v<<"\n";
        return v;
    }

    std::string generateTAC(std::vector<TACInstruction>& code) const override {
        auto r=expr->generateTAC(code);
        code.push_back({var,"=",r,""});
        return var;
    }

    std::string toString() const override {
        return var+" = "+expr->toString();
    }

    std::unique_ptr<ASTNode> simplify() override {
        return std::make_unique<AssignmentNode>(var, expr->simplify());
    }
};

// ────────── Parser ──────────
class Parser {
    Lexer lexer;
    Token cur;

    void advance() { cur = lexer.getNextToken(); }
    void expect(TokenType t) {
        if (cur.type!=t) throw std::runtime_error("Unexpected token: "+cur.text);
        advance();
    }

    std::unique_ptr<ASTNode> factor() {
        if (cur.type==TokenType::PLUS) {
            advance(); return factor();
        }
        if (cur.type==TokenType::MINUS) {
            advance();
            return std::make_unique<UnaryOpNode>("-", factor());
        }
        if (cur.type==TokenType::NUMBER) {
            double v=cur.value; std::string txt=cur.text;
            advance();
            return std::make_unique<NumberNode>(v,txt);
        }
        if (cur.type==TokenType::IDENTIFIER) {
            std::string name=cur.text; advance();
            return std::make_unique<VariableNode>(name);
        }
        if (cur.type==TokenType::FUNCTION) {
            std::string name=cur.text; advance();
            expect(TokenType::LPAREN);
            std::vector<std::unique_ptr<ASTNode>> args;
            args.push_back(expr());
            if (name=="pow") {
                expect(TokenType::COMMA);
                args.push_back(expr());
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
        throw std::runtime_error("Unexpected in factor: "+cur.text);
    }

    std::unique_ptr<ASTNode> term() {
        auto node=factor();
        while (cur.type==TokenType::MUL||cur.type==TokenType::DIV) {
            std::string op = (cur.type==TokenType::MUL?"*":"/");
            advance();
            node = std::make_unique<BinaryOpNode>(op, std::move(node), factor());
        }
        return node;
    }

    std::unique_ptr<ASTNode> expr() {
        auto node=term();
        while (cur.type==TokenType::PLUS||cur.type==TokenType::MINUS) {
            std::string op = (cur.type==TokenType::PLUS?"+":"-");
            advance();
            node = std::make_unique<BinaryOpNode>(op, std::move(node), term());
        }
        return node;
    }

public:
    Parser(const std::string& in): lexer(in) { advance(); }

    std::unique_ptr<ASTNode> parse() {
        if (cur.type==TokenType::IDENTIFIER) {
            std::string name=cur.text; advance();
            if (cur.type==TokenType::ASSIGN) {
                advance();
                auto rhs=expr();
                return std::make_unique<AssignmentNode>(name, std::move(rhs));
            } else {
                // not assign, treat as var
                return std::make_unique<VariableNode>(name);
            }
        }
        return expr();
    }
};

// ────────── Process & Collect Outputs ──────────
struct EvalOutputs {
    std::string tokens, simplifiedAST, evalSteps, finalResult, tac;
};

EvalOutputs processExpression(const std::string& line) {
    std::ostringstream out_tok, out_ast, out_eval, out_final, out_tac;
    SymbolTable symbols;

    // — TOKENS —
    {
      Lexer lx(line);
      for (Token tk=lx.getNextToken(); tk.type!=TokenType::END; 
           tk=lx.getNextToken())
      {
        out_tok << tk.text;
        if (tk.type==TokenType::NUMBER)
          out_tok << " (value="<<tk.value<<")";
        out_tok<<"\n";
      }
    }

    // — PARSE & SIMPLIFY —
    Parser parser(line);
    auto root = parser.parse();
    auto simp = root->simplify();
    out_ast << simp->toString() << "\n";

    // — EVAL STEPS & FINAL —
    {
      // redirect std::cout
      std::streambuf* old = std::cout.rdbuf();
      std::cout.rdbuf(out_eval.rdbuf());
      double result = simp->evaluate(symbols);
      std::cout.rdbuf(old);
      out_final << result;
    }

    // — THREE‑ADDRESS CODE —
    {
      std::vector<TACInstruction> code;
      simp->generateTAC(code);
      for (auto& instr: code)
        out_tac << instr.toString() << "\n";
    }

    return { out_tok.str(),
             out_ast.str(),
             out_eval.str(),
             out_final.str(),
             out_tac.str() };
}

// ────────── New GTKmm Window with Notebook ──────────

class CalculatorWindow : public Gtk::Window {
public:
  CalculatorWindow(){
    set_title("Expression Compiler GUI");
    set_default_size(800,600);

    // Vertical box to hold entry+button and then the notebook
    auto vbox = Gtk::make_managed<Gtk::Box>(Gtk::ORIENTATION_VERTICAL, 5);
    add(*vbox);

    // Top row: entry + button
    auto hbox = Gtk::make_managed<Gtk::Box>(Gtk::ORIENTATION_HORIZONTAL, 5);
    vbox->pack_start(*hbox, Gtk::PACK_SHRINK);
    entry_.set_placeholder_text("Enter expression...");
    hbox->pack_start(entry_);
    btn_eval_.set_label("Evaluate");
    hbox->pack_start(btn_eval_, Gtk::PACK_SHRINK);

    // Notebook
    notebook_ = Gtk::make_managed<Gtk::Notebook>();
    vbox->pack_start(*notebook_);

    // Helper to add one TextView page to the notebook
    auto add_tab = [&](const Glib::ustring& label, Gtk::TextView*& tv){
      // Create the TextView and wrap it in a scrolled window
      auto sw = Gtk::make_managed<Gtk::ScrolledWindow>();
      sw->set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);

      tv = Gtk::make_managed<Gtk::TextView>();
      tv->set_editable(false);
      // Fill the entire notebook page
      tv->set_hexpand(true);
      tv->set_vexpand(true);

      sw->add(*tv);
      notebook_->append_page(*sw, label);
    };

    // Add the five tabs
    add_tab("Tokens",    tv_tok_);
    add_tab("AST",       tv_ast_);
    add_tab("Steps",     tv_eval_);
    add_tab("Result",    tv_res_);
    add_tab("3‑Addr Code", tv_tac_);

    // Wire button
    btn_eval_.signal_clicked()
      .connect(sigc::mem_fun(*this, &CalculatorWindow::on_evaluate));

    // Apply custom styles
    apply_styles();

    show_all_children();
  }

protected:
  Gtk::Entry    entry_;
  Gtk::Button   btn_eval_;
  Gtk::Notebook *notebook_;
  Gtk::TextView *tv_tok_, *tv_ast_, *tv_eval_, *tv_res_, *tv_tac_;

  void apply_styles() {
    try {
        // Create a CSS provider to style the widgets
        Glib::RefPtr<Gtk::CssProvider> provider = Gtk::CssProvider::create();
        
    const std::string css = R"(
    /* Main window styling */
    .main-window {
        background-color: #2d2d2d;
        font-family: 'Segoe UI', Roboto, sans-serif;
        color: #ffffff;
    }

    /* Entry widget styling */
    .custom-entry {
        font-size: 16px;
        padding: 12px 15px;
        border: 2px solid #4A4A4A;
        border-radius: 8px;
        background-color: #3d3d3d;
        color: #ffffff;
        transition: all 200ms ease-in-out;
    }

    .custom-entry:focus {
        border-color: #00ff88;
        box-shadow: 0 0 4px rgba(0,255,136,0.3);
    }

    /* Button styling */
    .gradient-btn {
        background-image: linear-gradient(to bottom, #00ff88, #00ccff);
        color: #2d2d2d;
        font-size: 16px;
        font-weight: 600;
        padding: 12px 25px;
        border: none;
        border-radius: 8px;
        transition: all 200ms ease-in-out;
    }

    .gradient-btn:hover {
        background-image: linear-gradient(to bottom, #00e67a, #00b8e6);
        -gtk-icon-effect: highlight;
    }

    /* Notebook styling */
    GtkNotebook {
        background-color: #363636;
        border-radius: 8px;
        padding: 10px;
    }

    GtkNotebook tab {
        background-color: #4A4A4A;
        color: #ffffff;
        padding: 8px 15px;
        border-radius: 5px;
        margin: 2px;
    }

    GtkNotebook tab:active {
        background-image: linear-gradient(to bottom, #00ff88, #00ccff);
        color: #2d2d2d;
    }

    /* Text view styling */
    GtkTextView {
        font-family: monospace;
        font-size: 14px;
        background-color: #2d2d2d;
        color: #00ff88;
        padding: 15px;
        border: 2px solid #4A4A4A;
        caret-color: #00ff88;
    }

    /* Scrollbar styling */
    GtkScrollbar slider {
        min-width: 12px;
        background-color: #00ff88;
    }

    GtkScrollbar trough {
        background-color: #3d3d3d;
    }
)";



        // Load the CSS into the provider
        provider->load_from_data(css);

        // Apply the CSS to the window
        Gtk::StyleContext::add_provider_for_screen(
            Gdk::Screen::get_default(),
            provider,
            GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
        );
    } catch (const Glib::Error& e) {
        std::cerr << "Error applying CSS: " << e.what() << std::endl;
    }
}

  void on_evaluate(){
    const auto expr = entry_.get_text();
    if(expr.empty()) return;

    EvalOutputs out;
    try {
      out = processExpression(expr);
    } catch(const std::exception& e){
      // On error, dump it into the “Steps” tab
      out.tokens       = "";
      out.simplifiedAST= "";
      out.evalSteps    = std::string("Error: ") + e.what();
      out.finalResult  = "";
      out.tac          = "";
    }

    tv_tok_->get_buffer()->set_text(out.tokens);
    tv_ast_->get_buffer()->set_text(out.simplifiedAST);
    tv_eval_->get_buffer()->set_text(out.evalSteps);
    tv_res_->get_buffer()->set_text(out.finalResult);
    tv_tac_->get_buffer()->set_text(out.tac);
  }
};


int main(int argc, char* argv[]){
  auto app = Gtk::Application::create(argc, argv, "com.example.exprgui");
  CalculatorWindow win;
  return app->run(win);
}
