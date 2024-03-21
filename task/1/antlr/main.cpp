#include "SYsULexer.h" // 确保这里的头文件名与您生成的词法分析器匹配
#include <fstream>
#include <iostream>
#include <regex>
#include <unordered_map>

// 映射定义，将ANTLR的tokenTypeName映射到clang的格式
std::unordered_map<std::string, std::string> tokenTypeMapping = {
  { "Int", "int" },
  { "If", "if" },
  { "Else", "else" },
  { "Less", "less" },
  { "While", "while" },
  { "Void", "void" },
  { "Identifier", "identifier" },
  { "LeftParen", "l_paren" },
  { "RightParen", "r_paren" },
  { "RightBrace", "r_brace" },
  { "LeftBrace", "l_brace" },
  { "LeftBracket", "l_square" },
  { "RightBracket", "r_square" },
  { "Constant", "numeric_constant" },
  { "Return", "return" },
  { "Semi", "semi" },
  { "EOF", "eof" },
  { "Equal", "equal" },
  { "Plus", "plus" },
  { "Star", "star" },
  { "Slash", "slash" },
  { "Comma", "comma" },
  { "Equalequal","equalequal" },
  { "Pipepipe","pipepipe" },
  { "Minus","minus" },
  { "Const","const" },
  { "Break","break" },
  { "Continue","continue" },
  { "Percent","percent" },
  { "Greater","greater" },
  { "Exclaim","exclaim" },
  { "Lessequal","lessequal" },
  { "Greaterequal","greaterequal" },
  { "Exclaimequal","exclaimequal" },
  { "Ampamp","ampamp" },
  // 在这里继续添加其他映射
};

void
print_token(const antlr4::Token* token,
            const antlr4::CommonTokenStream& tokens,
            std::ofstream& outFile,
            const antlr4::Lexer& lexer,
            std::string loc,
            int line,
            bool space,
            bool start)
{
  auto& vocabulary = lexer.getVocabulary();

  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));

  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; // 处理可能的空字符串情况

  if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
    tokenTypeName = tokenTypeMapping[tokenTypeName];
  }
  //std::string locInfo = " Loc=<0:0>";

  

  std::string locInfo = " Loc=<"+ loc + ":" + std::to_string(line) + ":" +
                          std::to_string(token->getCharPositionInLine() + 1) +
                          ">";

  bool startOfLine = start;
  bool leadingSpace = space;


  if (token->getText() != "<EOF>")
    outFile << tokenTypeName << " '" << token->getText() << "'";
  else
    outFile << tokenTypeName << " '"
            << "'";
  outFile << "\t";
  if (startOfLine)
    outFile << " [StartOfLine]";
  if (leadingSpace)
    outFile << " [LeadingSpace]";
  outFile << locInfo << std::endl;
}

int
main(int argc, char* argv[])
{
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  std::cout << "程序 '" << argv[0] << std::endl;
  std::cout << "输入 '" << argv[1] << std::endl;
  std::cout << "输出 '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();
  std::string str = argv[1];

  int line = 0; // 初始化行号为0
  bool space = false;
  bool start = false;
  std::string loc;

  for (auto&& token : tokens.getTokens()) {
    //对hidden channel：
      if (token->getChannel() == lexer.HIDDEN){
        //对预处理信息，实际上是要取最后一行的数字和文件路径
        if(token->getType() == lexer.LineAfterPreprocessing){
          std::string str = token->getText();
          //取行数
          std::regex patternOfNum("# ([0-9]+) ");
          std::smatch matchOfNum;
          if (std::regex_search(str, matchOfNum, patternOfNum)) {
            line=std::stoi(matchOfNum[1])-1;
          }
          //取文件路径
          std::regex patternOfLoc("\"(.+?)\"");
          std::smatch matchOfLoc;
          if (std::regex_search(str, matchOfLoc, patternOfLoc)) {
            loc = matchOfLoc[1];
          }
        }
        //空格
        else if (token->getType() == lexer.Whitespace){
          space = true;
        }
        //换行
        else if(token->getType() == lexer.Newline){
          space = false;
          start = true;
          line++;
        }
      }
    //对default channel
      else{
        print_token(token, tokens, outFile, lexer,loc,line,space,start);
        space = false;
        start = false;
      }
  }
}