use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    #[token("program")]
    Program,

    #[token("var")]
    Var,

    #[token("const")]
    Const,

    #[token("type")]
    Type,

    #[token("begin")]
    Begin,

    #[token("end")]
    End,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    #[token("do")]
    Do,

    #[token("for")]
    For,

    #[token("to")]
    To,

    #[token("downto")]
    DownTo,

    #[token("repeat")]
    Repeat,

    #[token("until")]
    Until,

    #[token("case")]
    Case,

    #[token("of")]
    Of,

    #[token("with")]
    With,

    #[token("procedure")]
    Procedure,

    #[token("function")]
    Function,

    #[token("forward")]
    Forward,

    #[token("external")]
    External,

    #[token("inline")]
    Inline,

    #[token("assembler")]
    Assembler,

    #[token("uses")]
    Uses,

    #[token("unit")]
    Unit,

    #[token("interface")]
    Interface,

    #[token("implementation")]
    Implementation,

    #[token("initialization")]
    Initialization,

    #[token("finalization")]
    Finalization,

    #[token("class")]
    Class,

    #[token("object")]
    Object,

    #[token("inherited")]
    Inherited,

    #[token("virtual")]
    Virtual,

    #[token("override")]
    Override,

    #[token("abstract")]
    Abstract,

    #[token("sealed")]
    Sealed,

    #[token("private")]
    Private,

    #[token("protected")]
    Protected,

    #[token("public")]
    Public,

    #[token("published")]
    Published,

    #[token("property")]
    Property,

    #[token("read")]
    Read,

    #[token("write")]
    Write,

    #[token("constructor")]
    Constructor,

    #[token("destructor")]
    Destructor,

    #[token("operator")]
    Operator,

    #[token("try")]
    Try,

    #[token("except")]
    Except,

    #[token("finally")]
    Finally,

    #[token("raise")]
    Raise,

    #[token("exit")]
    Exit,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("goto")]
    Goto,

    #[token("label")]
    Label,

    #[token("array")]
    Array,

    #[token("record")]
    Record,

    #[token("set")]
    Set,

    #[token("file")]
    File,

    #[token("packed")]
    Packed,

    #[token("string")]
    String,

    #[token("integer")]
    Integer,

    #[token("real")]
    Real,

    #[token("boolean")]
    Boolean,

    #[token("char")]
    Char,

    #[token("pointer")]
    Pointer,

    #[token("nil")]
    Nil,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("not")]
    Not,

    #[token("xor")]
    Xor,

    #[token("shl")]
    Shl,

    #[token("shr")]
    Shr,

    #[token("mod")]
    Mod,

    #[token("in")]
    In,

    #[token("is")]
    Is,

    #[token("as")]
    As,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    IntegerLiteral(i64),

    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse::<f64>().unwrap())]
    RealLiteral(f64),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove quotes and unescape
        s[1..s.len()-1].to_string()
    })]
    StringLiteral(String),

    #[regex(r"'([^'\\]|\\.)'", |lex| {
        let s = lex.slice();
        // Remove quotes and get the character
        s.chars().nth(1).unwrap_or('\0')
    })]
    CharLiteral(char),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // Operators
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("div")]
    Divide,

    #[token("=")]
    Equal,

    #[token("<>")]
    NotEqual,

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterEqual,

    #[token(":=")]
    ColonEquals,

    #[token("..")]
    Range,

    #[token("^")]
    Caret,

    #[token("@")]
    AddressOf,

    #[token("&")]
    Ampersand,

    #[token("|")]
    Pipe,

    #[token("<<")]
    ShiftLeft,

    #[token(">>")]
    ShiftRight,

    #[token("><")]
    SymmetricDifference,

    // Delimiters
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    // Comments and whitespace
    #[regex(r"//[^\r\n]*")]
    LineComment,

    #[regex(r"\{[^}]*\}")]
    BlockComment,

    #[regex(r"[ \t\r\n]+")]
    Whitespace,
}
