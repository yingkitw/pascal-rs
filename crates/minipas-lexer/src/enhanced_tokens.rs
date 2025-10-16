use logos::Logos;

/// Enhanced token definitions based on Free Pascal Compiler
/// This provides comprehensive Pascal language support
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum EnhancedToken {
    // Operators (can be overloaded)
    #[token("+")]
    Plus,
    
    #[token("-")]
    Minus,
    
    #[token("*")]
    Star,
    
    #[token("/")]
    Slash,
    
    #[token("=")]
    Equal,
    
    #[token(">")]
    GreaterThan,
    
    #[token("<")]
    LessThan,
    
    #[token(">=")]
    GreaterEqual,
    
    #[token("<=")]
    LessEqual,
    
    #[token("<>")]
    NotEqual,
    
    #[token("**")]
    StarStar,
    
    #[token("as")]
    OpAs,
    
    #[token("in")]
    OpIn,
    
    #[token("is")]
    OpIs,
    
    #[token("or")]
    OpOr,
    
    #[token("and")]
    OpAnd,
    
    #[token("div")]
    OpDiv,
    
    #[token("mod")]
    OpMod,
    
    #[token("not")]
    OpNot,
    
    #[token("shl")]
    OpShl,
    
    #[token("shr")]
    OpShr,
    
    #[token("xor")]
    OpXor,
    
    #[token(":=")]
    Assignment,
    
    // Special characters
    #[token("^")]
    Caret,
    
    #[token("[")]
    LeftBracket,
    
    #[token("]")]
    RightBracket,
    
    #[token(".")]
    Point,
    
    #[token(",")]
    Comma,
    
    #[token("(")]
    LeftParen,
    
    #[token(")")]
    RightParen,
    
    #[token(":")]
    Colon,
    
    #[token(";")]
    Semicolon,
    
    #[token("@")]
    At,
    
    #[token("..")]
    PointPoint,
    
    #[token("...")]
    PointPointPoint,
    
    #[token("|")]
    Pipe,
    
    #[token("&")]
    Ampersand,
    
    // C-like operators
    #[token("+=")]
    PlusAssign,
    
    #[token("-=")]
    MinusAssign,
    
    #[token("&=")]
    AndAssign,
    
    #[token("|=")]
    OrAssign,
    
    #[token("*=")]
    StarAssign,
    
    #[token("/=")]
    SlashAssign,
    
    #[token("%=")]
    ModAssign,
    
    #[token(":=")]
    DivAssign,
    
    #[token("!=")]
    NotAssign,
    
    #[token("^=")]
    XorAssign,
    
    // Literals
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    StringLiteral(String),
    
    #[regex(r#"'([^'\\]|\\.)'"#, |lex| lex.slice().chars().nth(1).unwrap_or('\0'))]
    CharLiteral(char),
    
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    WideStringLiteral(String),
    
    #[regex(r#"'([^'\\]|\\.)'"#, |lex| lex.slice().chars().nth(1).unwrap_or('\0'))]
    WideCharLiteral(char),
    
    #[regex(r"\d+", |lex| lex.slice().parse().unwrap_or(0))]
    IntegerLiteral(i64),
    
    #[regex(r"\d+\.\d+([eE][+-]?\d+)?", |lex| lex.slice().parse().unwrap_or(0.0))]
    RealLiteral(f64),
    
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    
    // Boolean literals
    #[token("true")]
    True,
    
    #[token("false")]
    False,
    
    // Keywords (sorted by length, then alphabetically)
    #[token("c")]
    C,
    
    #[token("as")]
    As,
    
    #[token("at")]
    At,
    
    #[token("do")]
    Do,
    
    #[token("if")]
    If,
    
    #[token("in")]
    In,
    
    #[token("is")]
    Is,
    
    #[token("of")]
    Of,
    
    #[token("on")]
    On,
    
    #[token("or")]
    Or,
    
    #[token("to")]
    To,
    
    #[token("add")]
    Add,
    
    #[token("and")]
    And,
    
    #[token("asm")]
    Asm,
    
    #[token("dec")]
    Dec,
    
    #[token("div")]
    Div,
    
    #[token("end")]
    End,
    
    #[token("far")]
    Far,
    
    #[token("for")]
    For,
    
    #[token("inc")]
    Inc,
    
    #[token("mod")]
    Mod,
    
    #[token("nil")]
    Nil,
    
    #[token("not")]
    Not,
    
    #[token("out")]
    Out,
    
    #[token("set")]
    Set,
    
    #[token("shl")]
    Shl,
    
    #[token("shr")]
    Shr,
    
    #[token("try")]
    Try,
    
    #[token("var")]
    Var,
    
    #[token("xor")]
    Xor,
    
    #[token("case")]
    Case,
    
    #[token("copy")]
    Copy,
    
    #[token("cvar")]
    Cvar,
    
    #[token("else")]
    Else,
    
    #[token("exit")]
    Exit,
    
    #[token("fail")]
    Fail,
    
    #[token("file")]
    File,
    
    #[token("goto")]
    Goto,
    
    #[token("huge")]
    Huge,
    
    #[token("last")]
    Last,
    
    #[token("name")]
    Name,
    
    #[token("near")]
    Near,
    
    #[token("read")]
    Read,
    
    #[token("self")]
    SelfKeyword,
    
    #[token("sysv")]
    Sysv,
    
    #[token("then")]
    Then,
    
    #[token("type")]
    Type,
    
    #[token("unit")]
    Unit,
    
    #[token("univ")]
    Univ,
    
    #[token("uses")]
    Uses,
    
    #[token("with")]
    With,
    
    #[token("alias")]
    Alias,
    
    #[token("array")]
    Array,
    
    #[token("begin")]
    Begin,
    
    #[token("break")]
    Break,
    
    #[token("cdecl")]
    Cdecl,
    
    #[token("class")]
    Class,
    
    #[token("const")]
    Const,
    
    #[token("equal")]
    Equal,
    
    #[token("far16")]
    Far16,
    
    #[token("final")]
    Final,
    
    #[token("first")]
    First,
    
    #[token("index")]
    Index,
    
    #[token("label")]
    Label,
    
    #[token("local")]
    Local,
    
    #[token("raise")]
    Raise,
    
    #[token("until")]
    Until,
    
    #[token("while")]
    While,
    
    #[token("write")]
    Write,
    
    #[token("addref")]
    Addref,
    
    #[token("cblock")]
    Cblock,
    
    #[token("dispid")]
    Dispid,
    
    #[token("divide")]
    Divide,
    
    #[token("downto")]
    Downto,
    
    #[token("except")]
    Except,
    
    #[token("export")]
    Export,
    
    #[token("helper")]
    Helper,
    
    #[token("inline")]
    Inline,
    
    #[token("legacy")]
    Legacy,
    
    #[token("nested")]
    Nested,
    
    #[token("object")]
    Object,
    
    #[token("packed")]
    Packed,
    
    #[token("pascal")]
    Pascal,
    
    #[token("public")]
    Public,
    
    #[token("record")]
    Record,
    
    #[token("repeat")]
    Repeat,
    
    #[token("result")]
    Result,
    
    #[token("return")]
    Return,
    
    #[token("sealed")]
    Sealed,
    
    #[token("static")]
    Static,
    
    #[token("stored")]
    Stored,
    
    #[token("strict")]
    Strict,
    
    #[token("string")]
    String,
    
    #[token("system")]
    System,
    
    #[token("winapi")]
    Winapi,
    
    #[token("asmname")]
    Asmname,
    
    #[token("basereg")]
    Basereg,
    
    #[token("cppdecl")]
    Cppdecl,
    
    #[token("default")]
    Default,
    
    #[token("dynamic")]
    Dynamic,
    
    #[token("exports")]
    Exports,
    
    #[token("finally")]
    Finally,
    
    #[token("forward")]
    Forward,
    
    #[token("generic")]
    Generic,
    
    #[token("iocheck")]
    Iocheck,
    
    #[token("library")]
    Library,
    
    #[token("message")]
    Message,
    
    #[token("modulus")]
    Modulus,
    
    #[token("package")]
    Package,
    
    #[token("private")]
    Private,
    
    #[token("program")]
    Program,
    
    #[token("r12base")]
    R12base,
    
    #[token("rtlproc")]
    Rtlproc,
    
    #[token("section")]
    Section,
    
    #[token("stdcall")]
    Stdcall,
    
    #[token("syscall")]
    Syscall,
    
    #[token("varargs")]
    Varargs,
    
    #[token("virtual")]
    Virtual,
    
    #[token("absolute")]
    Absolute,
    
    #[token("abstract")]
    Abstract,
    
    #[token("baselast")]
    Baselast,
    
    #[token("basenone")]
    Basenone,
    
    #[token("basesysv")]
    Basesysv,
    
    #[token("constref")]
    Constref,
    
    #[token("contains")]
    Contains,
    
    #[token("continue")]
    Continue,
    
    #[token("cppclass")]
    Cppclass,
    
    #[token("explicit")]
    Explicit,
    
    #[token("external")]
    External,
    
    #[token("finalize")]
    Finalize,
    
    #[token("function")]
    Function,
    
    #[token("implicit")]
    Implicit,
    
    #[token("lessthan")]
    Lessthan,
    
    #[token("location")]
    Location,
    
    #[token("multiply")]
    Multiply,
    
    #[token("mwpascal")]
    Mwpascal,
    
    #[token("negative")]
    Negative,
    
    #[token("noinline")]
    Noinline,
    
    #[token("noreturn")]
    Noreturn,
    
    #[token("notequal")]
    Notequal,
    
    #[token("operator")]
    Operator,
    
    #[token("optional")]
    Optional,
    
    #[token("overload")]
    Overload,
    
    #[token("override")]
    Override,
    
    #[token("platform")]
    Platform,
    
    #[token("positive")]
    Positive,
    
    #[token("property")]
    Property,
    
    #[token("readonly")]
    Readonly,
    
    #[token("register")]
    Register,
    
    #[token("required")]
    Required,
    
    #[token("requires")]
    Requires,
    
    #[token("resident")]
    Resident,
    
    #[token("safecall")]
    Safecall,
    
    #[token("subtract")]
    Subtract,
    
    #[token("sysvbase")]
    Sysvbase,
    
    #[token("assembler")]
    Assembler,
    
    #[token("basefirst")]
    Basefirst,
    
    #[token("bitpacked")]
    Bitpacked,
    
    #[token("bitwiseor")]
    Bitwiseor,
    
    #[token("hardfloat")]
    Hardfloat,
    
    #[token("inherited")]
    Inherited,
    
    #[token("intdivide")]
    Intdivide,
    
    #[token("interface")]
    Interface,
    
    #[token("interrupt")]
    Interrupt,
    
    #[token("leftshift")]
    Leftshift,
    
    #[token("logicalor")]
    Logicalor,
    
    #[token("nodefault")]
    Nodefault,
    
    #[token("objcclass")]
    Objcclass,
    
    #[token("otherwise")]
    Otherwise,
    
    #[token("procedure")]
    Procedure,
    
    #[token("promising")]
    Promising,
    
    #[token("protected")]
    Protected,
    
    #[token("published")]
    Published,
    
    #[token("reference")]
    Reference,
    
    #[token("softfloat")]
    Softfloat,
    
    #[token("threadvar")]
    Threadvar,
    
    #[token("writeonly")]
    Writeonly,
    
    #[token("bitwiseand")]
    Bitwiseand,
    
    #[token("bitwisexor")]
    Bitwisexor,
    
    #[token("deprecated")]
    Deprecated,
    
    #[token("destructor")]
    Destructor,
    
    #[token("enumerator")]
    Enumerator,
    
    #[token("implements")]
    Implements,
    
    #[token("initialize")]
    Initialize,
    
    #[token("internproc")]
    Internproc,
    
    #[token("logicaland")]
    Logicaland,
    
    #[token("logicalnot")]
    Logicalnot,
    
    #[token("logicalxor")]
    Logicalxor,
    
    #[token("oldfpccall")]
    Oldfpccall,
    
    #[token("openstring")]
    Openstring,
    
    #[token("rightshift")]
    Rightshift,
    
    #[token("specialize")]
    Specialize,
    
    #[token("suspending")]
    Suspending,
    
    #[token("vectorcall")]
    Vectorcall,
    
    #[token("constructor")]
    Constructor,
    
    // Special tokens
    #[token("eof")]
    Eof,
    
    // Whitespace and comments
    #[regex(r"\s+", |lex| lex.slice().to_string())]
    Whitespace(String),
    
    #[regex(r"//.*", |lex| lex.slice().to_string())]
    LineComment(String),
    
    #[regex(r"/\*([^*]|\*[^/])*\*/", |lex| lex.slice().to_string())]
    BlockComment(String),
    
    // Preprocessor directives
    #[regex(r"#\w+", |lex| lex.slice().to_string())]
    PreprocessorDirective(String),
    
    // Error token
    #[error]
    Error,
}

impl EnhancedToken {
    /// Returns true if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(self,
            Self::Program | Self::Unit | Self::Library | Self::Package |
            Self::Uses | Self::Const | Self::Type | Self::Var | Self::Threadvar |
            Self::Function | Self::Procedure | Self::Constructor | Self::Destructor |
            Self::Begin | Self::End | Self::If | Self::Then | Self::Else |
            Self::Case | Self::Of | Self::Otherwise | Self::While | Self::Repeat |
            Self::Until | Self::For | Self::To | Self::Downto | Self::Do |
            Self::With | Self::Try | Self::Except | Self::Finally | Self::Raise |
            Self::Goto | Self::Exit | Self::Break | Self::Continue | Self::Halt |
            Self::And | Self::Or | Self::Not | Self::Xor | Self::Shl | Self::Shr |
            Self::Div | Self::Mod | Self::In | Self::Is | Self::As |
            Self::Array | Self::Record | Self::Set | Self::File | Self::String |
            Self::Class | Self::Object | Self::Interface | Self::Dispinterface |
            Self::Property | Self::Read | Self::Write | Self::Stored | Self::Default |
            Self::Nodefault | Self::Implements | Self::Index | Self::Name | Self::Readonly |
            Self::Writeonly | Self::Add | Self::Remove | Self::Enumerator |
            Self::Public | Self::Published | Self::Protected | Self::Private |
            Self::Strict | Self::Abstract | Self::Sealed | Self::Helper |
            Self::External | Self::Forward | Self::Virtual | Self::Dynamic |
            Self::Override | Self::Reintroduce | Self::Overload | Self::Message |
            Self::Static | Self::Inline | Self::Assembler | Self::Cdecl |
            Self::Pascal | Self::Register | Self::Safecall | Self::Stdcall |
            Self::Varargs | Self::Export | Self::Near | Self::Far | Self::Huge |
            Self::Absolute | Self::Abstract | Self::Assembler | Self::Cdecl |
            Self::Cppdecl | Self::Default | Self::Export | Self::External |
            Self::Far16 | Self::Forward | Self::Generic | Self::Index |
            Self::Local | Self::Name | Self::Near | Self::Noinline |
            Self::Noreturn | Self::Oldfpccall | Self::Override | Self::Pascal |
            Self::Platform | Self::Private | Self::Protected | Self::Public |
            Self::Published | Self::Readonly | Self::Register | Self::Reintroduce |
            Self::Safecall | Self::Sealed | Self::Static | Self::Stdcall |
            Self::Stored | Self::Strict | Self::Varargs | Self::Virtual |
            Self::Writeonly | Self::Deprecated | Self::Experimental | Self::Unimplemented
        )
    }
    
    /// Returns true if this token is an operator
    pub fn is_operator(&self) -> bool {
        matches!(self,
            Self::Plus | Self::Minus | Self::Star | Self::Slash |
            Self::Equal | Self::GreaterThan | Self::LessThan |
            Self::GreaterEqual | Self::LessEqual | Self::NotEqual |
            Self::OpAs | Self::OpIn | Self::OpIs | Self::OpOr |
            Self::OpAnd | Self::OpDiv | Self::OpMod | Self::OpNot |
            Self::OpShl | Self::OpShr | Self::OpXor | Self::Assignment |
            Self::PlusAssign | Self::MinusAssign | Self::StarAssign |
            Self::SlashAssign | Self::ModAssign | Self::DivAssign |
            Self::AndAssign | Self::OrAssign | Self::NotAssign | Self::XorAssign
        )
    }
    
    /// Returns true if this token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(self,
            Self::StringLiteral(_) | Self::CharLiteral(_) |
            Self::WideStringLiteral(_) | Self::WideCharLiteral(_) |
            Self::IntegerLiteral(_) | Self::RealLiteral(_)
        )
    }
    
    /// Returns the precedence of an operator token
    pub fn operator_precedence(&self) -> Option<u8> {
        match self {
            Self::OpNot => Some(8),
            Self::Star | Self::Slash | Self::OpDiv | Self::OpMod | Self::OpAnd => Some(7),
            Self::Plus | Self::Minus | Self::OpOr | Self::OpXor => Some(6),
            Self::OpShl | Self::OpShr => Some(5),
            Self::Equal | Self::NotEqual | Self::LessThan | Self::GreaterThan |
            Self::LessEqual | Self::GreaterEqual | Self::OpIn | Self::OpIs => Some(4),
            Self::Assignment => Some(1),
            _ => None,
        }
    }
    
    /// Returns true if this token is left-associative
    pub fn is_left_associative(&self) -> bool {
        match self {
            Self::Assignment => false,
            _ => true,
        }
    }
}
