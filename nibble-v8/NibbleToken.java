

enum TokenKind {
    IDENTIFIER("<identifier>"),
    LTL_INTEGER("<integer>"),
    LTL_FLOATING("<floating>"),
    LTL_STRING("<string>"),

    ASSIGN("="),
    EXP("^"),
    MULT("*"),
    DIV("/"),
    MOD("%"),
    PLUS("+"),
    MINUS("-"),
    OR("|"),
    AND("&"),
    NOT("!"),
    LT("<"),
    GT(">"),
    LTE("<="),
    GTE(">="),
    NE("!="),
    EQUALS("=="),
    ASSIGN_EXP("^="),
    ASSIGN_MULT("*="),
    ASSIGN_DIV("/="),
    ASSIGN_MOD("%="),
    ASSIGN_PLUS("+="),
    ASSIGN_MINUS("-="),
    INCR("++"),
    DECR("--"),

    SCAN("scan"),
    CAST("cast"),
    VAR("var"),
    IF("if"),
    ELIF("elif"),
    EL("el"),
    SELECT("select"),
    CASE("case"),
    DEFAULT("default"),
    BREAK("break"),
    CONTINUE("continue"),
    LOOP("loop"),
    TRUE("true"),
    FALSE("false"),
    NULL("null"),

    COMMA(","),
    DOT("."),
    LBRACK("["),
    RBRACK("]"),
    LPAREN("("),
    RPAREN(")"),
    SMCOLON(";"),
    COLON(":"),
    DIQUOTE("\""),
    TILDE("~"),
    BSLASH("\\"),
    EOF("<end_of_file>");

    private String image;

    private TokenKind(String image) {
        this.image = image;
    }

    public String image() {
        return image;
    }

    public String toString() {
        return image;
    }

}


public class NibbleToken {
    private TokenKind kind;

    private String image;

    private int line;

    public NibbleToken(TokenKind kind, String image, int line) {
        this.kind = kind;
        this.image = image;
        this.line = line;
    }

    public NibbleToken(TokenKind kind, int line) {
        this(kind, kind.toString(), line);
    }

    public String tokenRep() {
        return kind.toString();
    }

    public String image() {
        return image;
    }

    public int line() {
        return line;
    }

    public TokenKind kind() {
        return kind;
    }

    public String toString() {
        return image;
    }

}
