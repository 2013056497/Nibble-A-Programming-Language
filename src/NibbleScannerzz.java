import java.io.*;
import java.util.Hashtable;
import java.util.Stack;
import java.util.Vector;

public class NibbleScanner {

    public final static char EOFCH = (char) -1;
	
	// reserved keywords
    private Hashtable<String, TokenKind> reserved;

	// source of characters
    private NibbleCharReader input;
	
	// unscanned characters
    private char ch;
	
	// is true if error was found
    private boolean inError;
	
	// line number of current token
    private int line;

    private Vector<NibbleToken> backtrackingQueue;
    private Vector<NibbleToken> nextQueue;
    private Stack<Vector<NibbleToken>> queueStack;
    public boolean isLookingAhead;
	
	// previous token
    private NibbleToken previousToken;
	
    // current token
    private NibbleToken token;
	
	
    public NibbleScanner(String code) {
        this.input = new NibbleCharReader(code + EOFCH);
        inError = false;

        // Keywords in j--
        reserved = new Hashtable<String, TokenKind>();

        reserved.put(TokenKind.SCAN.image(), TokenKind.SCAN);
        reserved.put(TokenKind.CAST.image(), TokenKind.CAST);
        reserved.put(TokenKind.VAR.image(), TokenKind.VAR);
        reserved.put(TokenKind.IF.image(), TokenKind.IF);
        reserved.put(TokenKind.ELIF.image(), TokenKind.ELIF);
        reserved.put(TokenKind.EL.image(), TokenKind.EL);
        reserved.put(TokenKind.SELECT.image(), TokenKind.SELECT);
        reserved.put(TokenKind.CASE.image(), TokenKind.CASE);
        reserved.put(TokenKind.DEFAULT.image(), TokenKind.DEFAULT);
        reserved.put(TokenKind.BREAK.image(), TokenKind.BREAK);
        reserved.put(TokenKind.CONTINUE.image(), TokenKind.CONTINUE);
        reserved.put(TokenKind.LOOP.image(), TokenKind.LOOP);
        reserved.put(TokenKind.TRUE.image(), TokenKind.TRUE);
        reserved.put(TokenKind.FALSE.image(), TokenKind.FALSE);
        reserved.put(TokenKind.NULL.image(), TokenKind.NULL);

        // Prime the pump.
        nextCh();

        backtrackingQueue = new Vector<NibbleToken>();
        nextQueue = new Vector<NibbleToken>();
        queueStack = new Stack<Vector<NibbleToken>>();
        isLookingAhead = false;
    }
    
    
    public void next() {
        previousToken = token;
        token = getNextToken();
        if (backtrackingQueue.size() == 0) {
            token = getNextToken();
        } else {
            token = backtrackingQueue.remove(0);
        }
        if (isLookingAhead) {
            nextQueue.add(token);
        }
    }


    public void recordPosition() {
        isLookingAhead = true;
        queueStack.push(nextQueue);
        nextQueue = new Vector<NibbleToken>();
        nextQueue.add(previousToken);
        nextQueue.add(token);
    }



    public void returnToPosition() {
        while (backtrackingQueue.size() > 0) {
            nextQueue.add(backtrackingQueue.remove(0));
        }
        backtrackingQueue = nextQueue;
        nextQueue = queueStack.pop();
        isLookingAhead = !(queueStack.empty());

        // Restore previous and current tokens
        previousToken = backtrackingQueue.remove(0);
        token = backtrackingQueue.remove(0);
    }
	
    public NibbleToken token() {
        return token;
    }
	
    public NibbleToken getNextToken() {
        StringBuffer buffer;
        boolean moreWhiteSpace = true;
        while (moreWhiteSpace) {
            while (isWhitespace(ch)) {
                nextCh();
            }
            if (ch == '>') {
                nextCh();
                if (ch == '>') {
                    // CharReader maps all new lines to '\n'
                    while (ch != '\n' && ch != EOFCH) {
                        nextCh();
                    }
                } else {
                    reportScannerError("Operator > is not supported in nibble.");
                }
            } else {
                moreWhiteSpace = false;
            }
        }
        line = input.line();
        switch (ch) {

            case ',':
                nextCh();
                return new NibbleToken(TokenKind.COMMA, line);
            case '.':
                nextCh();
                return new NibbleToken(TokenKind.DOT, line);
            case '[':
                nextCh();
                return new NibbleToken(TokenKind.LBRACK, line);
            case ']':
                nextCh();
                return new NibbleToken(TokenKind.RBRACK, line);
            case '(':
                nextCh();
                return new NibbleToken(TokenKind.LPAREN, line);
            case ')':
                nextCh();
                return new NibbleToken(TokenKind.RPAREN, line);
            case ';':
                nextCh();
                return new NibbleToken(TokenKind.SMCOLON, line);
            case '~':
                nextCh();
                return new NibbleToken(TokenKind.TILDE, line);
            case '\\':
                nextCh();
                return new NibbleToken(TokenKind.BSLASH, line);

            case '=':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.EQUALS, line);
                } else {
                    return new NibbleToken(TokenKind.ASSIGN, line);
                }
            case '^':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.ASSIGN_EXP, line);
                } else {
                    return new NibbleToken(TokenKind.EXP, line);
                }
            case '*':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.ASSIGN_MULT, line);
                } else {
                    return new NibbleToken(TokenKind.MULT, line);
                }
            case '/':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.ASSIGN_DIV, line);
                } else {
                    return new NibbleToken(TokenKind.DIV, line);
                }
            case '%':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.ASSIGN_MOD, line);
                } else {
                    return new NibbleToken(TokenKind.MOD, line);
                }
            case '+':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.ASSIGN_PLUS, line);
                } else if (ch == '+') {
                    nextCh();
                    return new NibbleToken(TokenKind.INCR, line);
                } else {
                    return new NibbleToken(TokenKind.PLUS, line);
                }
            case '-':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.ASSIGN_MINUS, line);
                } else if (ch == '-') {
                    nextCh();
                    return new NibbleToken(TokenKind.DECR, line);
                } else {
                    return new NibbleToken(TokenKind.MINUS, line);
                }

            case '|':
                nextCh();
                return new NibbleToken(TokenKind.OR, line);
            case '&':
                nextCh();
                return new NibbleToken(TokenKind.AND, line);
            case '!':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.NE, line);
                } else {
                    return new NibbleToken(TokenKind.NOT, line);
                }

            case '<':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.LTE, line);
                } else {
                    return new NibbleToken(TokenKind.LT, line);
                    // reportScannerError("Operator < is not supported in j--.");
                    // return getNextToken();
                }
            case '>':
                nextCh();
                if (ch == '=') {
                    nextCh();
                    return new NibbleToken(TokenKind.GTE, line);
                } else {
                    return new NibbleToken(TokenKind.GT, line);
                    // reportScannerError("Operator < is not supported in j--.");
                    // return getNextToken();
                }


            case '"':
                buffer = new StringBuffer();
                buffer.append("\"");
                nextCh();
                while (ch != '"' && ch != '\n' && ch != EOFCH) {
                    if (ch == '~') {
                        nextCh();
                        buffer.append(escape());
                    } else {
                        buffer.append(ch);
                        nextCh();
                    }
                }
                if (ch == '\n') {
                    reportScannerError("Unexpected end of line found in String");
                } else if (ch == EOFCH) {
                    reportScannerError("Unexpected end of file found in String");
                } else {
                    // Scan the closing "
                    nextCh();
                    buffer.append("\"");
                }
                return new NibbleToken(TokenKind.LTL_STRING, buffer.toString(), line);
        case EOFCH:
            return new NibbleToken(TokenKind.EOF, line);
        case '0':
            // Handle only simple decimal integers for now.
            nextCh();
            return new NibbleToken(TokenKind.LTL_INTEGER, "0", line);
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            buffer = new StringBuffer();
            while (isDigit(ch)) {
                buffer.append(ch);
                nextCh();
            }
            return new NibbleToken(TokenKind.LTL_INTEGER, buffer.toString(), line);
        default:
            if (isIdentifierStart(ch)) {
                buffer = new StringBuffer();
                while (isIdentifierPart(ch)) {
                    buffer.append(ch);
                    nextCh();
                }
                String identifier = buffer.toString();
                if (reserved.containsKey(identifier.toLowerCase())) {
                    return new NibbleToken(reserved.get(identifier.toLowerCase()), line);
                } else {
                    return new NibbleToken(TokenKind.IDENTIFIER, identifier, line);
                }
            } else {
                reportScannerError("Unidentified input token: '%c'", ch);
                nextCh();
                return getNextToken();
            }
        }
    }

    private String escape() {
        switch (ch) {
        case 't':
            nextCh();
            return "~t";
        case 'n':
            nextCh();
            return "~n";
        case '"':
            nextCh();
            return "~\"";
        case '~':
            nextCh();
            return "~~";
        default:
            reportScannerError("Badly formed escape: ~%c", ch);
            nextCh();
            return "";
        }
    }

    private void nextCh() {
        line = input.line();
        try {
            ch = input.nextChar();
        } catch (Exception e) {
            reportScannerError("Unable to read characters from input");
        }
    }

    private void reportScannerError(String message, Object... args) {
        inError = true;
        System.err.printf("line %d: ", line);
        System.err.printf(message, args);
        System.err.println();
    }

    private boolean isDigit(char c) {
        return (c >= '0' && c <= '9');
    }

    private boolean isWhitespace(char c) {
        switch (c) {
        case ' ':
        case '\t':
        case '\n': // CharReader maps all new lines to '\n'
        case '\f':
            return true;
        }
        return false;
    }

    private boolean isIdentifierStart(char c) {
        return (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z');
    }

    private boolean isIdentifierPart(char c) {
        return (isIdentifierStart(c) || isDigit(c) || c == '_');
    }

    public boolean isInError() {
        return inError;
    }
}


class NibbleCharReader {
	
    public final static char EOFCH = (char) -1;
	
    private LineNumberReader lineNumberReader;
	
    public NibbleCharReader(String code) {
        lineNumberReader = new LineNumberReader(new StringReader(code));
    }
	
    public char nextChar() throws IOException{
        return (char) lineNumberReader.read();
    }
	
    public int line() {
        // LineNumberReader counts lines from 0.
        return lineNumberReader.getLineNumber() + 1;
    }
	
    public void close() throws IOException {
        lineNumberReader.close();
    }
}