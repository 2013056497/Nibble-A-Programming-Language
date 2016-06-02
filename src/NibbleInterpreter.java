
import java.util.ArrayList;
import java.util.Scanner;


public class NibbleInterpreter {

    /** The lexical analyzer with which tokens are scanned. */
    private NibbleScanner scanner;

    /** Whether a parser error has been found. */
    private boolean isInError;

    /** Wheter we have recovered from a parser error. */
    private boolean isRecovered;


    private boolean isSkipping;

    private ArrayList<String> varName = new ArrayList<String>();
    private ArrayList<String> varType = new ArrayList<String>();
    private ArrayList<String> varVal = new ArrayList<String>();

    public NibbleInterpreter(NibbleScanner scanner) {
        this.scanner = scanner;
        isInError = false;
        isSkipping = false;
        isRecovered = true;
        scanner.next(); // Prime the pump
    }

    public boolean errorHasOccurred() {
        return isInError;
    }

    // ////////////////////////////////////////////////
    // Parsing Support ///////////////////////////////
    // ////////////////////////////////////////////////


    private boolean see(TokenKind sought) {
        return (sought == scanner.token().kind());
    }


    private boolean have(TokenKind sought) {
        if (see(sought)) {
            scanner.next();
            return true;
        } else {
            return false;
        }
    }
    private void mustBe(TokenKind sought) {
        if (scanner.token().kind() == sought) {
            scanner.next();
            isRecovered = true;
        } else if (isRecovered) {
            isRecovered = false;
            reportParserError("%s found where %s sought", scanner.token()
                    .image(), sought.image());
        } else {
            // Do not report the (possibly spurious) error,
            // but rather attempt to recover by forcing a match.
            while (!see(sought) && !see(TokenKind.EOF)) {
                scanner.next();
            }
            if (see(sought)) {
                scanner.next();
                isRecovered = true;
            }
        }
    }

    private void reportParserError(String message, Object... args) {
        isInError = true;
        isRecovered = false;
        System.err
                .printf("line %d: ", scanner.token().line());
        System.err.printf(message, args);
        System.err.println();
    }

    private boolean isStringInside(String string, ArrayList<String> arraylist) {
        
       for (String s : arraylist) {
            if (s.equals(string)) {
                return true;
            }
       }
                return false;
    }

    // PARSER PROPER

    public void nibbleStart() {
        varName.clear();
        varType.clear();
        varVal.clear();
        while (!see(TokenKind.EOF)) {
            block();
        }
        mustBe(TokenKind.EOF);
    }

    private void block() {
        int line = scanner.token().line();

        mustBe(TokenKind.LBRACK);
        while (!see(TokenKind.RBRACK) && !see(TokenKind.EOF)) {
            statement();
        }
        mustBe(TokenKind.RBRACK);
        //return new JBlock(line, statements);
    }

    private void skipBlock() {
        int line = scanner.token().line();

        mustBe(TokenKind.LBRACK);
        while (!see(TokenKind.RBRACK) && !see(TokenKind.EOF)) {
            scanner.next();
        }
        mustBe(TokenKind.RBRACK);
        //return new JBlock(line, statements);
    }

    private void statement() {
        int line = scanner.token().line();
        if (see(TokenKind.VAR)) {
            var_declare();
        } else if (see(TokenKind.IDENTIFIER)) {
            var_set();
            mustBe(TokenKind.BSLASH);
        } else if (see(TokenKind.SCAN)) {
            stmt_input();
        } else if (see(TokenKind.CAST)) {
            stmt_output();
        } else if (see(TokenKind.SELECT)) {
            stmt_select();
        } else if (see(TokenKind.IF)) {
            stmt_if();
        } else if (see(TokenKind.LOOP)) {
            stmt_loop();
        } else if (have(TokenKind.BREAK)) {
            mustBe(TokenKind.BSLASH);
        } else if (have(TokenKind.CONTINUE)) {
            mustBe(TokenKind.BSLASH);
        } else { // Must be a statementExpression
            //statementExpression();
            mustBe(TokenKind.BSLASH);
        }
    }

    private void var_declare() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        have(TokenKind.VAR);
        do {
            arraylist.addAll(var_elemt());
            if (isStringInside(arraylist.get(0),varName)) {
                System.out.println("line "+line+": variable "+arraylist.get(0)+" already exists");
            } else {
            if (!isSkipping) {
                varName.add(arraylist.get(0));
                varType.add(arraylist.get(1));
                varVal.add(arraylist.get(2));
            }
            }
            System.out.println("");
        } while (have(TokenKind.COMMA));
        mustBe(TokenKind.BSLASH);
    }

    private void var_set() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        do {
            arraylist.addAll(var_elemt());
            if (isStringInside(arraylist.get(0),varName)) {
            if (!isSkipping) {
                int index = varName.indexOf(arraylist.get(0));
                varType.set(index,arraylist.get(1));
                varVal.set(index,arraylist.get(2));
            }
            } else {
                System.out.println("line "+line+": variable "+arraylist.get(0)+" does not exist");
            }
            System.out.println("");
        } while (have(TokenKind.COMMA));
    }

    private ArrayList<String> var_elemt() {
        ArrayList<String> arraylist = new ArrayList<String>();
        if(see(TokenKind.IDENTIFIER)) {
            arraylist.add(scanner.token().image());
        } else {
            arraylist.add("NULL");
        }
        mustBe(TokenKind.IDENTIFIER);
        if (have(TokenKind.ASSIGN)) {
            arraylist.addAll(expression());
        } else {
            arraylist.add("NULL");
            arraylist.add("NULL");
        }
        return arraylist;
    }

    private void stmt_input() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        mustBe(TokenKind.SCAN);
        Scanner sc = new Scanner(System.in);
        if(see(TokenKind.IDENTIFIER))
            arraylist.add(scanner.token().image());
        if (isStringInside(arraylist.get(0),varName)) {
        int index = varName.indexOf(arraylist.get(0));
        mustBe(TokenKind.IDENTIFIER);
            if (!isSkipping) {
        String input = sc.next();
            try {
                if (input.toLowerCase().contains(".".toLowerCase())) {
                    float x = Float.parseFloat(input);
                    varType.set(index,"LTL_FLOATING");
                    varVal.set(index,input);
                } else {
                    int y = Integer.parseInt(input);
                    varType.set(index,"LTL_INTEGER");
                    varVal.set(index,input);
                }

            } catch(Exception e) {
                    varType.set(index,"LTL_STRING");
                    varVal.set(index,input);
            }
        }
        } else {
            System.out.println("line "+line+": variable "+arraylist.get(0)+" does not exist");
        }

        mustBe(TokenKind.BSLASH);
    }

    private void stmt_output() {
        ArrayList<String> arraylist = new ArrayList<String>();
        mustBe(TokenKind.CAST);
        /*if(see(TokenKind.IDENTIFIER)) {
            System.out.print("<output>\n<identifier iden="+scanner.token().image()+" />\n</output>");
            mustBe(TokenKind.IDENTIFIER);
        } else if (see(TokenKind.LTL_STRING)) {
            System.out.print("<output>\n<lit_string="+scanner.token().image()+">\n</output>");
            mustBe(TokenKind.LTL_STRING);
        } else {
        }*/
        arraylist.addAll(expression());
        if (arraylist.get(0).equals("LTL_INTEGER") 
            | arraylist.get(0).equals("LTL_FLOATING")
            | arraylist.get(0).equals("LTL_STRING")
            | arraylist.get(0).equals("LTL_BOOLEAN")
            | arraylist.get(0).equals("NULL") ) {
            if (!isSkipping) {
                System.out.println(arraylist.get(1));
            }
        }
        mustBe(TokenKind.BSLASH);
    }

    private void stmt_if() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        boolean eflag = true;
        have(TokenKind.IF);
        mustBe(TokenKind.LPAREN);
        arraylist.addAll(expression());
        mustBe(TokenKind.RPAREN);
        if (arraylist.get(0).equals("LTL_BOOLEAN")) {
            if (arraylist.get(1).equalsIgnoreCase("TRUE")) {
                eflag = false;
            } else {
                isSkipping = true;
            }
            block();
                isSkipping = false;
        } else {
            System.out.println("line "+line+": incompatible types, "+arraylist.get(0)+" cannot be converted to LTL_BOOLEAN");

        }
        while (see(TokenKind.ELIF)) {
            ArrayList<String> arraylist2 = new ArrayList<String>();
            mustBe(TokenKind.ELIF);
            mustBe(TokenKind.LPAREN);
            arraylist2.addAll(expression());
            mustBe(TokenKind.RPAREN);
            if (arraylist2.get(0).equals("LTL_BOOLEAN")) {
                if (arraylist2.get(1).equalsIgnoreCase("TRUE")) {
                    eflag = false;
                    block();
                } else {
                isSkipping = true;
                }
                    block();
                isSkipping = false;
            } else {
                System.out.println("line "+line+": incompatible types, "+arraylist2.get(0)+" cannot be converted to LTL_BOOLEAN");

            }
        }
        if (see(TokenKind.EL)) {
            have(TokenKind.EL);
            if (!eflag) {
                isSkipping = true;
            }
                block();
                isSkipping = false;
        }
    }

    private void stmt_loop() {
        int line = scanner.token().line();
        have(TokenKind.LOOP);

                ArrayList<String> arraylist = new ArrayList<String>();
                mustBe(TokenKind.LPAREN);
                arraylist.addAll(expression());
                mustBe(TokenKind.RPAREN);
                        block();
    }

    private void stmt_select() {
        have(TokenKind.SELECT);
        mustBe(TokenKind.LPAREN);
        expression();
        mustBe(TokenKind.RPAREN);
        mustBe(TokenKind.LBRACK);
        selCase_elemt();
        mustBe(TokenKind.RBRACK);
    }

    private void selCase_elemt() {
        do {
            if (see(TokenKind.CASE)) {
                mustBe(TokenKind.CASE);
                expression();
                mustBe(TokenKind.COLON);
                block();
            }
        } while (see(TokenKind.CASE));
        if (see(TokenKind.DEFAULT)) {
            mustBe(TokenKind.DEFAULT);
            mustBe(TokenKind.COLON);
            block();
        }
    }

    private ArrayList<String> expression() {
        ArrayList<String> arraylist = new ArrayList<String>();
        arraylist.addAll(logical_expr());
        return arraylist;
    }

    private ArrayList<String> logical_expr() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        do {
        } while(have(TokenKind.OR)||have(TokenKind.AND));
        arraylist.addAll(relational_expr());

        while(see(TokenKind.OR)||see(TokenKind.AND)) {
            ArrayList<String> temp = new ArrayList<String>();
            if(have(TokenKind.OR)) {
                temp.add("OR");
            }
            if(have(TokenKind.AND)){
                temp.add("AND");
            }
            temp.addAll(relational_expr());
            if (temp.get(0).equals("OR")) {
                if (arraylist.get(0).equals("LTL_BOOLEAN") && temp.get(1).equals("LTL_BOOLEAN") ) {
                    boolean x = arraylist.get(1).equalsIgnoreCase("TRUE") || temp.get(2).equalsIgnoreCase("TRUE");
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '|', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("AND")) {
                if (arraylist.get(0).equals("LTL_BOOLEAN") && temp.get(1).equals("LTL_BOOLEAN") ) {
                    boolean x = arraylist.get(1).equalsIgnoreCase("TRUE") && temp.get(2).equalsIgnoreCase("TRUE");
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '&', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            }
        }
        return arraylist;
    }

    private ArrayList<String> relational_expr() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        arraylist.addAll(additive_expr());
        ArrayList<String> temp = new ArrayList<String>();
        if (see(TokenKind.LT) | see(TokenKind.GT) 
            | see(TokenKind.LTE) | see(TokenKind.GTE) 
            | see(TokenKind.NE) | see(TokenKind.EQUALS)) {
            if(have(TokenKind.LT)) {
                temp.add("LT");
            } else if(have(TokenKind.GT)){
                temp.add("GT");
            } else if(have(TokenKind.LTE)){
                temp.add("LTE");
            } else if(have(TokenKind.GTE)){
                temp.add("GTE");
            } else if(have(TokenKind.NE)){
                temp.add("NE");
            } else if(have(TokenKind.EQUALS)){
                temp.add("EQUALS");
            }
            temp.addAll(additive_expr());
            if (temp.get(0).equals("LT")) {
                // SEMANTIC RULE
                if (arraylist.get(0).equals("LTL_INTEGER") || arraylist.get(0).equals("LTL_FLOATING") 
                    && temp.get(1).equals("LTL_INTEGER") || temp.get(1).equals("LTL_FLOATING")) {
                    try {
                    boolean x = (Float.parseFloat(arraylist.get(1))<Float.parseFloat(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                    } catch(Exception e) {
                        System.out.println("line "+line+": bad operand types for binary operator '<', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                        arraylist.set(0,"TYPE_ERROR");
                    }
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '<', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("GT")) {
                // SEMANTIC RULE
                if ((arraylist.get(0).equals("LTL_INTEGER") || arraylist.get(0).equals("LTL_FLOATING") )
                    && (temp.get(1).equals("LTL_INTEGER") || temp.get(1).equals("LTL_FLOATING"))) {
                    try {
                    boolean x = (Float.parseFloat(arraylist.get(1))>Float.parseFloat(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                    } catch(Exception e) {
                        System.out.println("line "+line+": bad operand types for binary operator '<', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                        arraylist.set(0,"TYPE_ERROR");
                    }
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '>', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("LTE")) {
                // SEMANTIC RULE
                if ((arraylist.get(0).equals("LTL_INTEGER") || arraylist.get(0).equals("LTL_FLOATING") )
                    && (temp.get(1).equals("LTL_INTEGER") || temp.get(1).equals("LTL_FLOATING"))) {
                    try {
                    boolean x = (Float.parseFloat(arraylist.get(1))<=Float.parseFloat(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                    } catch(Exception e) {
                        System.out.println("line "+line+": bad operand types for binary operator '<', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                        arraylist.set(0,"TYPE_ERROR");
                    }
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '<=', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("GTE")) {
                // SEMANTIC RULE
                if ((arraylist.get(0).equals("LTL_INTEGER") || arraylist.get(0).equals("LTL_FLOATING") )
                    && (temp.get(1).equals("LTL_INTEGER") || temp.get(1).equals("LTL_FLOATING"))) {
                    try {
                    boolean x = (Float.parseFloat(arraylist.get(1))>=Float.parseFloat(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                    } catch(Exception e) {
                        System.out.println("line "+line+": bad operand types for binary operator '<', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                        arraylist.set(0,"TYPE_ERROR");
                    }
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '>=', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("NE")) {
                // SEMANTIC RULE
                if ((arraylist.get(0).equals("LTL_INTEGER") || arraylist.get(0).equals("LTL_FLOATING") )
                    && (temp.get(1).equals("LTL_INTEGER") || temp.get(1).equals("LTL_FLOATING"))) {
                    boolean x = (Float.parseFloat(arraylist.get(1))!=Float.parseFloat(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_STRING") && temp.get(1).equals("LTL_STRING") ) {
                    boolean x = !(arraylist.get(1).equals(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '!=', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("EQUALS")) {
                // SEMANTIC RULE
                if ((arraylist.get(0).equals("LTL_INTEGER") || arraylist.get(0).equals("LTL_FLOATING") )
                    && (temp.get(1).equals("LTL_INTEGER") || temp.get(1).equals("LTL_FLOATING"))) {
                    boolean x = (Float.parseFloat(arraylist.get(1))==Float.parseFloat(temp.get(2)));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_STRING") && temp.get(1).equals("LTL_STRING") ) {
                    boolean x = arraylist.get(1).equals(temp.get(2));
                    arraylist.set(0,"LTL_BOOLEAN");
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '==', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            }
        }
        return arraylist;
    }

    private ArrayList<String> additive_expr() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        arraylist.addAll(multiplicative_expr());

        while (see(TokenKind.PLUS)||see(TokenKind.MINUS)) {

            ArrayList<String> temp = new ArrayList<String>();

            if(have(TokenKind.PLUS)) {
                temp.add("PLUS");
            }
            else if(have(TokenKind.MINUS)){
                temp.add("MINUS");
            }

            temp.addAll(multiplicative_expr());
            if (temp.get(0).equals("PLUS")) {
                // SEMANTIC RULE
                if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_INTEGER")) {
                    int x = Integer.parseInt(arraylist.get(1))+Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Float.parseFloat(arraylist.get(1))+Float.parseFloat(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Integer.parseInt(arraylist.get(1))+Float.parseFloat(temp.get(2));
                    arraylist.set(0,"LTL_FLOATING");
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_INTEGER")) {
                    float x = Float.parseFloat(arraylist.get(1))+Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_STRING") && temp.get(1).equals("LTL_STRING")) {
                    String x = arraylist.get(1)+temp.get(2);
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_STRING") && temp.get(1).equals("LTL_INTEGER")) {
                    String x = ""+arraylist.get(1)+temp.get(2);
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_STRING") && temp.get(1).equals("LTL_FLOATING")) {
                    String x = ""+arraylist.get(1)+temp.get(2);
                    arraylist.set(1,""+x);
                }  else {
                    System.out.println("line "+line+": bad operand types for binary operator '+', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            }
            else if (temp.get(0).equals("MINUS")) {
                // SEMANTIC RULE
                if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_INTEGER")) {
                    int x = Integer.parseInt(arraylist.get(1))-Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Float.parseFloat(arraylist.get(1))-Float.parseFloat(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Integer.parseInt(arraylist.get(1))-Float.parseFloat(temp.get(2));
                    arraylist.set(0,"LTL_FLOATING");
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_INTEGER")) {
                    float x = Float.parseFloat(arraylist.get(1))-Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '-', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            }
        }
        return arraylist;

    }

    private ArrayList<String> multiplicative_expr(){
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
            arraylist.addAll(unary_expr());
        while(see(TokenKind.MULT)||see(TokenKind.DIV)||see(TokenKind.MOD)) {
            ArrayList<String> temp = new ArrayList<String>();
            
            if(have(TokenKind.MULT)) {
                temp.add("MULT");
            }
            else if(have(TokenKind.DIV)){
                temp.add("DIV");
            }
            else if(have(TokenKind.MOD)){
                temp.add("MOD");
            }
            temp.addAll(unary_expr());
            if (temp.get(0).equals("MULT")) {
                // SEMANTIC RULE
                if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_INTEGER")) {
                    int x = Integer.parseInt(arraylist.get(1))*Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Float.parseFloat(arraylist.get(1))*Float.parseFloat(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Integer.parseInt(arraylist.get(1))*Float.parseFloat(temp.get(2));
                    arraylist.set(0,"LTL_FLOATING");
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_INTEGER")) {
                    float x = Float.parseFloat(arraylist.get(1))*Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '*', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            } else if (temp.get(0).equals("DIV")) {
                // SEMANTIC RULE
                if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_INTEGER")) {
                    int x = Integer.parseInt(arraylist.get(1))/Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Float.parseFloat(arraylist.get(1))/Float.parseFloat(temp.get(2));
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_FLOATING")) {
                    float x = Integer.parseInt(arraylist.get(1))/Float.parseFloat(temp.get(2));
                    arraylist.set(0,"LTL_FLOATING");
                    arraylist.set(1,""+x);
                } else if (arraylist.get(0).equals("LTL_FLOATING") && temp.get(1).equals("LTL_INTEGER")) {
                    float x = Float.parseFloat(arraylist.get(1))/Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);
                } else {
                    System.out.println("line "+line+": bad operand types for binary operator '/', 1st type:"+arraylist.get(0)+" 2nd type:" +temp.get(1));
                    arraylist.set(0,"TYPE_ERROR");
                }
            }
            
        }

        return arraylist;
    }

    private ArrayList<String> unary_expr() {
        int line = scanner.token().line();
        ArrayList<String> arraylist = new ArrayList<String>();
        if(see(TokenKind.IDENTIFIER)) {
            arraylist.add(scanner.token().image());;
            mustBe(TokenKind.IDENTIFIER);
            if (isStringInside(arraylist.get(0),varName)) {
                int index = varName.indexOf(arraylist.get(0));
                arraylist.set(0,varType.get(index));
                arraylist.add(varVal.get(index));
            } else {
                System.out.println("line "+line+": variable "+arraylist.get(0)+" does not exist");
                arraylist.set(0,"DOESNOTEXIST_ERROR");
                arraylist.add("NULL");
            }/*
            if(have(TokenKind.INCR)){
                System.out.print("++");
            }
            else if(have(TokenKind.DECR)){
                System.out.print("--");
            }*/
        } else if(have(TokenKind.NOT)) {
            //mustBe(TokenKind.IDENTIFIER);
            arraylist.addAll(unary_expr());
            //SEMANTIC RULE
            if (arraylist.get(0).equals("LTL_BOOLEAN")) {
                if (arraylist.get(1).equalsIgnoreCase("TRUE")) {
                    arraylist.set(1,"FALSE");
                } else {
                    arraylist.set(1,"TRUE");
                }
            } else {
                System.out.println("line "+line+": bad operand type "+arraylist.get(0)+" for unary operator '!'");
                arraylist.set(0,"TYPE_ERROR");
            }
        } else if(have(TokenKind.MINUS)) {
            System.out.print("-");
            arraylist.addAll(unary_expr());
            //SEMANTIC RULE
            if (arraylist.get(0).equals("LTL_INTEGER")) {
                int x = -(Integer.parseInt(arraylist.get(1)));
                arraylist.set(1,""+x);
            } else {
                System.out.println("line "+line+": bad operand type "+arraylist.get(0)+" for unary operator '-'");
                arraylist.set(0,"TYPE_ERROR");
            }
        }    else if(have(TokenKind.LPAREN)) {
            arraylist.addAll(expression());
            mustBe(TokenKind.RPAREN);
        } else {
            return literal();
        }

        return arraylist;
    }

    private ArrayList<String> literal() {
        ArrayList<String> arraylist = new ArrayList<String>();
        if(see(TokenKind.LTL_INTEGER)) {
            arraylist.add("LTL_INTEGER");
            arraylist.add(scanner.token().image());
            mustBe(TokenKind.LTL_INTEGER);
            return arraylist;
            
        } else if(see(TokenKind.LTL_FLOATING)) {
            arraylist.add("LTL_FLOATING");
            arraylist.add(scanner.token().image());
            mustBe(TokenKind.LTL_FLOATING);
            return arraylist;
            
        } else if(see(TokenKind.LTL_STRING)) {
            arraylist.add("LTL_STRING");
            String image = scanner.token().image();
            arraylist.add(image.substring(1,image.length()-1));
            mustBe(TokenKind.LTL_STRING);
            if(have(TokenKind.INCR)){
                System.out.print("++");
            }
            return arraylist;
        } else if(see(TokenKind.TRUE)) {
            arraylist.add("LTL_BOOLEAN");
            arraylist.add("TRUE");
            mustBe(TokenKind.TRUE);
            return arraylist;
        } else if(see(TokenKind.FALSE)) {
            arraylist.add("LTL_BOOLEAN");
            arraylist.add("FALSE");
            mustBe(TokenKind.FALSE);
            return arraylist;
        } else if(see(TokenKind.NULL)) {
            arraylist.add("NULL");
            arraylist.add("NULL");
            mustBe(TokenKind.NULL);
            return arraylist;
        } else {
            arraylist.add("PARSE-ERROR");
            arraylist.add("NULL");
        }
        return arraylist;
    }

}