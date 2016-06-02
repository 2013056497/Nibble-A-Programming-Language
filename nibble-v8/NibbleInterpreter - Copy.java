
import java.util.ArrayList;


public class NibbleInterpreter {

    /** The lexical analyzer with which tokens are scanned. */
    private NibbleScanner scanner;

    /** Whether a parser error has been found. */
    private boolean isInError;

    /** Wheter we have recovered from a parser error. */
    private boolean isRecovered;

    private ArrayList<String> varName = new ArrayList<String>();
    private ArrayList<String> varType = new ArrayList<String>();
    private ArrayList<String> varVal = new ArrayList<String>();

    public NibbleInterpreter(NibbleScanner scanner) {
        this.scanner = scanner;
        isInError = false;
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
           if(s.equals(string)){
               return true;
           }
       }
       return false;
    }

    // PARSER PROPER

    public void nibbleStart() {
        while (!see(TokenKind.EOF)) {
            block();
        }
        mustBe(TokenKind.EOF);
    }

    private void block() {
        int line = scanner.token().line();

        mustBe(TokenKind.LBRACK);
        if(!isInError)
            System.out.println("<block>");
        while (!see(TokenKind.RBRACK) && !see(TokenKind.EOF)) {
            statement();
        }
        mustBe(TokenKind.RBRACK);
        if(!isInError)
            System.out.println("</block>");
        //return new JBlock(line, statements);
    }

    private void statement() {
        int line = scanner.token().line();
        if (see(TokenKind.LBRACK)) {
            block();
        } else if (see(TokenKind.VAR)) {
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
            System.out.println("<break_stmt />");
        } else if (have(TokenKind.CONTINUE)) {
            mustBe(TokenKind.BSLASH);
            System.out.println("<continue_stmt />");
        } else { // Must be a statementExpression
            //statementExpression();
            mustBe(TokenKind.BSLASH);
        }
    }

    private void var_declare() {
        have(TokenKind.VAR);
        System.out.println("<declaration>");
        do {
            var_elemt();
            System.out.println("");
        } while (have(TokenKind.COMMA));
        System.out.println("</declaration>");
        mustBe(TokenKind.BSLASH);
    }

    private void var_set() {
        do {
            var_elemt();
            System.out.println("");
        } while (have(TokenKind.COMMA));
    }

    private void var_elemt() {
        if(see(TokenKind.IDENTIFIER))
            System.out.print("<identifier iden="+scanner.token().image());
        mustBe(TokenKind.IDENTIFIER);
        if (have(TokenKind.ASSIGN)) {
            System.out.println(">");
            System.out.println("<expression>");  
            expression();
            System.out.println("</expression>");
        } else if (have(TokenKind.ASSIGN_PLUS)) {
            System.out.println(" shorthand=plus>");
            System.out.println("<expression>");
            expression();
            System.out.println("</expression>");
        } else if (have(TokenKind.ASSIGN_MINUS)) {
            System.out.println(" shorthand=minus>");
            System.out.println("<expression>");
            expression();
            System.out.print("</expression>");
        } else if (have(TokenKind.ASSIGN_MULT)) {
            System.out.println(" shorthand=mult>");
            System.out.println("<expression>");
            expression();
            System.out.print("</expression>");
        } else if (have(TokenKind.ASSIGN_DIV)) {
            System.out.println(" shorthand=div>");
            System.out.println("<expression>");
            expression();
            System.out.print("</expression>");
        } else if (have(TokenKind.ASSIGN_MOD)) {
            System.out.println(" shorthand=mod>");
            System.out.println("<expression>");
            expression();
            System.out.println("</expression>");
        } else {
            System.out.println(" />");
        }
        System.out.print("</identifier>");
    }

    private void stmt_input() {
        mustBe(TokenKind.SCAN);
        if(see(TokenKind.IDENTIFIER))
            System.out.print("<input>\n<identifier iden="+scanner.token().image()+" />\n</input>");
        mustBe(TokenKind.IDENTIFIER);
        mustBe(TokenKind.BSLASH);
        System.out.println(">");
    }

    private void stmt_output() {
        ArrayList<String> arraylist = new ArrayList<String>();
        mustBe(TokenKind.CAST);
        if(see(TokenKind.IDENTIFIER)) {
            System.out.print("<output>\n<identifier iden="+scanner.token().image()+" />\n</output>");
            mustBe(TokenKind.IDENTIFIER);
        } else if (see(TokenKind.LTL_STRING)) {
            System.out.print("<output>\n<lit_string="+scanner.token().image()+">\n</output>");
            mustBe(TokenKind.LTL_STRING);
        } else {
            System.out.println("<output>");
            arraylist.addAll(expression());
            System.out.println("</output>");
            System.out.println("OUTPUT: "+arraylist.get(1));
        }
        mustBe(TokenKind.BSLASH);
        System.out.println(">");
    }

    private void stmt_if() {
        have(TokenKind.IF);
        System.out.println("<if>");
        mustBe(TokenKind.LPAREN);
        System.out.println("<condition>");
        expression();
        System.out.println("</condition>");
        mustBe(TokenKind.RPAREN);
        block();
        System.out.println("</if>");
        while (see(TokenKind.ELIF)) {
            mustBe(TokenKind.ELIF);
            System.out.print("<elif>");
            mustBe(TokenKind.LPAREN);
            System.out.println("<condition>");
            expression();
            System.out.println("</condition>");
            mustBe(TokenKind.RPAREN);
            block();
            System.out.println("</elif>");
        }
        if (see(TokenKind.EL)) {
            have(TokenKind.EL);
            System.out.println("<el>");
            block();
            System.out.println("</el>");
        }
    }

    private void stmt_loop() {
        have(TokenKind.LOOP);
        mustBe(TokenKind.LPAREN);
        System.out.println("<loop>");
        System.out.println("<condition>");
        expression();
        System.out.println("</condition>");
        mustBe(TokenKind.RPAREN);
        block();
        System.out.println("</loop>");
    }

    private void stmt_select() {
        have(TokenKind.SELECT);
        mustBe(TokenKind.LPAREN);
        System.out.println("<select>");
        System.out.print("<expression>");
        expression();
        System.out.println("</expression>");
        mustBe(TokenKind.RPAREN);
        mustBe(TokenKind.LBRACK);
        selCase_elemt();
        mustBe(TokenKind.RBRACK);
        System.out.println("</select>");
    }

    private void selCase_elemt() {
        do {
            if (see(TokenKind.CASE)) {
                mustBe(TokenKind.CASE);
                System.out.println("<case>");
        System.out.print("<expression>");
                expression();
        System.out.println("</expression>");
                mustBe(TokenKind.COLON);
                System.out.println("</case>");
                block();
                System.out.println("</case>");
            }
        } while (see(TokenKind.CASE));
        if (see(TokenKind.DEFAULT)) {
            mustBe(TokenKind.DEFAULT);
                System.out.print("<default>");
            mustBe(TokenKind.COLON);
            block();
            System.out.println("</default>");
        }
    }

    private ArrayList<String> expression() {
        ArrayList<String> arraylist = new ArrayList<String>();
        arraylist.addAll(logical_expr());
            System.out.println("");
        return arraylist;
    }

    private ArrayList<String> logical_expr() {
        ArrayList<String> arraylist = new ArrayList<String>();
        do {
        } while(have(TokenKind.OR)||have(TokenKind.AND));
        arraylist.addAll(relational_expr());

        while(see(TokenKind.OR)||see(TokenKind.AND)) {
            ArrayList<String> temp = new ArrayList<String>();
            if(have(TokenKind.OR)) {
                System.out.print("|");
            }
            if(have(TokenKind.AND)){
                System.out.print("&");
            }
            temp.addAll(relational_expr());
        }
        return arraylist;
    }

    private ArrayList<String> relational_expr() {
        ArrayList<String> arraylist = new ArrayList<String>();
        arraylist.addAll(additive_expr());
        if(have(TokenKind.LT)) {
            System.out.print("<");
            additive_expr();
        } else if(have(TokenKind.GT)){
            System.out.print(">");
            additive_expr();
        } else if(have(TokenKind.LTE)){
            System.out.print("<=");
            additive_expr();
        } else if(have(TokenKind.GTE)){
            System.out.print(">=");
            additive_expr();
        } else if(have(TokenKind.NE)){
            System.out.print("!=");
            additive_expr();
        } else if(have(TokenKind.EQUALS)){
            System.out.print("==");
            additive_expr();
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
                System.out.print("+");
            }
            else if(have(TokenKind.MINUS)){
                temp.add("MINUS");
                System.out.print("-");
            }

            temp.addAll(multiplicative_expr());
            if (temp.get(0).equals("PLUS")) {
                // SEMANTIC RULE
                if (arraylist.get(0).equals("LTL_INTEGER") && temp.get(1).equals("LTL_INTEGER")) {
                    int x = Integer.parseInt(arraylist.get(1))+Integer.parseInt(temp.get(2));
                    arraylist.set(1,""+x);

                    //System.out.print("OUTPUT:"+arraylist.get(1));

                }  else {
                    System.out.println("line "+line+": cannot add "+arraylist.get(0)+" to " +temp.get(1));
                }




            }
            else if (temp.get(0).equals("MINUS")) {
                //System.out.print("OUTPUT:"+temp.get(0));
            }
        }
        return arraylist;

    }

    private ArrayList<String> multiplicative_expr(){
        ArrayList<String> arraylist = new ArrayList<String>();
            arraylist.addAll(unary_expr());
        while(see(TokenKind.MULT)||see(TokenKind.DIV)||see(TokenKind.MOD)) {
            
            if(have(TokenKind.MULT)) {
                System.out.print("*");
            }
            else if(have(TokenKind.DIV)){
                System.out.print("/");
            }
            else if(have(TokenKind.MOD)){
                System.out.print("%");
            }
            unary_expr();
        }

        return arraylist;
    }

    private ArrayList<String> unary_expr() {
        ArrayList<String> arraylist = new ArrayList<String>();
        if(see(TokenKind.IDENTIFIER)) {
            arraylist.add("IDENTIFIER");
            arraylist.add(scanner.token().image());
            System.out.print("<identifier iden="+arraylist.get(1)+" />");
            mustBe(TokenKind.IDENTIFIER);
            if(have(TokenKind.INCR)){
                System.out.print("++");
            }
            else if(have(TokenKind.DECR)){
                System.out.print("--");
            }
            return arraylist;
        } else if(have(TokenKind.NOT)) {
            System.out.print("!<identifier iden="+scanner.token().image()+" />");
            mustBe(TokenKind.IDENTIFIER);
        } else if(have(TokenKind.MINUS)) {
            System.out.print("-");
            unary_expr();
        } else if(have(TokenKind.INCR)) {
            System.out.print("++");
            arraylist.add("IDENTIFIER");
            arraylist.add(scanner.token().image());
            System.out.print("<identifier iden="+arraylist.get(1)+" />");
            mustBe(TokenKind.IDENTIFIER);
        } else if(have(TokenKind.DECR)) {
            System.out.print("--");
            unary_expr();
        } else if(have(TokenKind.LPAREN)) {
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
            System.out.print("<lit_integer="+arraylist.get(1)+">");
            mustBe(TokenKind.LTL_INTEGER);
            return arraylist;
            
        } else if(see(TokenKind.LTL_FLOATING)) {
            arraylist.add("LTL_FLOATING");
            arraylist.add(scanner.token().image());
            System.out.print("<lit_float="+arraylist.get(1)+">");
            mustBe(TokenKind.LTL_FLOATING);
            return arraylist;
            
        } else if(see(TokenKind.LTL_STRING)) {
            arraylist.add("LTL_STRING");
            arraylist.add(scanner.token().image());
            System.out.print("<lit_string="+scanner.token().image()+">");
            mustBe(TokenKind.LTL_STRING);
            if(have(TokenKind.INCR)){
                System.out.print("++");
            }
            return arraylist;
        } else if(see(TokenKind.TRUE)) {
            arraylist.add("BOOLEAN");
            arraylist.add("TRUE");
            System.out.print("<lit_true>");
            mustBe(TokenKind.TRUE);
            return arraylist;
        } else if(see(TokenKind.FALSE)) {
            arraylist.add("BOOLEAN");
            arraylist.add("FALSE");
            System.out.print("<lit_false>");
            mustBe(TokenKind.FALSE);
            return arraylist;
        } else if(see(TokenKind.NULL)) {
            arraylist.add("NULL");
            arraylist.add("NULL");
            System.out.print("<null>");
            mustBe(TokenKind.NULL);
            return arraylist;
        }
        return arraylist;
    }

}