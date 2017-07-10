package fme.wsl.lexer;

import java.util.HashMap;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

import fme.wsl.ast.AST;
import fme.wsl.ast.ASTNode;
import fme.wsl.tables.LexerRuleTable;
import fme.wsl.tables.LexerTokenTable;

/**
 * This class parses a given WSL file and constructs a Lexer Token list
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class WSLLexer {

    private static WSLLexer singleton = new WSLLexer();

    private int pos;

    private Vector<String> passingLexTupel;

    private Document doc;

    private Vector<LexerToken> tokens;

    private HashMap<Integer, LexerToken> tokenLookUp;

    /**
     * The Constructor
     */
    private WSLLexer() {
        tokens = new Vector<LexerToken>();
        tokenLookUp = new HashMap<Integer, LexerToken>();
    }

    /**
     * Parses a given WSL document
     * 
     * @param wslfile
     *            The file to parse
     */
    public static void parseWSL(Document wslfile) {
        Logger.getLogger(WSLLexer.class.getCanonicalName()).log(Level.INFO,
            "Parsing WSL file ...");
        singleton.doc = wslfile;
        singleton.parse();
    }

    /**
     * Gets a WSL file chopped into tokens
     * 
     * @return A list of tokens
     */
    public static Vector<LexerToken> getTokens() {
        return singleton.tokens;
    }

    /**
     * Gets a WSL token which starts at the position pos
     * 
     * @param pos
     *            The start position of the token
     * @return The requested lexer token or null
     */
    public static LexerToken getToken(int pos) {
        return singleton.tokenLookUp.get(new Integer(pos));
    }

    private void parse() {

        int i, j, startpos, nodeRow;
        Vector<Integer> nodeRowStack = new Vector<Integer>();
        Vector<ASTNode> nodesForNextLexerToken = new Vector<ASTNode>();
        Vector<ASTNode> nodesEndForNextLexerToken = new Vector<ASTNode>();

        String ppm, st;
        String token, value;
        LexerToken lexerToken;

        tokens = new Vector<LexerToken>();
        tokenLookUp = new HashMap<Integer, LexerToken>();

        try {

            pos = 0;
            nodeRow = 0;
            lexerToken = null;

            while (pos <= doc.getLength()) {

                token = null;
                value = null;
                startpos = pos;

                // Ignore PrettyPrint Marks
                if (pos + 2 < doc.getLength())
                    ppm = doc.getText(pos, 2);
                else
                    ppm = "";
                while (ppm.equals("<#") || ppm.equals("<|")) {
                    st = "";
                    for (j = 0; !st.equals(">"); j++) {
                        st = doc.getText(pos + j, 1);
                    }
                    st = doc.getText(pos, j);
                    nodeRow = Integer
                        .parseInt(st.substring(2, st.length() - 2));

                    // Handle the NodeRowStack
                    // Begin Mark
                    if (ppm.equals("<#")) {
                        nodeRowStack.add(new Integer(nodeRow));
                        nodesForNextLexerToken.add(AST.getNodeFromRow(nodeRow));
                    }
                    // End Mark
                    else {
                        nodeRow = nodeRowStack.get(nodeRowStack.size() - 1)
                            .intValue();
                        nodeRowStack.removeElementAt(nodeRowStack.size() - 1);
                        nodesEndForNextLexerToken.add(AST
                            .getNodeFromRow(nodeRow));
                        if (nodeRowStack.size() > 0)
                            nodeRow = nodeRowStack.get(nodeRowStack.size() - 1)
                                .intValue();
                    }

                    pos = pos + j;

                    if (pos + 2 < doc.getLength())
                        ppm = doc.getText(pos, 2);
                    else
                        break;
                }

                for (i = 0; i < LexerRuleTable.getSize() + 1; i++) {
                    // Get the next token
                    // After this call the pos pointer should point to the
                    // last
                    // character of the identified token
                    token = checkChar(i, "0", pos);
                    if (token != null)
                        break;
                }

                // Optain value of the token if necessary
                if (token != null && passingLexTupel.get(8).length() > 0) {

                    if (passingLexTupel.get(8).equals("SCAN_IDENTIFIER")
                        || passingLexTupel.get(8).equals("SCAN_NUMBER")) {
                        i = scanCharSeq(pos);
                        value = doc.getText(pos, i);
                        pos = pos + i - 1;
                    } else if (passingLexTupel.get(8).equals("SCAN_STRING")
                        || passingLexTupel.get(8).equals("SCAN_COMMENT")) {
                        i = scanCharSeq(pos + 1, '"');
                        value = doc.getText(pos + 1, i);
                        pos = pos + i + 1;
                    }
                }

                if (token != null) {
                    if (!token.equals("WHITESPACE")) {
                        lexerToken = new LexerToken(token, value, startpos, pos);

                        tokens.add(lexerToken);
                        tokenLookUp.put(new Integer(startpos), lexerToken);

                        // Add start position for astNodes
                        for (int x = 0; x < nodesForNextLexerToken.size(); x++) {
                            nodesForNextLexerToken.get(x).addLexerToken(
                                tokens.size() - 1);
                        }
                        nodesForNextLexerToken.removeAllElements();

                        // Add end position for astNodes
                        for (int x = 0; x < nodesEndForNextLexerToken.size(); x++) {
                            nodesEndForNextLexerToken.get(x).addLexerToken(
                                tokens.size() - 2);
                        }
                        nodesEndForNextLexerToken.removeAllElements();

                        lexerToken.setASTNode(AST.getNodeFromRow(nodeRow));
                    }
                } else {
                    Logger.getLogger(this.getClass().getCanonicalName()).log(
                        Level.INFO,
                        "Token on pos:" + pos + " could not be identified!");
                }

                pos++;
            }

            // Add end position for last astNodes
            for (int x = 0; x < nodesEndForNextLexerToken.size(); x++) {
                nodesEndForNextLexerToken.get(x).addLexerToken(
                    tokens.size() - 1);

//System.out.println("End:" + nodesEndForNextLexerToken.get(x) + " token:" + tokens.get(tokens.size() - 1));

            }
            nodesEndForNextLexerToken.removeAllElements();

        } catch (BadLocationException e) {
            Logger.getLogger(this.getClass().getCanonicalName()).log(
                Level.SEVERE,
                "Got to an undefined location in the current document");
        }
    }

    /**
     * This function tries to identify a parsed char according to a given lexer
     * table tuple
     * 
     * @param checkNr
     *            The check number of the lexer table tuple
     * @param checkLevel
     *            The check level of the lexer table tuple
     * @param charPos
     *            The character position in the document
     * @return The identified token
     * @throws BadLocationException
     *             If the document ends unexpected
     */
    private String checkChar(int checkNr, String checkLevel, int charPos)
        throws BadLocationException {

        int i;
        char c, pc;
        String ret = null, tok;
        Vector<String> nextTupel, lexTTTupel;

        c = doc.getText(charPos, 1).charAt(0);

        if (checkLevel.equals("0"))
            passingLexTupel = LexerRuleTable.getLexerFirstCheck(checkNr);
        else
            passingLexTupel = LexerRuleTable.getLexerCheck(checkNr + "."
                + checkLevel);

        // Fill PassingCharacter (PC)
        pc = 0;
        if (passingLexTupel.get(3).length() == 1)
            pc = passingLexTupel.get(3).charAt(0);
        else if (passingLexTupel.get(4).length() > 0)
            pc = (char) Integer.parseInt(passingLexTupel.get(4));

        // If the condition modifier is true then the item is identified
        if (passingLexTupel.get(2).equals("TRUE")) {
            ret = passingLexTupel.get(7);
        }

        // Check the PC (PassingCharacter) of the Lexer Table Tuple is found on
        // the current position
        else if (c == pc) {
            // Check if it is identified
            if (passingLexTupel.get(7).length() > 0)
                ret = passingLexTupel.get(7);
            // If not do further checking
            else {
                i = 1;
                while (ret == null
                    && (nextTupel = LexerRuleTable.getLexerCheck(checkNr + "."
                        + checkLevel + "." + i)) != null) {
                    ret = checkChar(checkNr, checkLevel + "." + i, charPos + 1);
                    i++;

                    // Set the pos + 1 for every level of the recursion
                    if (ret != null && !nextTupel.get(2).equals("TRUE"))
                        pos++;
                }
            }
        }

        // Check if the Regular Expression of the Lexer Table Tuple matches on
        // the current position
        else if (passingLexTupel.get(5).length() > 0) {
            tok = Character.toString(c);

            if (tok.matches(passingLexTupel.get(5))) {
                ret = passingLexTupel.get(7);
            }
        }

        // Otherwise identify the token and try to find it in the passing group
        else if (passingLexTupel.get(6).length() > 0) {
            i = pos;
            if ((lexTTTupel = identToken()) != null) {
                tok = lexTTTupel.get(0);
                if (LexerTokenTable.getLexerTokenGroup(passingLexTupel.get(6))
                    .contains(tok)) {
                    if (passingLexTupel.get(7).length() > 0)
                        ret = passingLexTupel.get(7);
                    else
                        ret = tok;
                } else
                    pos = i;
            }
        }

        return ret;
    }

    /**
     * Identifies a token using the Lexer token table (WHITESPACE or RESERVED
     * WORD)
     * 
     * @return The token name or null
     * @throws BadLocationException
     *             If the end of the document is reached without finding a
     *             terminating whitespace
     */
    private Vector<String> identToken() throws BadLocationException {
        int spos;
        Vector<String> ttTupel, ret = null;

        // First try to lookup a single char
        if ((ttTupel = LexerTokenTable.getLexerTokenSingleChar(doc.getText(pos,
            1).charAt(0))) != null) {
            ret = ttTupel;
        }

        // Now try to read a reserved word
        if (ret == null) {

            // Search for the next whitespace
            spos = scanCharSeq(pos);

            // Test if it's a reserved word
            if (LexerTokenTable.getLexerTokenCharSequence(doc
                .getText(pos, spos)) != null) {
                ret = LexerTokenTable.getLexerTokenCharSequence(doc.getText(
                    pos, spos));
                pos = pos + spos - 1;
            }
        }

        return ret;
    }

    /**
     * Scans from the pos pointer to the next whitespace or special char
     * 
     * @param pos
     *            The position to scan from
     * @return The position of the next whitespace
     * @throws BadLocationException
     *             If the end of the document is reached without finding a
     *             terminating whitespace
     */
    private int scanCharSeq(int pos) throws BadLocationException {
        char c;
        int spos = 0;
        c = doc.getText(pos + spos, 1).charAt(0);
        while (LexerTokenTable.getLexerTokenSingleChar(c) == null
            || (!LexerTokenTable.getLexerTokenSingleChar(c).get(0).equals(
                "WHITESPACE") && !LexerTokenTable.getLexerTokenSingleChar(c)
                .get(0).equals("SPECIALCHAR"))) {
            spos++;
            c = doc.getText(pos + spos, 1).charAt(0);
        }
        return spos;
    }

    /**
     * Scans from the pos pointer to a specific char
     * 
     * @param pos
     *            The position to scan from
     * @param sc
     *            The char to search for
     * @return The position of the next whitespace
     * @throws BadLocationException
     *             If the end of the document is reached without finding a
     *             terminating whitespace
     */
    private int scanCharSeq(int pos, char sc) throws BadLocationException {
        char c;
        int spos = 0;
        c = doc.getText(pos + spos, 1).charAt(0);
        while (LexerTokenTable.getLexerTokenSingleChar(c) == null || c != sc) {
            spos++;
            c = doc.getText(pos + spos, 1).charAt(0);
        }
        return spos;
    }
}
