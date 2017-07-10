/**
 * Project: fme
 */

package fme.wsl.lexer;

import fme.wsl.ast.ASTNode;
import fme.wsl.tables.LexerTokenTable;

/**
 * This class represents a lexical token
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class LexerToken {

    private Integer nr;

    private String name;

    private String value;

    private int posStart;

    private int posEnd;

    private ASTNode astNode;

    public LexerToken(String name, String value, int posStart, int posEnd) {
        nr = Integer.parseInt(LexerTokenTable.getLexerToken(name).get(4));
        this.name = name;
	//        if (value != null)
	//	  this.value = value.replace("?", "");
        this.posStart = posStart;
        this.posEnd = posEnd;
    }

    public String getName() {
        return name;
    }

    public Integer getNr() {
        return nr;
    }

    public int getPosEnd() {
        return posEnd;
    }

    public int getPosStart() {
        return posStart;
    }

    public String getValue() {
        return value;
    }

    public String toString() {
        return "LexerToken:" + name + "(" + nr + ") Value=\"" + value
            + "\" Pos=" + posStart + "-" + posEnd;
    }

    /**
     * Associate a ast node with this lexer token
     * 
     * @param astNode
     *            The ast node
     */
    public void setASTNode(ASTNode astNode) {
        this.astNode = astNode;
    }

    /**
     * Get the associated ast node
     * 
     * @return The ast ndoe
     */
    public ASTNode getASTNode() {
        return astNode;
    }
}
