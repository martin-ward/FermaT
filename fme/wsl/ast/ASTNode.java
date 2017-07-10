/**
 * Project: fme
 */

package fme.wsl.ast;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.tree.TreeNode;

import fme.components.console.Console;
import fme.gui.MainFrame;
import fme.wsl.tables.LexerTokenTable;
import fme.wsl.tables.WSLTreeGrammarTable;

/**
 * This class represents a node in the AST of a WSL program
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class ASTNode implements TreeNode {

	/**
	 * Indicates if the tree should be valid at any time
	 */
	private static boolean treeValidationChecking = true;

	/**
	 * The id of the node
	 */
	private int id;

	/**
	 * The specific type of the node
	 */
	private String specificType;

	/**
	 * The syntax name (These names are printed when using the
	 * 
	 * @print_wsl routine)
	 */
	private String syntaxName;

	/**
	 * The general type of a node
	 */
	private int generalType;

	/**
	 * The parent of a node (null if the node is the top level node)
	 */
	private ASTNode parent;

	/**
	 * The children of a node
	 */
	private Vector<ASTNode> children;

	/**
	 * The value of a node
	 */
	private String value;

	/**
	 * True when this node is a grouping node
	 */
	private boolean isGroupNode = false;

	/**
	 * True if this node is a general node
	 */
	private boolean isGeneralNode = false;

	/**
	 * Corresponding Lexer tokens
	 */
	private Vector<Integer> lexerTokens;

	/**
	 * The template for PrettyPrint
	 */
	private String ppTemplate;

	/**
	 * The Constructor for clone
	 */
	private ASTNode() {
	}

	/**
	 * The Constructor
	 * 
	 * @param name
	 *            The syntax name of the node
	 * @param parent
	 *            The parent of the node (null if the node is the top level
	 *            node)
	 */
	public ASTNode(String name, ASTNode parent) {
		String nodeName, nodeValue = null;
		if (name.contains(" ")) {
			nodeName = name.substring(0, name.indexOf(" "));
			nodeValue = name.substring(name.indexOf(" ") + 1);
		} else
			nodeName = name;

		this.parent = parent;
		syntaxName = nodeName;

		try {
			id = Integer.parseInt(WSLTreeGrammarTable.getGrammarOfNode(
					syntaxName).get(0));
		} catch (NullPointerException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Node " + name + " couldn't be constructed");

			if (!Console.ignoreErrors)
				JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
						"A child node of " + parent.getSpecificType()
								+ " (Row:" + parent.getRow()
								+ ") couldn't be constructed", "Error",
						JOptionPane.ERROR_MESSAGE);

			return;
		}

		if (id < 10)
			isGeneralNode = true;
		else if (id < 100 && id > 9)
			isGroupNode = true;
		specificType = WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(1);
		if (!isGeneralNode && !isGroupNode)
			generalType = Integer.parseInt(WSLTreeGrammarTable
					.getGrammarOfNode(syntaxName).get(3));
		else
			generalType = id;
		children = new Vector<ASTNode>();
		if (nodeValue != null) {
		    value = nodeValue.replace("?", "?");
			if (WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(5).equals(
					"0")) {
				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.WARNING,
						"ASTNode " + specificType + " (" + getRow()
								+ ") shouldn't have a value \"" + value + "\"");
			}
		}
		lexerTokens = new Vector<Integer>();

		ppTemplate = WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(6);
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		if (value == null)
			return syntaxName;
		else
			return syntaxName + " (" + value + ")";
	}

	/**
	 * Prints a XML representation of the node
	 * 
	 * @return The XML representation of this node
	 */
	public String toStringXML() {
		String ret;

		if (isGroupNode)
			ret = "<ASTGroupNode ID=\"" + id + "\" Name=\"" + specificType
					+ "\"";
		else if (isGeneralNode)
			ret = "<ASTGeneralNode ID=\"" + id + "\" Name=\"" + specificType
					+ "\"";
		else
			ret = "<ASTNode ID=\"" + id + "\" Name=\"" + specificType
					+ "\" General_Type=\""
					+ WSLTreeGrammarTable.getGrammarOfNode(generalType).get(1)
					+ "\"";

		if (value == null
				&& !WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(4)
						.equals("")) {
			String[] children = WSLTreeGrammarTable
					.getGrammarOfNode(syntaxName).get(4).split(";");
			ret += " AllowedChildren=\"";
			for (int i = 0; i < children.length; i++) {
				ret += WSLTreeGrammarTable.getGrammarOfNode(
						Integer.parseInt(children[i])).get(1);
				if (i != children.length - 1)
					ret += ";";
			}
		}
		if (value != null) {
			ret += " Value=\"" + value + "\">";
		} else {
			ret += ">";
		}

		return ret;
	}

	/**
	 * Prints the node and all it's children
	 * 
	 * @return XML representation of the node and it's children
	 */
	public String toStringSubTree() {
		StringBuffer buf = new StringBuffer();
		ASTNode n;
		Iterator<ASTNode> i;

		buf.append(toStringXML() + "\n");

		i = children.iterator();
		while (i.hasNext()) {
			n = i.next();
			toStringSubTree("  ", n, buf);
		}

		return buf.toString();
	}

	// Getter / Setter Methods
	// =======================

	/**
	 * Get the id of the node
	 * 
	 * @return The id of the node
	 */
	public int getId() {
		return id;
	}

	/**
	 * Get the children of the node
	 * 
	 * @return The children of the node
	 */
	public Vector<ASTNode> getChildren() {
		return children;
	}

	/**
	 * Get the rightmost child node
	 * 
	 * @return The rightmost child node of this node
	 */
	public ASTNode getRightmostChild() {
		ASTNode child = this;
		while (child.getChildCount() != 0) {
			child = child.getChildren().lastElement();
		}
		return child;
	}

	/**
	 * Get the first lexer token with a value
	 * 
	 * @return The first lexer token with a value
	 */
	public int getLeftmostLexerTokenPos() {
		ASTNode child = this;
		child = this;
		while (child.getChildCount() != 0 && child.getLexerToken().size() == 0)
			child = child.getChildren().firstElement();
		if (child.getLexerToken().size() != 0)
			return child.getLexerToken().get(0).intValue();
		else {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.WARNING,
					"Node:" + specificType
							+ " can't find first lexer token with a value");
			return -1;
		}
	}

	/**
	 * Add a child to the node
	 * 
	 * @param child
	 *            The child to add
	 */
	public void addChild(ASTNode child) {
		if (isChildValid(child))
			children.add(child);
	}

	/**
	 * Get the general type id of the node
	 * 
	 * @return The general type id of the node
	 */
	public int getGeneralType() {
		return generalType;
	}

	/**
	 * Get the specific type id of the node
	 * 
	 * @return The specific type id of the node
	 */
	public String getSpecificType() {
		return specificType;
	}

	/**
	 * Get the value of the node
	 * 
	 * @return The value of the node
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Set the value of the node
	 * 
	 * @param value
	 */
	public void setValue(String value) {
	        	this.value = value.replace("?", "?");
	}

	/**
	 * Return true if the validation checking is enables (this guarantees that
	 * the tree is always valid)
	 * 
	 * @return True if the validation checking is enabled
	 */
	public static boolean isTreeValidationChecking() {
		return treeValidationChecking;
	}

	/**
	 * Set the validation checking
	 * 
	 * @param treeValidationCheck
	 *            True to enable / False to disable the checking
	 */
	public static void setTreeValidationChecking(boolean treeValidationCheck) {
		ASTNode.treeValidationChecking = treeValidationCheck;
	}

	public ASTNodePreOrderTreeIterator getTreeIterator() {
		return new ASTNodePreOrderTreeIterator(this);
	}

	/**
	 * Associate a lexer token (position in the lexer list) with this tree node
	 * 
	 * @param lexerToken
	 *            The lexerToken to associate
	 */
	public void addLexerToken(Integer lexerToken) {
		lexerTokens.add(lexerToken);
	}

	/**
	 * Get all associated lexer tokens
	 * 
	 * @param lexerToken
	 *            The lexerToken to associate
	 */
	public Vector<Integer> getLexerToken() {
		return lexerTokens;
	}

	/**
	 * Pretty Print this node into source code
	 * 
	 * @return The source code this node represents
	 */
	public StringBuffer prettyPrint(int in) {
		return prettyPrint(in, false);
	}

	/**
	 * Pretty Print this node into source code
	 * 
	 * @return The source code this node represents
	 */
	public StringBuffer prettyPrint(int in, boolean markingTags) {
		boolean indent = false;
		int skip;
		String c[], t1, t[], t2;
		StringBuffer code = new StringBuffer();
		ASTNode child;
		Iterator<ASTNode> it;

		if (ppTemplate == null || ppTemplate == "") {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.WARNING,
					"Node:" + specificType + " has no PrettyPrint template");
			return code;
		}

		if (markingTags)
			code.append("<#" + getRow() + "|>");

		// Chop up the Template
		c = ppTemplate.split(";");
		for (int i = 0; i < c.length; i++) {

			// Indention
			if (c[i].equals("I")) {
				in += 1;
				indent = true;
			}
			// If all children should be iterated with a separating token
			else if (c[i].startsWith("(A")) {
				skip = 0;
				if (c[i].startsWith("(A[")) {
					skip = Integer.parseInt(c[i]
							.substring(3, c[i].indexOf("]")));
					c[i] = c[i].substring(0, 2)
							+ c[i].substring(c[i].indexOf("]") + 1);
				}
				t = c[i].substring(3, c[i].length() - 1).split("#");
				// Get the seperating string
				t1 = "";
				for (int j = 0; j < t.length; j++) {
					if (t[j].equals("\\n"))
						t1 += "\n";
					else if (!t[j].equals("") && !t[j].equals(" "))
						t1 += LexerTokenTable.getLexerTokenRepresentation(t[j]);
					else
						t1 += t[j];
				}
				// Iterate through all children
				it = children.iterator();
				for (int j = 0; j < skip; j++) {
					it.next();
				}
				while (it.hasNext()) {
					child = it.next();
					t2 = child.prettyPrint(in, markingTags).toString();
					pp_append(t2, code, markingTags);
					if (it.hasNext()) {
						// Print seperating string

						// *** FermaT ELSE code ***
						if (specificType.equals("T_Cond")
								&& child.equals(children
										.get(children.size() - 2))) {

							// Check if ELSE needs to be printed at all or
							// whether it can be omitted.
							if (children.lastElement().children.size() == 2
									&& children.lastElement().children.get(1).children
											.get(0).getSpecificType().equals(
													"T_Skip")) {
								// Special code to write ELSE (within the last
								// guard node) SKIPs the next space and THEN
								// statement
								it.next();
							} else {
								// Special code to write ELSE (if there are more
								// than two guards)
								pp_append(t1.replace("ELSIF", "ELSE"), code,
										markingTags);
							}
						} else {
							pp_append(t1, code, markingTags);
						}
					}
				}
			}
			// Print a single token
			else if (c[i].startsWith("S_")) {
				t1 = LexerTokenTable.getLexerTokenRepresentation(c[i]);

				// *** FermaT ELSE code ***
				if (specificType.equals("T_Cond") && t1.equals("ELSIF")
						&& children.size() == 2) {
					// Check if ELSE needs to be printed at all or whether it
					// can be omitted.
					if (children.get(1).children.size() == 2
							&& children.get(1).children.get(1).children.get(0)
									.getSpecificType().equals("T_Skip")) {
						// Special code to write ELSE (within the last guard
						// node) SKIPs the next space and THEN statement
						i++;
						i++;
					} else {

						// Special code to write ELSE (if there are only 2
						// guards)
						pp_append("ELSE", code, markingTags);
					}
				} else {
					pp_append(t1, code, markingTags);
				}
			}
			// Print a space
			else if (c[i].startsWith(" ")) {
				pp_append(" ", code, markingTags);
			}
			// Print a single child
			else if (c[i].startsWith("(C")) {
				t1 = c[i].substring(2, c[i].length() - 1);

				// *** FermaT ELSE code ***
				if (t1.equals("0") && parent.specificType.equals("T_Cond")
						&& children.get(0).specificType.equals("T_True")
						&& parent.children.lastElement().equals(this)) {
					// Special code to write ELSE (within the last guard node)
					// SKIPs the next space and THEN statement
					i++;
					i++;
				} else {
					pp_append(children.get(Integer.parseInt(t1)).prettyPrint(
							in, markingTags).toString(), code, markingTags);
				}
			}
			// Print the value
			else if (c[i].equals("(V)")) {
				pp_append(value, code, markingTags);
			}
			// Print a new line
			else if (c[i].equals("\\n")) {
				pp_append("\n", code, markingTags);
			}
			// Print a character
			else if (c[i].startsWith("C:")) {
				pp_append((char) Integer.parseInt(c[i].substring(2)) + "",
						code, markingTags);
			}
			// Print token if there is a child sequence
			else if (c[i].startsWith("CS:")) {
				if (children.size() > 1) {
					t1 = LexerTokenTable.getLexerTokenRepresentation(c[i]
							.substring(3));
					pp_append(t1, code, markingTags);
				}
			}
			// Print token if parent is of a type
			else if (c[i].startsWith("PARENT(")) {
				if (children.size() > 1) {
					t = c[i].substring(7, c[i].length() - 1).split(":");
					if (parent.specificType.equals(t[0])) {
						t = t[1].split("#");
						for (int j = 0; j < t.length; j++) {
							if (t[j].equals("\\n"))
								t1 = "\n";
							else if (t[j].equals(" "))
								t1 = " ";
							else
								t1 = LexerTokenTable
										.getLexerTokenRepresentation(t[j]);
							pp_append(t1, code, markingTags);
						}
					}
				}
			}
		}

		if (indent) {
			t1 = indent(in) + code.toString();
			if (t1.endsWith("\n")) {
				t1 = t1.substring(0, t1.length() - 1);
				t1 = t1.replace("\n", "\n" + indent(in));
				t1 = t1 + "\n";
			} else {
				t1 = t1.replace("\n", "\n" + indent(in));
			}
			code = new StringBuffer(t1);
		}

		if (markingTags)
			code.append("<|" + getRow() + "#>");

		return code;
	}

	// Internal Methods
	// ================

	private void pp_append(String toAdd, StringBuffer code, boolean markingTags) {
		code.append(toAdd);
	}

	private void toStringSubTree(String indent, ASTNode n, StringBuffer buf) {
		ASTNode c;
		Iterator<ASTNode> i;

		buf.append(indent + n.toStringXML() + "\n");

		i = n.getChildren().iterator();
		while (i.hasNext()) {
			c = i.next();
			toStringSubTree(indent + "  ", c, buf);
		}
	}

	private boolean isChildValid(ASTNode child) {
		String[] sp = WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(4)
				.split(";");
		boolean childFound = false, posVerified = false;

		for (int i = 0; i < sp.length; i++) {
			if (!sp[i].equals("")
					&& sp[i].contains("" + child.getGeneralType())) {
				childFound = true;
				if (sp.length > 1 && i != children.size())
					continue;
				posVerified = true;
				break;
			}
		}

		if (!childFound) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.WARNING,
					"Node:" + specificType + " (" + getRow()
							+ ") shouldn't have a child of type:"
							+ child.getSpecificType() + ".");
			if (treeValidationChecking)
				return false;
		} else if (childFound && !posVerified) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.WARNING,
					"Node:" + specificType + " the child of type:"
							+ child.getSpecificType() + " (" + getRow()
							+ ") is at a wrong position.");
			if (treeValidationChecking)
				return false;
		}
		return true;
	}

	private String indent(int indent) {
		String ret = "";
		if (indent < 0)
			indent = 0;
		for (int i = 0; i < indent; i++) {
			ret += " ";
		}
		return ret;
	}

	// Interface TreeNode
	// ==================

	/**
	 * Get the parent of the node
	 * 
	 * @return The parent of the node
	 */
	public ASTNode getParent() {
		return parent;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.tree.TreeNode#children()
	 */
	public Enumeration<ASTNode> children() {
		return children.elements();
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.tree.TreeNode#getAllowsChildren()
	 */
	public boolean getAllowsChildren() {
		if (WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(4).equals(""))
			return false;
		else
			return true;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.tree.TreeNode#getChildAt(int)
	 */
	public ASTNode getChildAt(int childIndex) {
		return children.get(childIndex);
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.tree.TreeNode#getChildCount()
	 */
	public int getChildCount() {
		return children.size();
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.tree.TreeNode#getIndex(javax.swing.tree.TreeNode)
	 */
	public int getIndex(TreeNode node) {
		return children.indexOf(node);
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.tree.TreeNode#isLeaf()
	 */
	public boolean isLeaf() {
		if (WSLTreeGrammarTable.getGrammarOfNode(syntaxName).get(4).equals(""))
			return true;
		else
			return false;
	}

	// Own tree routines
	// =================

	public String getFermaTTreePath() {
		StringBuffer ret = new StringBuffer();
		int myIndex = 0;
		ASTNode par = parent, oldPar;

		if (parent != null)
			myIndex = parent.children.indexOf(this);

		while (par != null) {
			for (int i = 0; i < myIndex; i++) {
				ret.insert(0, "@RIGHT ");
			}
			ret.insert(0, "@DOWN ");
			oldPar = par;
			par = par.getParent();
			if (par != null) {
				myIndex = par.children.indexOf(oldPar);
			}
		}

		if (ret.length() == 0)
			return null;
		else
			return ret.toString().substring(0, ret.length() - 1);
	}

	public int getRow() {
		int myIndex = 0, row = 0;
		ASTNode par = parent, oldPar;

		if (parent != null)
			myIndex = parent.children.indexOf(this);

		while (par != null) {
			row++;
			for (int i = 0; i < myIndex; i++) {
				row += par.children.get(i).getAllChildrenCount() + 1;
			}
			oldPar = par;
			par = par.getParent();
			if (par != null)
				myIndex = par.children.indexOf(oldPar);
		}
		return row;
	}

	public int getAllChildrenCount() {
		int c = children.size();
		for (int i = 0; i < children.size(); i++) {
			c += children.get(i).getAllChildrenCount();
		}
		return c;
	}

	public ASTNode clone() {
		ASTNode dest = new ASTNode();

		cloneNode(this, dest);
		cloneChildren(this, dest);

		return dest;
	}

	// Internal Methods
	// ================

	private void cloneNode(ASTNode src, ASTNode dest) {
		dest.id = src.id;
		dest.specificType = src.specificType;
		dest.syntaxName = src.syntaxName;
		dest.generalType = src.generalType;
		dest.parent = null;
		dest.children = new Vector<ASTNode>();
		dest.value = src.value;
		dest.isGroupNode = src.isGroupNode;
		dest.isGeneralNode = src.isGeneralNode;

		// Copy all lexer tokens
		dest.lexerTokens = new Vector<Integer>();
		Iterator<Integer> i = src.lexerTokens.iterator();
		while (i.hasNext()) {
			Integer i1 = i.next(), i2;
			i2 = new Integer(i1.intValue());
			dest.lexerTokens.add(i2);
		}
	}

	private void cloneChildren(ASTNode src, ASTNode dest) {

		ASTNode child, childClone;

		// Clone children vector
		if (src.children != null && src.children.size() != 0) {
			Iterator<ASTNode> i = src.children.iterator();
			while (i.hasNext()) {
				child = i.next();
				childClone = new ASTNode();
				cloneNode(child, childClone);
				cloneChildren(child, childClone);
				childClone.parent = dest;
				dest.children.add(childClone);
			}
		}

	}
}
