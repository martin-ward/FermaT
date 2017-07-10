/**
 * Project: fme
 */

package fme.wsl.ast;

import java.io.File;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.text.Document;
import javax.swing.text.PlainDocument;

import fme.wsl.lexer.LexerToken;
import fme.wsl.lexer.WSLLexer;
import fme.wsl.tables.WSLTreeGrammarTable;

/**
 * This class holds the AST of the current WSL program
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class AST {

	/**
	 * The AST of a WSL program
	 */
	private static ASTNode ast;

	/**
	 * The file of the WSL program
	 */
	private static File file;

	/**
	 * The tree as a list of nodes
	 */
	private static Vector<ASTNode> nodes;

	/**
	 * Parse a tree from the FermaT engine
	 * 
	 * @param buffer
	 *            The text representation of the engine's tree
	 */
	public static boolean parseTree(String buffer) {

		int level, i;
		Vector<TreeNode> tree = new Vector<TreeNode>();
		String line, name;
		StringTokenizer st = new StringTokenizer(buffer, "\n");

		while (st.hasMoreTokens()) {
			line = st.nextToken();
			if (line.contains("\r"))
				line = line.substring(0, line.length() - 1);
			for (i = 0; i < line.length(); i++) {
				if (line.charAt(i) != ' ' && line.charAt(i) != ':')
					break;
			}
			name = line.substring(i, line.length());
			if (name.equals("#t"))
				break;
			level = (line.length() - name.length()) / 3;

			if (WSLTreeGrammarTable.getGrammarOfNode(name.split(" ")[0]) == null) {
				tree.lastElement().append("\n" + name);
			} else {
				tree.add(new TreeNode(level, name));
			}
		}

		TreeNode tn = tree.firstElement();
		tree.remove(0);

		// File contains no element
		try {
			if (tree.firstElement().name.equals(""))
				return false;
		} catch (NoSuchElementException e) {
			return false;
		}

		ASTNode node = new ASTNode(tn.name, null);
		addNode(tree, node);

		ast = node;

		// Create the list of Nodes
		nodes = new Vector<ASTNode>();
		nodes.add(ast);
		fillNodes(ast);

		Logger.getLogger(AST.class.getCanonicalName()).log(Level.INFO,
				"New tree successfully parsed.");
		return true;
	}

	/**
	 * Fill the list of nodes
	 * 
	 * @param n
	 *            The start node
	 */
	private static void fillNodes(ASTNode n) {
		for (int i = 0; i < n.getChildren().size(); i++) {
			nodes.add(n.getChildren().get(i));
			fillNodes(n.getChildren().get(i));
		}
	}

	/**
	 * Links the tree nodes to the lexer tokens
	 * 
	 * @param tokens
	 *            The lexer tokens
	 */
	public static void identifyTokens(Document document) {
		Logger.getLogger(AST.class.getCanonicalName()).log(Level.INFO,
				"Trying to link the tree nodes with the WSL source code.");

		Vector<LexerToken> ltl;
		Iterator<LexerToken> i1, i2;

		// Generate the source code from the tree with linkage information
		PlainDocument pdoc = new PlainDocument();
		try {
			pdoc.insertString(0, ast.prettyPrint(0, true).toString(), null);
			//System.out.println(ast.prettyPrint(0, false).toString());
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Generate the linkage between tree and source code
		WSLLexer.parseWSL(pdoc);

		ltl = new Vector<LexerToken>(WSLLexer.getTokens());

		i1 = ltl.iterator();

		// Now generate the lexer token list with the correct position
		// information
		WSLLexer.parseWSL(document);

		i2 = WSLLexer.getTokens().iterator();

		// Set the AST Nodes in the correct LexerToken list
		while (i1.hasNext() && i2.hasNext()) {
			i2.next().setASTNode(i1.next().getASTNode());
		}
	}

	/**
	 * Get the AST of the current program
	 * 
	 * @return The top level node of the AST
	 */
	public static ASTNode getAST() {
		return ast;
	}

	/**
	 * Get a node from a specific row
	 * 
	 * @param row
	 *            The row
	 * @return The ASTNode from the specified row
	 */
	public static ASTNode getNodeFromRow(int row) {
		return nodes.get(row);
	}

	/**
	 * Get the current source file
	 * 
	 * @return The current source file
	 */
	public static File getFile() {
		return file;
	}

	/**
	 * Set the current source File
	 * 
	 * @param file
	 *            The new source file
	 */
	public static void setFile(File file) {
		AST.file = file;
	}

	// Internal Methods
	// ================

	private static void addNode(Vector<TreeNode> tree, ASTNode node) {

		if (tree.size() == 0)
			return;

		TreeNode tn = tree.firstElement(), tnNew;
		tree.remove(0);
		ASTNode childnode = new ASTNode(tn.name, node);
		node.addChild(childnode);

		while (tree.size() > 0) {
			tnNew = tree.firstElement();
			if (tnNew.level == tn.level) {
				tree.remove(0);
				tn = tnNew;
				childnode = new ASTNode(tn.name, node);
				node.addChild(childnode);
			} else if (tnNew.level > tn.level)
				addNode(tree, childnode);
			else if (tnNew.level < tn.level)
				return;
		}
	}

	private static class TreeNode {

		public int level;

		public String name;

		public TreeNode(int level, String name) {
			this.level = level;
			this.name = name;
		}

		public void append(String toAdd) {
			name = name + toAdd;
		}

		public String toString() {
			return "TreeNode:" + name + " (level=" + level + ")";
		}
	}
}
