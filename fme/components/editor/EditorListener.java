/**
 * Project: fme
 */

package fme.components.editor;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;

import com.eaio.stringsearch.BoyerMooreHorspoolRaita;
import com.eaio.stringsearch.ShiftOrClasses;
import com.eaio.stringsearch.StringSearch;

import fme.components.ComponentRegistry;
import fme.components.UndoManager;
import fme.components.console.Console;
import fme.components.editor.dialogs.SearchDialog;
import fme.components.tree.TreeGUI;
import fme.config.CM;
import fme.wsl.ast.ASTNode;
import fme.wsl.lexer.LexerToken;
import fme.wsl.lexer.WSLLexer;

public class EditorListener implements CaretListener, DocumentListener,
		KeyListener, ActionListener, UndoableEditListener {

	private EditorGUI editorGUI;

	private boolean noUpdate = false;

	private boolean blockColoring = true;

	private int oldPos;

	private WaitClock clock;

	/**
	 * The Constructor
	 * 
	 * @param editorGUI
	 *            The editor GUI which uses this listener
	 */
	public EditorListener(EditorGUI editorGUI) {
		this.editorGUI = editorGUI;
		oldPos = -1;
		clock = new WaitClock(this);
		new Thread(clock).start();
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Action Event:" + e.getActionCommand());

		// Search String was entered
		if (e.getSource() instanceof JTextField) {
			if (SearchDialog.getDialog().getJCheckBox().isSelected())
				searchRegularExpression(SearchDialog.getDialog()
						.getJTextField().getText(), false);
			else
				searchKeyWord(SearchDialog.getDialog().getJTextField()
						.getText(), false);
		}
		// Find Button
		else if (e.getActionCommand().equals("Find")) {
			if (SearchDialog.getDialog().getJCheckBox().isSelected())
				searchRegularExpression(SearchDialog.getDialog()
						.getJTextField().getText(), false);
			else
				searchKeyWord(SearchDialog.getDialog().getJTextField()
						.getText(), false);
		}
		// Replace Button
		else if (e.getActionCommand().equals("Replace")) {
			if (SearchDialog.getDialog().getJCheckBox().isSelected())
				searchRegularExpression(SearchDialog.getDialog()
						.getJTextField().getText(), true);
			else
				searchKeyWord(SearchDialog.getDialog().getJTextField()
						.getText(), true);
		}
		// Cancel Button
		else if (e.getActionCommand().equals("Cancel")) {
			SearchDialog.getDialog().setVisible(false);
		}
		// Help Button
		else if (e.getActionCommand().equals("Help")) {
			JOptionPane
					.showMessageDialog(
							editorGUI,
							"Enter a keyword/regular expression and optional a replacement. Tick the box regular expression\n"
									+ "to enable regular expressions.\n\n"
									+ "Possible expressions are:\n"
									+ "         x : A character from the Alphabet\n"
									+ "         ? : A \"don't care\" symbol which matches all symbols\n"
									+ "         [ .. ] : A class of characters where ranges (a-z, 0-9) are allowed (e.g. [a-z])\n"
									+ "         ^ : The negation of a character or class of characters (e.g. ^a, ^[abc], ^[c-h])\n",
							"Help", JOptionPane.PLAIN_MESSAGE);
		}
		// Disable the coloring
		else if (e.getActionCommand().equals("Menu:BlockColoring")) {
			if (blockColoring) {
				editorGUI.markText(0, 0, Color.WHITE);
				blockColoring = false;
			} else {
				if (editorGUI.isDocumentModified()) {
					disableBlockColoring();
					JOptionPane
							.showMessageDialog(
									editorGUI,
									"Document has been modified.\nPlease save the changes to enable the coloring.",
									"Message", JOptionPane.ERROR_MESSAGE);
				} else if (Console.getInstance().isDocumentErroneous()) {
					JOptionPane
							.showMessageDialog(
									editorGUI,
									"Document contains syntax errors!\nColoring is not available.",
									"Message", JOptionPane.ERROR_MESSAGE);
				} else {
					editorGUI.removeMarks();
					blockColoring = true;
				}
			}
		}
		// Find something
		else if (e.getActionCommand().equals("Menu:FindReplace"))
			editorGUI.search();
		// Cut,Copy and Paste
		else if (e.getActionCommand().equals("Menu:Cut"))
			editorGUI.cut();
		else if (e.getActionCommand().equals("Menu:Copy"))
			editorGUI.copy();
		else if (e.getActionCommand().equals("Menu:Paste"))
			editorGUI.paste();
		else if (e.getActionCommand().equals("Menu:NormalFont")) {
			editorGUI.setFont((String) CM.getAsString("gui.Editor.Font"));
		} else if (e.getActionCommand().equals("Menu:LargeFont")) {
			editorGUI.setFont((String) CM.getAsString("gui.Editor.FontLarge"));
		}
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.CaretListener#caretUpdate(javax.swing.event.CaretEvent)
	 */
	public void caretUpdate(CaretEvent e) {
		int j, st, en, row, pos;
		LexerToken token = null;
		ASTNode astNode = null;
		Vector<LexerToken> tokenList = WSLLexer.getTokens();
		Color col;

		editorGUI.updateStatusBar();

		if (noUpdate || !blockColoring
				|| Console.getInstance().isDocumentErroneous())
			return;

		// If the user clicks on the same location again nothing happens
		if (e.getDot() == oldPos)
			return;
		else
			oldPos = e.getDot();

		pos = e.getDot();

		// Search for the token begin
		for (j = pos; j >= 0; j--) {
			if ((token = WSLLexer.getToken(j)) != null) {
				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.INFO, "Click on Token:" + token.getName());
				break;
			}
		}

		// If a token has been identified mark it and its corresponding tree
		// node
		if (token != null) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.INFO, "Click on Tree Node:" + token.getASTNode());

			if (token.getASTNode() != null) {
				astNode = token.getASTNode();

				// Mark the AST Node in the tree
				// This will also throw the events to the ComponentRegistry
				noUpdate = true;
				TreeGUI tree = (TreeGUI) ComponentRegistry
						.getComponent("TreeGUI");
				row = astNode.getRow();
				tree.expandTreeToRow(row);
				tree.getTree().setSelectionPath(
						tree.getTree().getPathForRow(row));
				tree.getTree().scrollRowToVisible(row);
				noUpdate = false;

				// Mark a block
				if (astNode.getLexerToken().size() == 2) {
					st = tokenList.get(
							astNode.getLexerToken().get(0).intValue())
							.getPosStart();

					en = astNode.getLexerToken().get(1).intValue();

					// Detect if the last token consists of more than one token
					if (!tokenList.get(en).getASTNode().equals(astNode)) {
						en = tokenList.get(en).getASTNode().getLexerToken()
								.lastElement().intValue();
					}

					en = tokenList.get(en).getPosEnd();
					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.INFO,
							"  Marking block from:" + st + " to " + en);
				}
				// Mark a single statement
				else {
					st = tokenList.get(
							astNode.getLexerToken().get(0).intValue())
							.getPosStart();

					en = tokenList.get(
							astNode.getLexerToken().get(0).intValue())
							.getPosEnd();

					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.INFO,
							"  Marking single token from:" + st + " to " + en);
				}
				col = CM.getAsColor("gui.Editor.BlockColor");
			}
			// Mark just the token which is not part of the AST
			else {
				st = token.getPosStart();
				en = token.getPosEnd();

				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.INFO,
						"  Marking not identified token from:" + st + " to "
								+ en);

				col = CM.getAsColor("gui.Editor.BlockColorUnidentified");
			}
			noUpdate = true;
			editorGUI.markText(st, en, col);
			editorGUI.setDot(pos);
			noUpdate = false;
		}
		ComponentRegistry.updateGUI(ComponentRegistry.EDITOR_MARKING);
	}

	public void changedUpdate(DocumentEvent e) {
		editorGUI.updateStatusBar();
	}

	public void insertUpdate(DocumentEvent e) {
		if (!editorGUI.isDocumentModified()) {
			disableBlockColoring();
			editorGUI.setDocumentModified(true);
			editorGUI.getInternalFrame().setTitle(
					editorGUI.getInternalFrame().getTitle() + " - Modified");
		}
	}

	public void removeUpdate(DocumentEvent e) {
		if (!editorGUI.isDocumentModified()) {
			disableBlockColoring();
			editorGUI.setDocumentModified(true);
			editorGUI.getInternalFrame().setTitle(
					editorGUI.getInternalFrame().getTitle() + " - Modified");
		}
	}

	public void undoableEditHappened(UndoableEditEvent e) {
		if (!e.getEdit().toString().contains("AttributeUndoableEdit")
				&& e.getEdit().isSignificant()) {
			UndoManager.getInstance().storeEditorEvent(e.getEdit());
		}
	}

	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() != KeyEvent.VK_UP
				&& e.getKeyCode() != KeyEvent.VK_DOWN
				&& e.getKeyCode() != KeyEvent.VK_LEFT
				&& e.getKeyCode() != KeyEvent.VK_RIGHT) {
			noUpdate = true;
			editorGUI.removeMarks();
		} else if (e.getModifiers() != 0) {
			noUpdate = true;
			editorGUI.removeMarks();
		}

		clock.startClock(new ClockCommand() {
			public void execute() {
				editorGUI.removeMarks();
			}
		});
	}

	public void keyReleased(KeyEvent e) {
	}

	public void keyTyped(KeyEvent e) {
	}

	public void disableBlockColoring() {
		((EditorMenuBar) editorGUI.getInternalFrame().getJMenuBar())
				.getCheckBox().setSelected(false);
		blockColoring = false;
	}

	public void enableBlockColoring() {
		((EditorMenuBar) editorGUI.getInternalFrame().getJMenuBar())
				.getCheckBox().setSelected(true);
		blockColoring = true;
	}

	/**
	 * Disable Component
	 */
	public void disable() {
		noUpdate = true;
	}

	/**
	 * Enable Component
	 */
	public void enable() {
		noUpdate = false;
	}

	// Internal Methods
	// ================

	/**
	 * Performs a search for a keyword
	 */
	private void searchKeyWord(String word, boolean replace) {
		StringSearch so = new BoyerMooreHorspoolRaita();
		performSearch(word, so, word.length(), replace);
	}

	/**
	 * Perform a search with regular expressions
	 */
	private void searchRegularExpression(String regEx, boolean replace) {
		char a[];
		int wl = regEx.length();
		ShiftOrClasses so = new ShiftOrClasses();

		a = regEx.toCharArray();
		for (int i = 0; i < a.length; i++) {
			if (a[i] == '[')
				wl = wl - 4;
			if (a[i] == '^')
				wl = wl - 1;
		}
		if (wl < 0)
			return;

		performSearch(regEx, so, wl, replace);
	}

	/**
	 * Performs a search
	 */
	private void performSearch(String word, StringSearch so, int wl,
			boolean replace) {
		boolean bc;

		int pos, co;
		String text = editorGUI.getText(), replaceText = SearchDialog
				.getDialog().getJTextField1().getText();

		if (!SearchDialog.getDialog().getJCheckBox1().isSelected()) {
			word = word.toLowerCase();
			text = text.toLowerCase();
		}

		editorGUI.removeMarks();

		// All ocurencies should be marked
		if (SearchDialog.getDialog().getJCheckBox11().isSelected()) {

			disableBlockColoring();

			pos = 0;
			co = 0;

			while (pos != -1 && pos + wl < text.length()) {
				pos = so.searchString(text, pos, word);
				if (pos != -1) {
					if (!replace)
						editorGUI.markText(pos, pos + wl - 1, CM
								.getAsColor("gui.Editor.SearchColor"), false);
					else {
						try {
							editorGUI.getDocument().remove(pos - co, wl);
							editorGUI.getDocument().insertString(pos - co,
									replaceText, null);
							editorGUI.markText(pos - co, pos - co
									+ replaceText.length() - 1, CM
									.getAsColor("gui.Editor.SearchColor"),
									false);
							co += wl - replaceText.length();
						} catch (BadLocationException e) {
							Logger
									.getLogger(
											this.getClass().getCanonicalName())
									.log(
											Level.INFO,
											"Try to insert / remove "
													+ replaceText
													+ " on a bad location:"
													+ (pos - co));
						}
					}
				}
				if (pos != -1)
					pos += wl;
			}
		} else {
			if (replace && editorGUI.getSelection() != null) {
				pos = editorGUI.getDot();
				try {
					bc = blockColoring;
					blockColoring = false;
					editorGUI.getDocument().remove(pos - wl, wl);
					editorGUI.getDocument().insertString(pos - wl, replaceText,
							null);
					blockColoring = bc;
					if (!SearchDialog.getDialog().getJCheckBox1().isSelected())
						text = editorGUI.getText().toLowerCase();
					else
						text = editorGUI.getText();
				} catch (BadLocationException e) {
					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.INFO,
							"Try to insert / remove " + replaceText
									+ " on a bad location:" + (pos - wl));
				}
			}

			pos = so.searchString(text, editorGUI.getDot(), word);
			if (pos == -1) {
				JOptionPane.showMessageDialog(editorGUI,
						"String / Pattern not found!", "Message",
						JOptionPane.INFORMATION_MESSAGE);
			} else {
				bc = blockColoring;
				blockColoring = false;
				editorGUI.markText(pos, pos + wl);
				blockColoring = bc;
			}
		}
	}

	// Internal Classes
	// ================

	private class WaitClock implements Runnable {

		private boolean finished = true;

		private int timer = 0;

		private EditorListener listener;

		private ClockCommand cc;

		public WaitClock(EditorListener listener) {
			this.listener = listener;
		}

		public void run() {

			while (true) {
				if (!finished) {
					while (timer != 0) {
						try {
							Thread.sleep(100);
						} catch (InterruptedException e) {
						}
						timer--;
					}
					listener.noUpdate = false;
					finished = true;
					cc.execute();
				}
				try {
					Thread.sleep(100);
				} catch (InterruptedException e) {
				}
			}
		}

		public void startClock() {
			timer = 10;
			finished = false;
		}

		public void startClock(ClockCommand cc) {
			timer = 3;
			finished = false;
			this.cc = cc;
		}

		public boolean isFinished() {
			return finished;
		}
	}

	private class ClockCommand {
		public void execute() {
		}
	}
}
