/**
 * Project: fme
 */

package fme.components.editor;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

import fme.components.abstraction.GUIComponent;
import fme.components.console.Console;
import fme.components.editor.dialogs.SearchDialog;
import fme.config.CM;
import fme.wsl.ast.AST;

public class EditorGUI extends GUIComponent {

	public static final int MARK_AST_SUBTREE = 1;

	private int oldST, oldEN;

	/**
	 * Defined for serialization
	 */
	private static final long serialVersionUID = 4962241615006772424L;

	private StyledDocument document;

	private JTextPane editorPane;

	private JScrollPane scrollPane;

	private EditorListener listener;

	private boolean documentModified;

	/**
	 * The status bar
	 */
	private JLabel statusBar;

	/**
	 * (non-Javadoc)
	 * 
	 * @see fme.components.abstraction.GUIComponent#initComponent()
	 */
	protected boolean initComponent() {

		listener = new EditorListener(this);
		setLayout(new BorderLayout());

		getInternalFrame().setJMenuBar(new EditorMenuBar(listener));

		editorPane = new JTextPane();
		editorPane.setFont(Font.decode((String) CM
				.getAsString("gui.Editor.Font")));
		editorPane.setBackground((Color) CM.get("gui.Editor.BackgroundColor"));
		editorPane.setForeground((Color) CM.get("gui.Editor.FontColor"));
		editorPane.setCaretColor((Color) CM.get("gui.Editor.CaretColor"));
		editorPane.addCaretListener(listener);
		editorPane.addKeyListener(listener);
		document = (StyledDocument) editorPane.getDocument();
		document.addDocumentListener(listener);
		scrollPane = new JScrollPane(editorPane);
		add(scrollPane);
		oldST = 0;
		oldEN = 0;

		statusBar = new JLabel(" ");
		add(statusBar, BorderLayout.SOUTH);

		return true;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see fme.components.abstraction.GUIComponent#getName()
	 */
	public String getName() {
		return "WSL Editor";
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see fme.components.abstraction.GUIComponent#update()
	 */
	public void update() {

		if (AST.getFile() == null)
			return;
		editorPane.setText(AST.getAST().prettyPrint(-1).toString());
		document = (StyledDocument) editorPane.getDocument();
		document.addDocumentListener(listener);
		document.addUndoableEditListener(listener);

		AST.identifyTokens(document);
		documentModified = false;
		getInternalFrame().setTitle(getName());
		revalidate();
		enableBlockColoring();
	}

	/**
	 * Causes the editor to display a file with errors
	 */
	public void displayFileWithError() {

		try {
			editorPane.setPage(Console.getInstance().getWorkingFile().toURI()
					.toURL());
			document = (StyledDocument) editorPane.getDocument();
			document.addDocumentListener(listener);
			document.addUndoableEditListener(listener);
			getInternalFrame().setTitle(getName());
		} catch (IOException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "File to display caused an IOException.");
		}
		revalidate();
	}

	/**
	 * Returns the actual loaded document
	 * 
	 * @return The actual document
	 */
	public StyledDocument getDocument() {
		return document;
	}

	public void removeMarks() {
		// Delete the old marking
		SimpleAttributeSet s = new SimpleAttributeSet();
		StyleConstants.setBackground(s, Color.WHITE);
		((DefaultStyledDocument) editorPane.getDocument())
				.setCharacterAttributes(0, document.getLength(), s, true);
		oldST = 0;
		oldEN = 0;
	}

	/**
	 * Mark some text with a specific background color
	 * 
	 * @param start
	 *            The start char position
	 * @param end
	 *            The end char position
	 * @param color
	 *            The color to use
	 */
	public void markText(int start, int end, Color color) {
		markText(start, end, color, true);
	}

	/**
	 * Mark some text with a specific background color
	 * 
	 * @param start
	 *            The start char position
	 * @param end
	 *            The end char position
	 * @param color
	 *            The color to use
	 */
	public void markText(int start, int end, Color color, boolean deleteOld) {

		// Delete the old marking
		SimpleAttributeSet s = new SimpleAttributeSet();

		if (deleteOld) {
			try {
				StyleConstants.setBackground(s, Color.WHITE);
				((DefaultStyledDocument) editorPane.getDocument())
						.setCharacterAttributes(oldST, oldEN + 1 - oldST, s,
								true);
			} catch (Exception e) {
			}
		}

		oldST = start;
		oldEN = end;

		StyleConstants.setBackground(s, color);
		((DefaultStyledDocument) editorPane.getDocument())
				.setCharacterAttributes(start, end + 1 - start, s, true);
	}

	/**
	 * Mark some text
	 * 
	 * @param start
	 *            The start char position
	 * @param end
	 *            The end char position
	 */
	public void markText(int start, int end) {
		editorPane.select(start, end);
	}

	/**
	 * Get the text
	 * 
	 * @return The text of the editor
	 */
	public String getText() {
		try {
			return document.getText(0, document.getLength());
		} catch (BadLocationException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Can't get the text from the document.");
			return "";
		}
	}

	public void updateCaret() {
		editorPane.setCaretPosition(oldST);
	}

	public void updateStatusBar() {
		statusBar.setText(" Line="
				+ (editorPane.getDocument().getDefaultRootElement()
						.getElementIndex(editorPane.getCaretPosition()) + 1)
				+ "    Caret Position=" + editorPane.getCaretPosition());
	}

	public int getDot() {
		return editorPane.getCaret().getDot();
	}

	public void setDot(int d) {
		editorPane.getCaret().setDot(d);
	}

	/**
	 * Shows the specified position in the text
	 * 
	 * @param i
	 *            The position to show
	 */
	public void showPosition(int i) {
		editorPane.setCaretPosition(i);
	}

	/**
	 * Performs a search in the text
	 */
	public void search() {
		SearchDialog.showDialog(listener);
	}

	/**
	 * Cut the actual selection into the clipboard
	 */
	public void cut() {
		editorPane.cut();
	}

	/**
	 * Copy the actual selection into the clipboard
	 */
	public void copy() {
		editorPane.copy();
	}

	/**
	 * Paste the actual selection into the clipboard
	 */
	public void paste() {
		editorPane.paste();
	}

	/**
	 * Return the selected text
	 */
	public String getSelection() {
		return editorPane.getSelectedText();
	}

	/**
	 * Indicates if the document has been modified
	 * 
	 * @return True if the document has been modified / False if not
	 */
	public boolean isDocumentModified() {
		return documentModified;
	}

	/**
	 * Set the document to be modified
	 * 
	 * @param documentModified
	 *            Toggle the modified bit
	 */
	public void setDocumentModified(boolean documentModified) {
		this.documentModified = documentModified;
	}

	public void disableBlockColoring() {
		listener.disableBlockColoring();
	}

	public void enableBlockColoring() {
		listener.enableBlockColoring();
	}

	/**
	 * Disable Component
	 */
	public void disable() {
		listener.disable();
	}

	/**
	 * Enable Component
	 */
	public void enable() {
		listener.enable();
	}

	/**
	 * Set the font
	 * 
	 * @param font
	 *            The font as a String
	 */
	public void setFont(String font) {
		editorPane.setFont(Font.decode(font));
	}
}
