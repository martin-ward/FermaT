/**
 * Project: fme
 */

package fme.components.catalogue;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.JOptionPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import fme.components.ComponentRegistry;
import fme.components.console.Console;
import fme.components.editor.EditorGUI;
import fme.components.tree.TreeGUI;
import fme.gui.MainFrame;

public class CatalogueListener implements ActionListener, ListSelectionListener {

	private CatalogueGUI parent;

	private HashMap<Integer, Vector<String>> validTransformations;

	private HashMap<Integer, Vector<String>> invalidTransformations;

	private HashMap<Integer, Vector<String>> invalidTransformationsMsgs;

	private String msg;

	public CatalogueListener(CatalogueGUI parent) {
		this.parent = parent;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		String trans, t;
		int in;
		boolean result;

		if (e.getActionCommand().equals("Menu:ApplyTransformation")
				|| e.getActionCommand().equals(
						"CatalogueToolBar:ApplyTransformation")) {

			trans = parent.getSelectedElement();

			if (trans == null)
				return;

			t = Catalogue.getInstance().getEngineName().get(trans);

			result = testTransformation(t);

			if (result) {
				in = parent.getSelectedIndex();
				Console.getInstance().applyTransformation(t, "");
				parent.setSelectedIndex(in);
				validTransformations = null;
				invalidTransformations = null;
			} else {
				JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
						"Transformation applicability check failed!\n" + msg,
						"Message", JOptionPane.ERROR_MESSAGE);
			}

		}
		if (e.getActionCommand().equals("Menu:ApplyTransformationWithArgument")
				|| e.getActionCommand().equals(
						"CatalogueToolBar:ApplyTransformationWithArgument")) {

			String args;

			trans = parent.getSelectedElement();

			if (trans == null)
				return;

			t = Catalogue.getInstance().getEngineName().get(trans);

			result = testTransformation(t);

			if (result) {
				args = JOptionPane
						.showInputDialog("Please enter the argument(s) for the transformation:");
				in = parent.getSelectedIndex();
				Console.getInstance().applyTransformation(t, args);
				parent.setSelectedIndex(in);
				validTransformations = null;
				invalidTransformations = null;
			} else {
				JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
						"Transformation applicability check failed!\n" + msg,
						"Message", JOptionPane.ERROR_MESSAGE);
			}

		} else if (e.getActionCommand().equals("Menu:TestTransformation")
				|| e.getActionCommand().equals(
						"CatalogueToolBar:TestTransformation")) {

			trans = parent.getSelectedElement();

			if (trans == null)
				return;

			t = Catalogue.getInstance().getEngineName().get(trans);

			result = testTransformation(t);

			if (result)
				parent.setSelectionColor(Color.BLACK, Color.GREEN);
			else {
				parent.setSelectionColor(Color.BLACK, Color.RED);
				JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
						"Transformation applicability check failed!\n" + msg,
						"Message", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals("Menu:HelpTransformation")
				|| e.getActionCommand().equals(
						"CatalogueToolBar:HelpTransformation")) {

			trans = parent.getSelectedElement();

			if (trans == null)
				return;

			JOptionPane.showMessageDialog(MainFrame.getMainFrame(), Catalogue
					.getInstance().getDescription().get(trans));
		}

		else if (e.getActionCommand().equals("Menu:TestAllTransformation")
				|| e.getActionCommand().equals(
						"CatalogueToolBar:TestAllTransformation")) {

			parent.selectElements(Console.getInstance()
					.testAllTransformations(), Color.BLACK, Color.GREEN);

		} else {
			parent.setList(e.getActionCommand());
		}

	}

	public void invalidTransformationApplicability() {
		validTransformations = null;
	}

	private boolean testTransformation(String trans) {
		msg = "";

		if (((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"))
				.getSelectedNode() == null)
			return false;

		int currentRow = ((TreeGUI) ComponentRegistry
				.getGUIComponent("TreeGUI")).getSelectedNode().getRow();
		boolean result;

		// If the cache is empty or invalid (Document has been modified)
		// recreate it
		if (validTransformations == null
				|| ((EditorGUI) ComponentRegistry.getGUIComponent("EditorGUI"))
						.isDocumentModified()) {
			validTransformations = new HashMap<Integer, Vector<String>>();
			invalidTransformations = new HashMap<Integer, Vector<String>>();
			invalidTransformationsMsgs = new HashMap<Integer, Vector<String>>();
		}
		// If the cache is valid than use it
		else {
			if (validTransformations.get(new Integer(currentRow)) != null) {
				if (validTransformations.get(new Integer(currentRow)).contains(
						trans)) {
					return true;
				}
			}

			if (invalidTransformations.get(new Integer(currentRow)) != null) {
				if (invalidTransformations.get(new Integer(currentRow))
						.contains(trans)) {
					msg = invalidTransformationsMsgs.get(
							new Integer(currentRow)).elementAt(
							(invalidTransformations
									.get(new Integer(currentRow))
									.indexOf(trans)));
					return false;
				}
			}
		}

		result = Console.getInstance().testTransformation(trans);
		if (result) {
			if (validTransformations.get(new Integer(currentRow)) == null)
				validTransformations.put(new Integer(currentRow),
						new Vector<String>());
			validTransformations.get(new Integer(currentRow)).add(trans);
		} else {
			if (invalidTransformations.get(new Integer(currentRow)) == null) {
				invalidTransformations.put(new Integer(currentRow),
						new Vector<String>());
				invalidTransformationsMsgs.put(new Integer(currentRow),
						new Vector<String>());
			}
			invalidTransformations.get(new Integer(currentRow)).add(trans);
			msg = Console.getInstance().getLastErrorMessage();
			invalidTransformationsMsgs.get(new Integer(currentRow)).add(msg);
		}

		return result;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
	 */
	public void valueChanged(ListSelectionEvent e) {
		parent.setSelectionColor(Color.BLACK, Color.YELLOW);
	}
}
