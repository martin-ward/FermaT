/**
 * Project: fme
 */

package fme.components.catalogue;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;

import fme.components.abstraction.GUIComponent;

/**
 * 
 * This class is a graphical representation of the transformation catalogue of
 * the FermaT Engine
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class CatalogueGUI extends GUIComponent {

	private CatalogueListener listener;

	private HashMap<String, Vector<String>> groups;

	private String currentList;

	private JList list;

	/**
	 * Defined for serialization
	 */
	private static final long serialVersionUID = -8554718626713102191L;

	/**
	 * (non-Javadoc)
	 * 
	 * @see fme.components.abstraction.GUIComponent#getName()
	 */
	public String getName() {
		return "Transformation Catalogue";
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see fme.components.abstraction.GUIComponent#initComponent()
	 */
	protected boolean initComponent() {
		String keyw[], name;
		Vector<String> names;
		Iterator<String> i;

		groups = new HashMap<String, Vector<String>>();
		listener = new CatalogueListener(this);
		setLayout(new BorderLayout());
		getInternalFrame().setJMenuBar(new CatalogueMenuBar(listener));
		// Create groups
		names = Catalogue.getInstance().getNames();
		i = names.iterator();
		while (i.hasNext()) {
			name = i.next();
			keyw = Catalogue.getInstance().getKeywords().get(name).split(" ");
			for (int j = 0; j < keyw.length; j++) {
				if (groups.get(keyw[j]) == null)
					groups.put(keyw[j], new Vector<String>());
				groups.get(keyw[j]).add(name);
			}
		}
		groups.put("All", names);

		currentList = "All";

		// Add the list
		list = new JList(names);
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.addListSelectionListener(listener);
		setToolBar(new CatalogueToolBar(listener));

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(4, Math.round(Math.round(names
				.size() / 4))));

		// Button "All" should be the first one
		JButton button = new JButton("All");
		button.setActionCommand("All");
		button.addActionListener(listener);
		button.setBorderPainted(false);
		button.setHorizontalTextPosition(2);
		button.setBorder(null);
		button.setHorizontalAlignment(SwingConstants.CENTER);
		buttonPanel.add(button);

		i = groups.keySet().iterator();
		while (i.hasNext()) {
			name = i.next();
			if (name.equals("All"))
				name = i.next();
			button = new JButton(name);
			button.setActionCommand(name);
			button.addActionListener(listener);
			button.setBorderPainted(false);
			button.setHorizontalTextPosition(2);
			button.setBorder(null);
			button.setHorizontalAlignment(SwingConstants.CENTER);
			buttonPanel.add(button);
		}

		add(buttonPanel, BorderLayout.NORTH);
		add(new JScrollPane(list));

		return true;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see fme.components.abstraction.GUIComponent#update()
	 */
	public void update() {
		listener.invalidTransformationApplicability();
	}

	public String getSelectedElement() {
		return (String) list.getSelectedValue();
	}

	public int getSelectedIndex() {
		return list.getSelectedIndex();
	}

	public void setSelectedIndex(int i) {
		list.setSelectedIndex(i);
	}

	public Vector<String> getSelectedList() {
		return groups.get(currentList);
	}

	public void setList(String name) {
		if (groups.get(name) != null) {
			list.setListData(groups.get(name));
			list.revalidate();
			currentList = name;
		}
	}

	public void removeSelection() {
		list.removeSelectionInterval(0, list.getModel().getSize());
	}

	public void setSelectionColor(Color f, Color b) {
		list.setSelectionForeground(f);
		list.setSelectionBackground(b);
	}

	/**
	 * Selects multiple elements.
	 * 
	 * @param elements
	 *            The elements of the main list which should be highlighted. If
	 *            a group is selected only the relevant fields are highlighted!
	 * @param f
	 *            Foreground color
	 * @param b
	 *            Background color
	 */
	public void selectElements(int[] elements, Color f, Color b) {
		if (elements == null)
			return;

		int clElements[];
		Vector<Integer> clElementsV = new Vector<Integer>();
		Vector<String> all = groups.get("All");
		
		Vector<String> current = groups.get(currentList);
		for (int i = 0; i < elements.length; i++) {
			if (elements[i] == -1) {
				continue;
			}
			if (current.contains(all.get(elements[i]))) {
				clElementsV.add(new Integer(current.indexOf(all
						.get(elements[i]))));
			}
		}
		clElements = new int[clElementsV.size()];
		for (int i = 0; i < clElementsV.size(); i++) {
			clElements[i] = clElementsV.get(i).intValue();
		}
		list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		list.setSelectedIndices(clElements);
		list.setSelectionForeground(f);
		list.setSelectionBackground(b);
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	}
}
