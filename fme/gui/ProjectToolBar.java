/**
 * Project: fuml
 */

package fme.gui;

import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JToolBar;

import fme.components.MainEventHandler;

/**
 * This class represents the toolbar above the graphical tree
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class ProjectToolBar extends JToolBar {

	private static final long serialVersionUID = 8406974568198044633L;

	private File prj;

	private JComboBox box;

	private JButton projectName;

	/**
	 * The Constructor
	 */
	public ProjectToolBar(MainEventHandler listener) {

		this.setFloatable(false);

		box = new JComboBox();
		box.addActionListener(listener);
		projectName = new JButton("Project: - ");
		projectName.setActionCommand("Menu:ShowProjectManager");
		projectName.addActionListener(listener);
		projectName.setBorderPainted(false);
		add(projectName);
		add(new JLabel("    File: "));
		add(box);
	}

	/**
	 * Set all available filenames in the project
	 * 
	 * @param filenames
	 *            The available filenames
	 */
	// public void setFilenames(Vector<File> filenames) {
	public void update() {
		if (prj == null)
			return;
		File dir = prj.getParentFile();
		Vector<String> list = new Vector<String>();
		Iterator<String> it;

		// Get files
		scanDir(dir, list);
		// Sort files
		Collections.sort(list, new wslFileListComparator());
		// Add items to ComboBox
		box.removeAllItems();
		it = list.iterator();
		while (it.hasNext()) {
			box.addItem(it.next());
		}
		box.revalidate();
	}

	private void scanDir(File dir, Vector<String> list) {
		String file;
		for (int i = 0; i < dir.list().length; i++) {
			file = dir.getAbsolutePath() + File.separator + dir.list()[i];
			if (new File(file).isFile()) {
				if (file.endsWith(".wsl")) {
					list.add(file);
				}
			} else {
				// If to scan subdirectories
				// scanDir(new File(file),list);
			}
		}
	}

	/**
	 * Set current project
	 * 
	 * @param name
	 *            The name of the project
	 */
	public void setProject(String name, File prj) {
		this.prj = prj;
		projectName.setText("Project: " + name + " ");
		projectName.revalidate();
	}

	/**
	 * Get the selected file
	 * 
	 * @return The current selected File
	 */
	public String getSelectedFile() {
		return (String) box.getSelectedItem();
	}

	private class wslFileListComparator implements Comparator<Object> {
		public int compare(Object o1, Object o2) {
			String s1 = (String) o1, s2 = (String) o2;
			String t1, t2;
			int n1, n2;

			try {
				t1 = s1.substring(s1.lastIndexOf("-") + 1, s1.length() - 4);
				n1 = Integer.parseInt(t1);
			} catch (Exception e) {
				n1 = -1;
			}
			try {
				t2 = s2.substring(s2.lastIndexOf("-") + 1, s2.length() - 4);
				n2 = Integer.parseInt(t2);
			} catch (Exception e) {
				n2 = -1;
			}

			if (n1 == -1 && n2 == -1)
				return 0;
			else if (n1 == -1)
				return -1;
			else if (n2 == -1)
				return 1;
			else if (n1 == n2)
				return 0;
			else if (n1 < n2)
				return -1;
			else
				return 1;
		}
	}
}
