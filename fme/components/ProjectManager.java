package fme.components;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import fme.components.console.Console;
import fme.config.CM;
import fme.gui.MainFrame;
import fme.gui.MainMenuBar;
import fme.wsl.ast.ASTNode;

/**
 * The class represents the Project Manager
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 * 
 */
public class ProjectManager {

	private static ProjectManager singleton = new ProjectManager();

	private JFrame pmFrame;

	private JToolBar toolBar;

	private JLabel title;

	private JList list;

	private String[] toolTipText = { "New Project", "Open Project",
			"Save Project", "Close Project", "Open file from Project",
			"Create Empty File In Project", "Add file to Project",
			"Remove File From Project", "Show Modification History",
			"Delete History" };

	private String[] icon = { "icons/new.gif", "icons/open.gif",
			"icons/save.gif", "icons/close.gif", "icons/open2.gif",
			"icons/add2.gif", "icons/add.gif", "icons/remove.gif",
			"icons/align_view.gif", "icons/align.gif" };

	private String[] actionCommand = { "Menu:NewProject", "Menu:OpenProject",
			"Menu:SaveProject", "Menu:CloseProject", "Menu:LoadFile",
			"Menu:CreateNewToProject", "Menu:AddToProject",
			"Menu:RemoveFromProject", "Menu:ShowHistory", "Menu:DeleteHistory" };

	private boolean error = false;

	private String name = "-";

	private static File prjFile;

	private Vector<File> files;

	private File currentFile;

	private HashMap<String, Vector<String>> history = new HashMap<String, Vector<String>>();

	/**
	 * The Constructor
	 */
	private ProjectManager() {
		JButton button;

		pmFrame = new JFrame("ProjectManager");
		pmFrame.setLocation(CM.getAsInt("gui.ProjectManager.InitXPos"), CM
				.getAsInt("gui.ProjectManager.InitYPos"));
		pmFrame.setSize(CM.getAsInt("gui.ProjectManager.InitWidth"), CM
				.getAsInt("gui.ProjectManager.InitHeight"));
		pmFrame.setVisible(false);

		// Handling for window closing event
		pmFrame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				((JFrame) e.getSource()).setVisible(false);
			}
		});

		// ToolBar
		toolBar = new JToolBar();
		toolBar.setFloatable(false);
		title = new JLabel("Project: - ");
		toolBar.add(title);
		toolBar.add(new JLabel("    "));
		for (int i = 0; i < toolTipText.length; i++) {
			button = new JButton();
			button.setToolTipText(toolTipText[i]);
			button.setIcon(new ImageIcon(icon[i]));
			button.setActionCommand(actionCommand[i]);
			button
					.addActionListener(MainFrame.getMainFrame()
							.getEventHandler());
			toolBar.add(button);
			if (i == 3 || i == 7)
				toolBar.add(new JLabel("    "));
		}
		pmFrame.getContentPane().add(toolBar, BorderLayout.PAGE_START);
		list = new JList();
		pmFrame.getContentPane()
				.add(new JScrollPane(list), BorderLayout.CENTER);
	}

	/**
	 * Get the only instance of the ProjectManager
	 * 
	 * @return The ProjectManager
	 */
	public static ProjectManager getProjectManager() {
		return singleton;
	}

	/**
	 * Show the ProjectManager
	 */
	public void showProjectManager() {
		pmFrame.setVisible(true);
	}

	/**
	 * Load a project file
	 * 
	 * @param file
	 *            The project file to load
	 */
	public void loadProjectFile(File file) {

		System.setProperty("org.xml.sax.driver",
				"org.apache.xerces.parsers.SAXParser");

		error = false;

		prjFile = file;

		try {
			XMLReader xmlReader = XMLReaderFactory.createXMLReader();
			DefaultHandler handler = new ProjectFileParser(file);
			xmlReader.setContentHandler(handler);
			xmlReader.parse(file.toURI().toURL().toString());
		} catch (SAXException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "SAX: XML Parser Error");
		} catch (IOException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "SAX: IO Error");
		}

		if (error)
			JOptionPane
					.showMessageDialog(
							MainFrame.getMainFrame(),
							"One or more files of project "
									+ name
									+ " could not be loaded. Check log for details ...",
							"Message", JOptionPane.ERROR_MESSAGE);

		// Update FME
		MainFrame.getMainFrame().getEventHandler().setDisable(true);
		MainFrame.getMainFrame().getProjectToolBar().setProject(name, prjFile);
		MainFrame.getMainFrame().getProjectToolBar().update();
		MainFrame.getMainFrame().getEventHandler().setDisable(false);
		list.setListData(files);
		title.setText("Project: " + name + " ");
		title.revalidate();

		if (currentFile != null && currentFile.exists()) {
			Console.getInstance().loadFile(currentFile);
		} else {
			if (currentFile == null) {
				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.INFO, "No file was opened in the project.");
			} else {
				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.WARNING, "Can't open last file:" + currentFile);
			}
		}
		MainMenuBar.setEnableNonProjectMenues(true);
	}

	/**
	 * Close current project file
	 */
	public void closeProjectFile() {
		int choice = 1;

		if (files != null && files.size() != 0)
			choice = JOptionPane.showConfirmDialog(null,
					"Save modification history in project ?");

		if (choice == 2)
			return;
		// Save project before close
		if (choice == 0)
			saveProjectFile();

		name = "-";
		files = new Vector<File>();

		// Update FME
		MainFrame.getMainFrame().getProjectToolBar().setProject(name, null);
		MainFrame.getMainFrame().getProjectToolBar().update();
		list.setListData(files);
		title.setText("Project: " + name + " ");
		title.revalidate();
		deleteHistory();
		MainMenuBar.setEnableNonProjectMenues(false);
	}

	/**
	 * Create a new project file
	 */
	public void newProjectFile() {
		String name = JOptionPane.showInputDialog(pmFrame,
				"Please enter a name for the new project:");

		if (name == null)
			return;

		JOptionPane.showMessageDialog(pmFrame,
				"Now choose the project directory");

		JFileChooser chooser;
		if (CM.getAsString("StartDirectory").equals("_Home_")) {
			chooser = new JFileChooser(
					new File(System.getProperty("user.home")));
		} else if (CM.getAsString("StartDirectory").equals("_CurrentDir_")) {
			chooser = new JFileChooser(new File(System.getProperty("user.dir")));
		} else {
			chooser = new JFileChooser(new File(CM
					.getAsString("StartDirectory")));
		}
		chooser.setLocale(Locale.UK);
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		if (chooser.showOpenDialog(pmFrame) == JFileChooser.APPROVE_OPTION) {
			prjFile = new File(chooser.getSelectedFile().getAbsolutePath()
					+ File.separator + name + ".fpf");
			files = new Vector<File>();
			this.name = name;
			currentFile = null;

			System.out.println("1");

			// Update FME
			MainFrame.getMainFrame().getProjectToolBar().setProject(name,
					prjFile);
			MainFrame.getMainFrame().getProjectToolBar().update();
			System.out.println("2");
			list.setListData(files);
			title.setText("Project: " + name + " ");
			title.revalidate();
			deleteHistory();
			saveProjectFile();
			MainMenuBar.setEnableNonProjectMenues(true);
		}
	}

	/**
	 * Save current project file
	 */
	public void saveProjectFile() {
		StringBuffer save = new StringBuffer();
		Iterator<String> keys;
		String key, ap1, ap2;

		if (name.equals("-"))
			return;

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Saving project file...");

		save.append("<FME_Project name=\"" + name + "\">\n");
		save.append("  <Project_Contents>\n");
		for (int i = 0; i < files.size(); i++) {
			ap1 = prjFile.getAbsolutePath().replace(prjFile.getName(), "");
			ap2 = files.get(i).getAbsolutePath().replace(
					files.get(i).getName(), "");
			if (ap2.startsWith(ap1)) {
				save.append("    <File name=\"" + ap2.replace(ap1, "")
						+ files.get(i).getName() + "\" />\n");
			} else {
				save.append("    <File name=\"" + files.get(i) + "\" />\n");
			}
		}
		save.append("  </Project_Contents>\n");
		if (currentFile != null)
			save.append("  <Project_History current_file=\""
					+ absoluteToRelative(currentFile) + "\">\n");
		else
			save.append("  <Project_History current_file=\"\">\n");
		keys = history.keySet().iterator();
		while (keys.hasNext()) {
			key = keys.next();
			save.append("    <Item key=\""
					+ key
					+ "\" event=\""
					+ history.get(key).toString().substring(1,
							history.get(key).toString().length() - 1).replace(
							", ", ";") + "\" />\n");
		}
		save.append("  </Project_History>\n");
		save.append("</FME_Project>\n");
		IOManager.saveStringToFile(prjFile, save.toString());
	}

	/**
	 * Get the files within the project
	 * 
	 * @return A list of file objects
	 */
	public Vector<File> getFiles() {
		return files;
	}

	/**
	 * Get the frame of the ProjectManager
	 * 
	 * @return The frame of the ProjectManager
	 */
	public JFrame getFrame() {
		return pmFrame;
	}

	/**
	 * Get the current item from the list
	 * 
	 * @return The current selected item
	 */
	public File getCurrentItem() {
		return (File) list.getSelectedValue();
	}

	/**
	 * Remove the current item from the list
	 */
	public void removeSelectedItems() {
		if (list.getSelectedIndex() != -1) {
			for (int i = list.getSelectedIndices().length - 1; i != -1; i--) {
				files.remove(list.getSelectedIndices()[i]);
			}
			MainFrame.getMainFrame().getProjectToolBar().update();
			list.setListData(files);
		}
	}

	public void addItem() {
		File file;
		String dir1, dir2;
		if (name.equals("-"))
			return;
		// Choose a file
		JFileChooser chooser = new JFileChooser(prjFile.getAbsoluteFile());
		chooser.setMultiSelectionEnabled(true);
		chooser.setLocale(Locale.UK);
		chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

		if (chooser.showOpenDialog(ProjectManager.getProjectManager()
				.getFrame()) == JFileChooser.APPROVE_OPTION) {
			for (int i = 0; i < chooser.getSelectedFiles().length; i++) {
				file = chooser.getSelectedFiles()[i];
				dir1 = prjFile.getParentFile().getAbsolutePath();
				dir2 = file.getParentFile().getAbsolutePath();
				if (dir2.contains(dir1)) {
					files.add(file);
				} else {
					JOptionPane
							.showMessageDialog(
									null,
									"Added files must be within the project directory.",
									"Error", JOptionPane.ERROR_MESSAGE);
				}
			}
			list.setListData(files);
			MainFrame.getMainFrame().getProjectToolBar().update();
		}
	}

	public void createItem() {
		if (name.equals("-"))
			return;
		// Choose a file
		JFileChooser chooser = new JFileChooser(prjFile.getAbsoluteFile());
		chooser.setMultiSelectionEnabled(false);
		chooser.setLocale(Locale.UK);
		chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

		if (chooser.showSaveDialog(ProjectManager.getProjectManager()
				.getFrame()) == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			if (!file.getAbsolutePath().endsWith(".wsl"))
				file = new File(file.getAbsoluteFile() + ".wsl");
			IOManager.saveStringToFile(file, "SKIP\n");
			files.add(file);
			list.setListData(files);
			MainFrame.getMainFrame().getProjectToolBar().update();
		}
	}

	public void recordEvent(String event, File file, ASTNode node) {
		String f;
		Logger.getLogger(this.getClass().getCanonicalName()).log(
				Level.INFO,
				"RecordEvent:" + event + " File:" + file.getName()
						+ " ASTNode:" + node);

		if (event.equals("LoadFile")) {
			currentFile = file;
			return;
		}

		if (currentFile == null)
			currentFile = file;

		if (history.get(absoluteToRelative(currentFile)) == null)
			history.put(absoluteToRelative(currentFile), new Vector<String>());

		if (event.equals("Save")) {
			history.get(absoluteToRelative(currentFile)).add(
					"Save#" + absoluteToRelative(file));
			if (history.get(absoluteToRelative(file)) != null) {
				history.get(absoluteToRelative(file)).removeAllElements();
				// ToDo: Delete references to this item in other history records
			}
		} else if (event.equals("QuickSave")) {
			history.get(absoluteToRelative(currentFile)).add(
					"QuickSave#" + absoluteToRelative(file));
		} else if (event.equals("FinalSave")) {
			// Remove all entries which start with the base file
			Iterator<String> keys = history.keySet().iterator();
			Vector<String> r = new Vector<String>();
			while (keys.hasNext()) {
				f = keys.next();
				if (f != null
						&& f.startsWith(absoluteToRelative(file).replace(
								".wsl", ""))) {
					r.add(f);
				}
			}
			keys = r.iterator();
			while (keys.hasNext()) {
				history.remove(keys.next());
			}
			if (history.get(absoluteToRelative(file)) != null) {
				history.get(absoluteToRelative(file)).removeAllElements();
			}
		} else if (event.equals("PrettyPrint")) {
			history.get(absoluteToRelative(currentFile)).add(
					"PrettyPrint#" + absoluteToRelative(file));
		} else if (event.startsWith("ApplyTransformation")) {
			history.get(absoluteToRelative(currentFile)).add(
					"Transformation#" + absoluteToRelative(file) + "#"
							+ event.replace("ApplyTransformation#", "") + "#"
							+ node.getRow());
		}
	}

	/**
	 * Get the history of changes
	 * 
	 * @return A HashMap with records of all changes of each file
	 */
	public HashMap<String, Vector<String>> getHistory() {
		return history;
	}

	/**
	 * Delete the history of changes
	 * 
	 * @return A HashMap with records of all changes of each file
	 */
	public void deleteHistory() {
		history = new HashMap<String, Vector<String>>();
	}

	public static String absoluteToRelative(File file) {
		if (file != null) {
			if (prjFile != null) {
				String ret = file.getAbsolutePath().replace(
						prjFile.getAbsolutePath()
								.replace(prjFile.getName(), ""), "");
				return ret;
			} else
				return file.getAbsolutePath();
		} else {
			return "";
		}
	}

	public static String relativeToAbsolute(String relativePath) {
		if (prjFile != null) {
			return prjFile.getAbsolutePath().replace(prjFile.getName(), "")
					+ relativePath;
		} else
			return relativePath;
	}

	// Internal Classes
	// ================

	private class ProjectFileParser extends DefaultHandler {

		private File prjFile;

		public ProjectFileParser(File prjFile) {
			files = new Vector<File>();
			this.prjFile = prjFile;
			history = new HashMap<String, Vector<String>>();
		}

		public void startElement(String uri, String localName, String qName,
				Attributes attributes) throws SAXException {
			File file;
			String fn;
			String[] e;
			if (qName.equals("File")) {

				if (File.separator.equals("/")) {
					fn = attributes.getValue(0).replace("\\", File.separator);
				} else {
					fn = attributes.getValue(0).replace("/", File.separator);
				}

				file = new File(fn);
				if (!file.exists()) {
					file = new File(prjFile.getAbsolutePath().replace(
							prjFile.getName(), "")
							+ fn);
				}
				if (!file.exists()) {
					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.SEVERE,
							"Couldn't load project file:"
									+ attributes.getValue(0));
					error = true;
				} else {
					files.add(file);
				}
			} else if (qName.equals("FME_Project")) {
				name = attributes.getValue(0);
			} else if (qName.equals("Project_History")) {
				if (File.separator.equals("/")) {
					fn = attributes.getValue(0).replace("\\", File.separator);
				} else {
					fn = attributes.getValue(0).replace("/", File.separator);
				}
				if (fn.length() > 0) {
					currentFile = new File(prjFile.getPath().replace(
							prjFile.getName(), "")
							+ fn);
				}
			} else if (qName.equals("Item")) {
				if (File.separator.equals("/")) {
					fn = attributes.getValue(0).replace("\\", File.separator);
				} else {
					fn = attributes.getValue(0).replace("/", File.separator);
				}
				history.put(fn, new Vector<String>());
				e = attributes.getValue(1).split(";");
				for (int i = 0; i < e.length; i++) {
					if (File.separator.equals("/")) {
						e[i] = e[i].replace("\\", File.separator);
					} else {
						e[i] = e[i].replace("/", File.separator);
					}
					history.get(attributes.getValue(0)).add(e[i]);
				}
			}
		}
	}
}
