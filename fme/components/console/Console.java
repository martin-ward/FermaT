/**
 * Project: fme
 */

package fme.components.console;

import java.io.File;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JTextArea;

import fme.components.ComponentRegistry;
import fme.components.IOManager;
import fme.components.ProjectManager;
import fme.components.UndoManager;
import fme.components.catalogue.Catalogue;
import fme.components.editor.EditorGUI;
import fme.components.tree.TreeGUI;
import fme.config.CM;
import fme.gui.MainFrame;
import fme.wsl.ast.AST;

/**
 * This class provides a high level interface to the FermaT console. It
 * encapsulates basic functions like loading a wsl file
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class Console {

	/**
	 * The only instance of this class (singleton design pattern)
	 */
	private static Console singleton = new Console();

	private boolean documentErroneous;

	private String workingFile;

	// This is for the Genetic Algorithm and Transformation Sequence Manager.
	public static boolean ignoreErrors = false;

	/**
	 * Indicates if the engine is extended writing output
	 */
	private boolean debug = true;

	/**
	 * The Constructor
	 */
	private Console() {
	}

	/**
	 * Get the Console
	 * 
	 * @return The only instance of the Console
	 */
	public static Console getInstance() {
		return singleton;
	}

	/**
	 * Choose a file to save
	 */
	public void saveFileChooser() {
		String filename;
		File file;
		// Choose a file
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
		chooser.setFileFilter(new WSLFileFilter());
		chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

		if (chooser.showSaveDialog(MainFrame.getMainFrame()) == JFileChooser.APPROVE_OPTION) {

			filename = chooser.getSelectedFile().getAbsolutePath();
			if (!filename.endsWith(".wsl"))
				filename = filename + ".wsl";
			file = new File(filename);
			// Ask for overwrite if necessary
			if (file.exists()) {
				int ret = JOptionPane.showConfirmDialog(MainFrame
						.getMainFrame(), "File exists. Overwrite?", "Question",
						JOptionPane.YES_NO_OPTION);
				if (ret != JOptionPane.YES_OPTION)
					return;
			}

			saveFile(file);
		}
	}

	/**
	 * Choose a file to save
	 */
	public void exportWSLFile() {
		String filename;
		File file;
		// Choose a file
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
		chooser.setFileFilter(new WSLFileFilter());
		chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

		if (chooser.showSaveDialog(MainFrame.getMainFrame()) == JFileChooser.APPROVE_OPTION) {

			filename = chooser.getSelectedFile().getAbsolutePath();
			if (!filename.endsWith(".wsl"))
				filename = filename + ".wsl";
			file = new File(filename);
			// Ask for overwrite if necessary
			if (file.exists()) {
				int ret = JOptionPane.showConfirmDialog(MainFrame
						.getMainFrame(), "File exists. Overwrite?", "Question",
						JOptionPane.YES_NO_OPTION);
				if (ret != JOptionPane.YES_OPTION)
					return;
			}

			EditorGUI eg = ((EditorGUI) ComponentRegistry
					.getGUIComponent("EditorGUI"));

			if (!IOManager.saveStringToFile(file, eg.getText())) {
				JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
						"Can't save to file: " + file, "Message",
						JOptionPane.ERROR_MESSAGE);
				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.SEVERE, "Can't save to file " + file);
				return;
			}
		}
	}

	// Console Methods
	// ===============

	/**
	 * Get the last error Message from the engien
	 */
	public String getLastErrorMessage() {
		String buf;
		buf = sendBufferCommand("(@Fail_Message)\n", null);
		return buf;
	}

	/**
	 * Save the file to a user-selected file
	 */
	public void saveFile(File file) {
		int d;

		EditorGUI eg = ((EditorGUI) ComponentRegistry
				.getGUIComponent("EditorGUI"));

		d = eg.getDot();

		if (!IOManager.saveStringToFile(file, eg.getText())) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"Can't save to file: " + file, "Message",
					JOptionPane.ERROR_MESSAGE);
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Can't save to file " + file);
			return;
		}

		ProjectManager.getProjectManager().recordEvent("Save", file, null);

		// Load the file into the environment
		loadFile(file);

		eg.setDot(d);
		eg.enableBlockColoring();
	}

	/**
	 * Save the file to <name>-<number>.wsl where number is increasing each save
	 */
	public void quickSaveFile() {

		int d;
		String filename;

		if (workingFile == null) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"No file was selected", "Message",
					JOptionPane.ERROR_MESSAGE);
			saveFileChooser();
			return;
		}

		// Construct a new Filename
		filename = IOManager.extendWSLFileName(workingFile);

		// Save contents of the editor to the file
		EditorGUI eg = ((EditorGUI) ComponentRegistry
				.getGUIComponent("EditorGUI"));

		d = eg.getDot();

		if (!IOManager.saveStringToFile(filename, eg.getText())) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"Can't save to file: " + filename, "Message",
					JOptionPane.ERROR_MESSAGE);
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Can't save to file " + filename);
			return;
		}

		ProjectManager.getProjectManager().recordEvent("QuickSave",
				new File(filename), null);

		// Load the file into the environment
		loadFile(new File(filename));

		eg.setDot(d);
		eg.enableBlockColoring();
	}

	/**
	 * Save the file back to it's original file and delete all intermedia files
	 * <name>-<number>.wsl
	 */
	public void finalSaveFile() {

		int d;
		String filename, filename2;

		if (workingFile == null) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"No file was selected", "Message",
					JOptionPane.ERROR_MESSAGE);
			saveFileChooser();
			return;
		}

		// Construct a new file name
		if (workingFile.matches(".*-[0-9]+.wsl")) {
			filename = workingFile.substring(0, workingFile.lastIndexOf("-"))
					+ ".wsl";
		} else {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"Current file is not an intermediate file", "Message",
					JOptionPane.ERROR_MESSAGE);
			return;
		}

		// Save contents of the editor to the file
		EditorGUI eg = ((EditorGUI) ComponentRegistry
				.getGUIComponent("EditorGUI"));

		d = eg.getDot();

		if (!IOManager.saveStringToFile(filename, eg.getText())) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"Can't save to file: " + filename, "Message",
					JOptionPane.ERROR_MESSAGE);
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Can't save to file " + filename);
			return;
		}

		ProjectManager.getProjectManager().recordEvent("FinalSave",
				new File(filename), null);

		// Load the file into the environment
		loadFile(new File(filename));

		eg.setDot(d);
		eg.enableBlockColoring();

		// Delete all intermedia files
		File dir = new File(filename).getParentFile();
		File f[] = dir.listFiles();
		filename2 = filename.substring(0, filename.length() - 4).replace("\\",
				"\\\\");
		for (int i = 0; i < f.length; i++) {
			if (f[i].getAbsolutePath().matches(filename2 + ".*-[0-9]+.wsl")) {
				if (!f[i].delete()) {
					Logger.getLogger(this.getClass().getCanonicalName())
							.log(Level.SEVERE,
									"Can't delete file " + f[i].getName());
				}
			}
		}

		// Delete transformation history
		ProjectManager.getProjectManager().deleteHistory();
	}

	/**
	 * Run the current WSL file
	 */
	public void runFile() {
		ExecutionConsole.start(CM.getAsString("ExecutionCommand") + " "
				+ workingFile, workingFile);
	}

	/**
	 * Compute metrics for the current WSL file
	 */
	public void metricsFile() {
		ExecutionConsole.start(CM.getAsString("MetricsCommand") + " "
				+ workingFile, workingFile);
	}

	/**
	 * Apply transformation to current item
	 */
	public boolean applyTransformation(String trans, String data) {
		boolean ret = false;
		int row = ((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"))
				.getSelectedNode().getRow();
		String buf, file, oldfile, newPos, cmd;

		cmd = "(@TRANS " + trans + " \"" + data + "\")\n";
		buf = sendBufferCommand("(@TRANS " + trans + " \"" + data
				+ "\")(display \"***EndOfOperation***\")\n",
				"***EndOfOperation***");
		buf = buf.replace("***EndOfOperation***", "");

		if (debug) {
			System.out.println("Sending:" + cmd);
			System.out.println("Received:\n" + buf);
		}

		if (buf.toLowerCase().contains("error") && !ignoreErrors) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"Transformation failed!");
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Transformation " + trans + " failed!");
			return false;
		} else if (buf.toLowerCase().contains("error")) {
			return false;
		} else if (!ignoreErrors) {
			JTextArea graphicConsole = ((ConsoleGUI) ComponentRegistry
					.getGUIComponent("ConsoleGUI")).getTextArea();
			graphicConsole.append(buf + "\n");
		}

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Transformation " + trans + " was successful!");
		ret = true;

		// Get the new position of the current item
		newPos = sendBufferCommand("(@posn)\n", ")");
		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"New Position is:" + newPos);
		newPos = newPos.substring(1, newPos.indexOf(")"));

		// Construct a new Filename
		file = IOManager.extendWSLFileName(workingFile);

		// Now write and load the new file
		buf = sendBufferCommand("(@PP_Item (@Program) 80 \""
				+ file.replace("\\", "\\\\") + "\")\n", null);

		oldfile = workingFile;

		TreeGUI tg = ((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"));

		ProjectManager.getProjectManager().recordEvent(
				"ApplyTransformation#" + trans + "#Data#" + data,
				new File(file), tg.getSelectedNode());

		loadFile(new File(file));

		UndoManager.getInstance().storeEvent(
				UndoManager.TRANSFORMATION_APPLIED,
				oldfile + "#" + file + "#" + row);

		// Set the new position in the tree
		try {
			tg.expandTreeToPosn(newPos.split(" "));
		} catch (Exception e) {
		}

		return ret;
	}

	/**
	 * Test if transformation is possible to current item
	 */
	public boolean testTransformation(String trans) {
		boolean ret = false;
		String buf;

		// Test transformation of current item
		buf = sendBufferCommand("(@TRANS? " + trans + ")\n", "#");
		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Test transformation: " + trans + " result:" + buf);

		if (buf.contains("#t"))
			ret = true;

		return ret;
	}

	public int[] testAllTransformations() {
		String buf, alist, names, n[], a[];
		int ret[];

		// Test transformation of current item
		buf = sendBufferCommand("(@What_Trans ())\n", ")");
		if (buf.contains("ERROR"))
			return null;
		alist = buf.substring(1, buf.indexOf(")"));
		buf = sendBufferCommand("//T/Rs_/Name\n", "()");
		names = buf.substring(2, buf.indexOf("()") - 1).replace("`", "")
				.replace("'", "");
		n = names.split("\" \"");
		a = alist.split(" ");
		ret = new int[a.length];
		buf = "";

		for (int i = 0; i < a.length; i++) {
			try {
				int nameIndex = Catalogue.getInstance().getNames().indexOf(
						n[Integer.parseInt(a[i])]);

				if (nameIndex == -1) {
					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.WARNING,
							"Cannot find transformation: " + n[Integer.parseInt(a[i])]);
				}

				buf = buf + " " + n[Integer.parseInt(a[i]) - 1] + "("
						+ nameIndex + ")";
				ret[i] = Catalogue.getInstance().getNames().indexOf(
						n[Integer.parseInt(a[i]) - 1]);
			} catch (Exception e) {
			}
		}
		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Test all transformations - result: " + buf);
		return ret;
	}

	/**
	 * Update the tree position
	 */
	public void updateTreePosition(String position) {

		// Reset the @I pointer
		sendBufferCommand("(@New_Program (@Program))\n", "#<unspecified>");

		// Set position
		sendBufferCommand("(" + position.replace(" ", ")\n(") + ")\n",
				"#<unspecified>");
	}

	/**
	 * This will load a WSL file into the engine and will update the AST in the
	 * FME
	 * 
	 * @param file
	 *            The WSL file to load
	 */
	public boolean loadFile(File file) {
		String buf, filename = file.getAbsolutePath();

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Loading file:" + filename);

		workingFile = filename;

		// Load the file into the engine
		filename = filename.replace("\\", "\\\\");

		buf = sendBufferCommand("(@New_Program (@Parse_File \"" + filename
				+ "\" //T_/Statements))\n", null);

		// Show an error if something goes wrong ...
		if (buf.contains("not found!")) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"File not found!", "Message", JOptionPane.ERROR_MESSAGE);
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "File not found");
			workingFile = null;
			return false;
		}

		MainFrame.getMainFrame().setTitle(
				"FermaT Maintenance Environment" + " - "
						+ filename.replace("\\\\", "\\"));

		if (buf.toLowerCase().contains("error") && !ignoreErrors) {
			JOptionPane
					.showMessageDialog(
							MainFrame.getMainFrame(),
							"The WSL contains syntax errors (see console for details).",
							"Message", JOptionPane.ERROR_MESSAGE);
			((ConsoleGUI) ComponentRegistry.getGUIComponent("ConsoleGUI"))
					.getTextArea().append(buf);
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "The WSL contains syntax errors");

			ComponentRegistry
					.updateGUI(ComponentRegistry.NEW_FILE_WITH_ERROR_LOADED);
			documentErroneous = true;

			// Remove all text markings
			EditorGUI eg = ((EditorGUI) ComponentRegistry
					.getGUIComponent("EditorGUI"));
			eg.removeMarks();

			return true;
		}

		documentErroneous = false;

		buf = sendBufferCommand("(@Print_WSL (@Program) \"\")\n", "\n#t");

		// Set the file
		AST.setFile(new File(filename));

		// Parse the Tree
		if (!AST.parseTree(buf)) {
			ComponentRegistry
					.updateGUI(ComponentRegistry.NEW_FILE_WITH_ERROR_LOADED);

			documentErroneous = true;

			// Remove all text markings
			EditorGUI eg = ((EditorGUI) ComponentRegistry
					.getGUIComponent("EditorGUI"));
			eg.removeMarks();

			return true;
		}

		// Update the GUI
		ComponentRegistry.updateGUI(ComponentRegistry.NEW_FILE_LOADED);

		// Remove all old text markings
		EditorGUI eg = ((EditorGUI) ComponentRegistry
				.getGUIComponent("EditorGUI"));
		eg.removeMarks();

		// Select the first Node in the AST
		TreeGUI tg = ((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"));
		tg.expandTreeToRow(0);

		ProjectManager.getProjectManager().recordEvent("LoadFile", file, null);
		UndoManager.getInstance().resetEditorEventQueue();

		return true;
	}

	/**
	 * Get the working file of the console
	 * 
	 * @return The working file
	 */
	public File getWorkingFile() {
		if (workingFile == null)
			return null;
		return new File(workingFile);
	}

	/**
	 * Indicates if the document contains syntax errors
	 * 
	 * @return True if the document has syntax errors / False if not
	 */
	public boolean isDocumentErroneous() {
		return documentErroneous;
	}

	// Internal Methods
	// ================

	/**
	 * Send a command to the engine and return the result in a buffer (not on
	 * the graphical console)
	 * 
	 * @param cmd
	 *            The command to send
	 * @param waitFor
	 *            A character sequence which indicates the end of the result
	 * @return The buffer holding the result
	 */
	private String sendBufferCommand(String cmd, String waitFor) {
		String ret;
		// Tell the transformation engine to load the file
		ConsoleObserver co = ConsoleObserver.getConsoleObserver();
		co.setSendToBuffer(true);
		co.setWaitForString(waitFor);
		co.sendCommand(cmd);

		while (co.isBusy()) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException ex) {
			}
		}

		ret = co.getBuffer();
		co.setWaitForString(null);
		co.setSendToBuffer(false);

		return ret;
	}

	// Internal Classes
	// ================

	/**
	 * File Filter Class for XMI Files
	 */
	private class WSLFileFilter extends javax.swing.filechooser.FileFilter {

		public boolean accept(File f) {
			if (f.getName().endsWith(".wsl"))
				return true;
			else if (f.isDirectory())
				return true;
			else
				return false;
		}

		public String getDescription() {
			return "WSL Files";
		}
	}
}
