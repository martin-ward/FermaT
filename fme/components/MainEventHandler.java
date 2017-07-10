/**
 */

package fme.components;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JDesktopPane;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import fme.components.actionsystemcallgraph.ActionSystemCallGraph;
import fme.components.console.Console;
import fme.components.editor.EditorGUI;
import fme.components.functionprocedurecallgraph.FunctionProcedureCallGraph;
import fme.components.historygraph.HistoryGraph;
import fme.config.CM;
import fme.gui.MainFrame;

/**
 * This class will handle all main events from the gui (e.g. menu selections)
 * Use the MainFrame class to get the instance.
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class MainEventHandler implements ActionListener {

	private boolean disable = false;

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		if (disable)
			return;

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Action Event:" + e.getActionCommand());

		// Events from the JMenuBar
		// ========================
		if (e.getActionCommand().equals("Menu:ShowProjectManager")) {
			ProjectManager.getProjectManager().showProjectManager();
		} else if (e.getActionCommand().equals("comboBoxChanged")) {
			if (MainFrame.getMainFrame().getProjectToolBar().getSelectedFile() != null) {
				Console.getInstance().loadFile(
						new File(MainFrame.getMainFrame().getProjectToolBar()
								.getSelectedFile()));
				EditorGUI eg = ((EditorGUI) ComponentRegistry
						.getGUIComponent("EditorGUI"));
				eg.removeMarks();
			}
		} else if (e.getActionCommand().equals("Menu:LoadFile")) {
			if (ProjectManager.getProjectManager().getCurrentItem() != null) {
				Console.getInstance().loadFile(
						ProjectManager.getProjectManager().getCurrentItem());
				EditorGUI eg = ((EditorGUI) ComponentRegistry
						.getGUIComponent("EditorGUI"));
				eg.removeMarks();
			}
		} else if (e.getActionCommand().equals("Menu:AddToProject")) {
			ProjectManager.getProjectManager().addItem();
		} else if (e.getActionCommand().equals("Menu:CreateNewToProject")) {
			ProjectManager.getProjectManager().createItem();
		} else if (e.getActionCommand().equals("Menu:RemoveFromProject")) {
			ProjectManager.getProjectManager().removeSelectedItems();
		} else if (e.getActionCommand().equals("Menu:NewProject")) {
			ProjectManager.getProjectManager().newProjectFile();
		} else if (e.getActionCommand().equals("Menu:OpenProject")) {
			// Choose a file
			JFileChooser chooser;
			if (CM.getAsString("StartDirectory").equals("_Home_")) {
				chooser = new JFileChooser(new File(System
						.getProperty("user.home")));
			} else if (CM.getAsString("StartDirectory").equals("_CurrentDir_")) {
				chooser = new JFileChooser(new File(System
						.getProperty("user.dir")));
			} else {
				chooser = new JFileChooser(new File(CM
						.getAsString("StartDirectory")));
			}
			chooser.setLocale(Locale.UK);
			chooser.setFileFilter(new FPFFileFilter());
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			if (chooser.showOpenDialog(ProjectManager.getProjectManager()
					.getFrame()) == JFileChooser.APPROVE_OPTION) {
				ProjectManager.getProjectManager().loadProjectFile(
						chooser.getSelectedFile());
			}
		} else if (e.getActionCommand().equals("Menu:SaveProject")) {
			ProjectManager.getProjectManager().saveProjectFile();
		} else if (e.getActionCommand().equals("Menu:CloseProject")) {
			ProjectManager.getProjectManager().closeProjectFile();
		} else if (e.getActionCommand().equals("Menu:DeleteHistory")) {
			ProjectManager.getProjectManager().deleteHistory();
		} else if (e.getActionCommand().equals("Menu:ShowHistory")) {
			HistoryGraph.getInstance().showHistoryGraph(
					ProjectManager.getProjectManager().getHistory());
		} else if (e.getActionCommand().equals(
				"Menu:FunctionProcedureCallGraph")) {
			FunctionProcedureCallGraph.getInstance()
					.showCallGraph(false, false);
		} else if (e.getActionCommand().equals(
				"Menu:FunctionProcedureCallGraphWholeProject")) {
			FunctionProcedureCallGraph.getInstance().showCallGraph(true, true);
		} else if (e.getActionCommand().equals("Menu:OpenWSLFile")) {
			File file;
			// Choose a file
			JFileChooser chooser;
			if (CM.getAsString("StartDirectory").equals("_Home_")) {
				chooser = new JFileChooser(new File(System
						.getProperty("user.home")));
			} else if (CM.getAsString("StartDirectory").equals("_CurrentDir_")) {
				chooser = new JFileChooser(new File(System
						.getProperty("user.dir")));
			} else {
				chooser = new JFileChooser(new File(CM
						.getAsString("StartDirectory")));
			}
			chooser.setLocale(Locale.UK);
			chooser.setFileFilter(new WSLFileFilter());
			chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			if (chooser.showOpenDialog(MainFrame.getMainFrame()) == JFileChooser.APPROVE_OPTION) {
				file = chooser.getSelectedFile();
				Console.getInstance().loadFile(file);
				EditorGUI eg = ((EditorGUI) ComponentRegistry
						.getGUIComponent("EditorGUI"));
				eg.removeMarks();
			}
		} else if (e.getActionCommand().equals("Menu:SaveWSLFile")) {
			disable = true;
			Console.getInstance().saveFileChooser();
			MainFrame.getMainFrame().getProjectToolBar().update();
			disable = false;
		} else if (e.getActionCommand().equals("Menu:ExportWSLFile")) {
			disable = true;
			Console.getInstance().exportWSLFile();
			MainFrame.getMainFrame().getProjectToolBar().update();
			disable = false;
		} else if (e.getActionCommand().equals("Menu:QuickSaveWSLFile")) {
			disable = true;
			Console.getInstance().quickSaveFile();
			MainFrame.getMainFrame().getProjectToolBar().update();
			disable = false;
		} else if (e.getActionCommand().equals("Menu:SaveFinalWSLFile")) {
			disable = true;
			Console.getInstance().finalSaveFile();
			MainFrame.getMainFrame().getProjectToolBar().update();
			disable = false;
		} else if (e.getActionCommand().equals("Menu:Run")) {
			Console.getInstance().runFile();
		} else if (e.getActionCommand().equals("Menu:Metrics")) {
			Console.getInstance().metricsFile();
		} else if (e.getActionCommand().equals("Menu:Exit")) {
			ProjectManager.getProjectManager().closeProjectFile();
			System.exit(0);
		} else if (e.getActionCommand().equals("Menu:Undo")) {
			UndoManager.getInstance().undoLastEvent();
		} else if (e.getActionCommand().equals("Menu:Redo")) {
			UndoManager.getInstance().redoLastEvent();
		} else if (e.getActionCommand().equals("Menu:Info")) {
			JOptionPane
					.showMessageDialog(
							MainFrame.getMainFrame(),
							"\nFermaT Maintenance Environment\n\nVersion:"
									+ CM.get("version")
									+ "\n\nFor questions and suggestions please contact:\n\n"
									+ "Matthias Ladkau (matthias@ladkau.de)\n\n",
							"Info", JOptionPane.INFORMATION_MESSAGE,
							new ImageIcon("icons/fme_logo.gif"));
		} else if (e.getActionCommand().equals("Menu:ShowAllWindows")) {
			JDesktopPane desktop = MainFrame.getMainFrame().getDesktopPane();
			for (int i = 0; i < desktop.getAllFrames().length; i++) {
				try {
					desktop.getAllFrames()[i].setMaximum(true);
				} catch (Exception ex) {
				}
			}
		} else if (e.getActionCommand().equals("Menu:WindowsDefault")) {
			JDesktopPane desktop = MainFrame.getMainFrame().getDesktopPane();
			int h = desktop.getHeight(), w = desktop.getWidth(), x = 0, y = 0;
			for (int i = 0; i < desktop.getAllFrames().length; i++) {

				if (desktop.getAllFrames()[i].getTitle().equals(
						"Abstract Syntax Tree")) {
					desktop.getAllFrames()[i].setSize(w / 4, h);
					desktop.getAllFrames()[i].setLocation(x, y);
				}
				if (desktop.getAllFrames()[i].getTitle().equals("WSL Editor")) {
					desktop.getAllFrames()[i].setSize(w / 2, h * 3 / 4);
					desktop.getAllFrames()[i].setLocation(x + w / 4, y);
				}
				if (desktop.getAllFrames()[i].getTitle().equals("Console")) {
					desktop.getAllFrames()[i].setSize(w * 3 / 4, h / 4);
					desktop.getAllFrames()[i].setLocation(x + w / 4, y + h * 3
							/ 4);
				}
				if (desktop.getAllFrames()[i].getTitle().equals(
						"Transformation Catalogue")) {
					desktop.getAllFrames()[i].setSize(w / 4, h * 3 / 4);
					desktop.getAllFrames()[i].setLocation(x + w * 3 / 4, y);
				}
			}
		} else if (e.getActionCommand().equals("Menu:WindowsAbreast")) {
			JDesktopPane desktop = MainFrame.getMainFrame().getDesktopPane();
			int h = desktop.getHeight(), w = desktop.getWidth(), x = 0, y = 0;
			w = w / desktop.getAllFrames().length;
			for (int i = 0; i < desktop.getAllFrames().length; i++) {
				desktop.getAllFrames()[i].setSize(w, h);
				desktop.getAllFrames()[i].setLocation(x, y);
				x += w;
			}
		} else if (e.getActionCommand().equals("Menu:WindowsBox")) {
			JDesktopPane desktop = MainFrame.getMainFrame().getDesktopPane();
			int h = desktop.getHeight(), w = desktop.getWidth(), x = 0, y = 0;
			h = h / 2;
			w = w / (desktop.getAllFrames().length / 2);
			for (int i = 0; i < desktop.getAllFrames().length; i++) {
				if (y == 0 && i >= (desktop.getAllFrames().length / 2)) {
					x = 0;
					y += h;
				}
				desktop.getAllFrames()[i].setSize(w, h);
				desktop.getAllFrames()[i].setLocation(x, y);
				x += w;
			}
		} else if (e.getActionCommand().equals("Menu:ActionSystemCallGraph")) {
			if (!ActionSystemCallGraph.getInstance().showCallGraph()) {
				JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
						"Can't find an ActionSystem", "Message",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * Get the status of the listener
	 * 
	 * @return True if the listener is enabled
	 */
	public boolean isDisable() {
		return disable;
	}

	/**
	 * Disable the listener
	 * 
	 * @param disable
	 *            Set true if the listener should be disabled
	 */
	public void setDisable(boolean disable) {
		this.disable = disable;
	}

	// Internal Classes
	// ================

	/**
	 * File Filter Class for WSL Files
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

	/**
	 * File Filter Class for FPF Files
	 */
	private class FPFFileFilter extends javax.swing.filechooser.FileFilter {

		public boolean accept(File f) {
			if (f.getName().endsWith(".fpf"))
				return true;
			else if (f.isDirectory())
				return true;
			else
				return false;
		}

		public String getDescription() {
			return "FermaT Project Files";
		}
	}
}
