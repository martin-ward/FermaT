/**
 * Project: fme
 */

package fme.components.historygraph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JOptionPane;

import sve.SVEMain;
import sve.engine.ExportManager;
import sve.engine.PrintingManager;
import sve.layout.graph.CircleGraphLayout;
import sve.layout.graph.FlowLayout;
import sve.layout.graph.GridLayout;
import sve.layout.graph.SmartLayout;
import sve.layout.graph.TreeLayout;
import sve.structures.abstraction.AbstractEdge;
import sve.structures.abstraction.AbstractNode;
import fme.components.ComponentRegistry;
import fme.components.ProjectManager;
import fme.components.console.Console;
import fme.components.tree.TreeGUI;

public class HistoryGraphEventHandler implements ActionListener {

	private SVEMain main;

	public HistoryGraphEventHandler(SVEMain main) {
		this.main = main;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		if (e.getActionCommand().equals("DoubleClickOnElement")) {
			String event, num;
			if (e.getSource() instanceof AbstractNode) {
				event = e.getActionCommand() + " ->"
						+ ((AbstractNode) e.getSource()).getName();
				File file = new File(ProjectManager
						.relativeToAbsolute(((AbstractNode) e.getSource())
								.getName()));
				if (file.exists())
					Console.getInstance().loadFile(file);
			} else if (e.getSource() instanceof AbstractEdge) {
				event = e.getActionCommand() + " ->"
						+ ((AbstractEdge) e.getSource()).getName();
				File file = new File(ProjectManager
						.relativeToAbsolute(((AbstractEdge) e.getSource())
								.getEnd1().getName()));
				if (file.exists()) {
					Console.getInstance().loadFile(file);
					try {
						num = event.split("#")[event.split("#").length - 1];
						((TreeGUI) ComponentRegistry.getComponent("TreeGUI"))
								.expandTreeToRow(Integer.parseInt(num));
					} catch (NumberFormatException ex) {
						// No node to be highlighted
					}
				}
			}
		} else if (e.getActionCommand().equals("Menu:ExportScreenshot")) {
			ExportManager.exportScreenshotToPicture(main.getMainDisplay());
		} else if (e.getActionCommand().equals("Menu:ExportDiagram")) {
			ExportManager.exportDiagramToPicture(main.getMainDisplay());
		} else if (e.getActionCommand().equals("Menu:Print")) {
			PrintingManager.printDiagramWithSetup(main.getMainDisplay());
		} else if (e.getActionCommand().equals("Menu:LayoutSmart")) {
			try {
				main.layoutGraph(new SmartLayout());
			} catch (NumberFormatException ex) {
				JOptionPane.showMessageDialog(null,
						"Can't execute layout algorithm on present graph.",
						"Error", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals("Menu:LayoutTree")) {
			try {
				main.layoutGraph(new TreeLayout());
			} catch (Exception ex) {
				JOptionPane.showMessageDialog(null,
						"Can't execute layout algorithm on present graph.",
						"Error", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals("Menu:LayoutCircle")) {
			try {
				main.layoutGraph(new CircleGraphLayout());
			} catch (Exception ex) {
				JOptionPane.showMessageDialog(null,
						"Can't execute layout algorithm on present graph.",
						"Error", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals("Menu:LayoutGrid")) {
			try {
				main.layoutGraph(new GridLayout());
			} catch (Exception ex) {
				JOptionPane.showMessageDialog(null,
						"Can't execute layout algorithm on present graph.",
						"Error", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals("Menu:LayoutFlow")) {
			try {
				main.layoutGraph(new FlowLayout());
			} catch (Exception ex) {
				JOptionPane.showMessageDialog(null,
						"Can't execute layout algorithm on present graph.",
						"Error", JOptionPane.ERROR_MESSAGE);
			}
		}

	}
}
