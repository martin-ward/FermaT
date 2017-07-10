/**
 * Project: fme
 */

package fme.components.functionprocedurecallgraph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

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
import fme.components.console.Console;
import fme.components.tree.TreeGUI;

public class FunctionProcedureCallGraphEventHandler implements ActionListener {

	private SVEMain main;

	public FunctionProcedureCallGraphEventHandler(SVEMain main) {
		this.main = main;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		String filepath;
		AbstractNode node;
		AbstractEdge edge;
		int row;

		FunctionProcedureCallGraph.getInstance().setItemSelection(false);

		try {
			if (e.getActionCommand().equals("DoubleClickOnElement")) {
				if (e.getSource() instanceof AbstractNode) {
					node = (AbstractNode) e.getSource();
					filepath = node.getName().split("#")[0];
					if (filepath.equals("extern"))
						return;
					if (!Console.getInstance().getWorkingFile()
							.getAbsolutePath().equals(filepath)) {
						Console.getInstance().loadFile(new File(filepath));
					}
					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.INFO,
							"Displaying:" + filepath + " Node:"
									+ node.getName().split("#")[1]);
					((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"))
							.expandTreeToRow(Integer.parseInt(node.getName()
									.split("#")[1]));
				} else if (e.getSource() instanceof AbstractEdge) {
					edge = (AbstractEdge) e.getSource();
					filepath = edge.getName().split("#")[0];
					if (!Console.getInstance().getWorkingFile()
							.getAbsolutePath().equals(filepath)) {
						Console.getInstance().loadFile(new File(filepath));
					}

					row = Integer.parseInt(edge.getName().split("#")[1]) + 1;

					Logger.getLogger(this.getClass().getCanonicalName()).log(
							Level.INFO,
							"Displaying:" + filepath + " Node:" + row);
					((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"))
							.expandTreeToRow(row);
				}
			} else if (e.getActionCommand().equals("Menu:ExportScreenshot")) {
				ExportManager.exportScreenshotToPicture(main.getMainDisplay());
			} else if (e.getActionCommand().equals("Menu:ExportDiagram")) {
				ExportManager.exportDiagramToPicture(main.getMainDisplay());
			} else if (e.getActionCommand().equals("Menu:Print")) {
				PrintingManager.printDiagramWithSetup(main.getMainDisplay());
			} else if (e.getActionCommand().equals("GoSource")) {
				main.getMainDisplay().getVirtualSpaceManager().centerOnGlyph(
						((AbstractEdge) e.getSource()).getEnd1()
								.getGlyphRepresentation(),
						main.getMainDisplay().getCamera(), 500);
			} else if (e.getActionCommand().equals("GoTarget")) {
				main.getMainDisplay().getVirtualSpaceManager().centerOnGlyph(
						((AbstractEdge) e.getSource()).getEnd2()
								.getGlyphRepresentation(),
						main.getMainDisplay().getCamera(), 500);
			} else if (e.getActionCommand().equals("Menu:LayoutSmart")) {
				try {
					main.layoutGraph(new SmartLayout());
				} catch (Exception ex) {
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

		} catch (Exception ex) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.WARNING, "Error in selecting corresponding node");
		}

		FunctionProcedureCallGraph.getInstance().setItemSelection(true);
	}
}
