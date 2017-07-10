/**
 * Project: fme
 */

package fme.components.actionsystemcallgraph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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
import fme.components.tree.TreeGUI;
import fme.wsl.ast.ASTNode;

public class ActionSystemCallGraphEventHandler implements ActionListener {

	private SVEMain main;

	public ActionSystemCallGraphEventHandler(SVEMain main) {
		this.main = main;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		ASTNode node;
		if (e.getActionCommand().equals("Menu:ExportScreenshot")) {
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
		} else if (e.getActionCommand().equals("DoubleClickOnElement")) {
			if (e.getSource() instanceof AbstractNode) {
				String name = ((AbstractNode) e.getSource()).getName();
				node = ActionSystemCallGraph.getInstance().getActionASTNode(
						name);
				if (node != null) {
					((TreeGUI) ComponentRegistry.getGUIComponent("TreeGUI"))
							.expandTreeToRow(node.getRow());
				}
			}
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
	}
}
