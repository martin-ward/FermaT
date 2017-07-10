/**
 * Project: fme
 */

package fme.components.historygraph;

import java.awt.Color;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import sve.ConfigManager;
import sve.SVEMain;
import sve.gui.SimpleFrame;
import sve.layout.graph.SmartLayout;
import sve.structures.abstraction.AbstractNode;
import sve.structures.edges.BendableDirectedEdge;
import sve.structures.graphs.DirectedGraph;
import sve.structures.nodes.CollapsableTextNode;
import sve.structures.nodes.TextNode;
import fme.config.CM;
import fme.wsl.ast.AST;

/**
 * This class is to show a call graph for an WSL Action System
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class HistoryGraph {

	private SVEMain main;
	
	/**
	 * The only instance of this class
	 */
	private static HistoryGraph singleton = new HistoryGraph();

	/**
	 * Return the only instance of this class
	 * 
	 * @return The ActionSystemCallGraph
	 */
	public static HistoryGraph getInstance() {
		return singleton;
	}

	/**
	 * Tries to construct the call graph from the current WSL file
	 * 
	 * @return True if successful / False if no Actionsystem could be found
	 */
	public boolean showHistoryGraph(HashMap<String, Vector<String>> history) {

		boolean ret = true;
		String file, file2, event;
		CollapsableTextNode tn;
		Iterator<String> i, i2;
		HashMap<String, CollapsableTextNode> files = new HashMap<String, CollapsableTextNode>();
		
		if (AST.getAST() == null)
			return true;

		singleton = new HistoryGraph();
		main = new SVEMain();

		// Set the configuration
		ConfigManager.getConfig().put("graph.ConnectionLine.Color",
				CM.getAsColor("gui.HistoryGraph.LineColor"));
		ConfigManager.getConfig().put("graph.ConnectionLine.MarkedColor",
				CM.getAsColor("gui.HistoryGraph.MarkedLineColor"));
		ConfigManager.getConfig().put("graph.Textnode.BackgroundColor",
				CM.getAsColor("gui.HistoryGraph.NodeColor"));
		// Create an abstract graph
		DirectedGraph directedGraph = new DirectedGraph();
		// Initialise the graph
		main.initGraph(directedGraph);

		i = history.keySet().iterator();

		// Build Nodes
		while (i.hasNext()) {
			file = i.next();

			// Create Node
			if (file != null && !file.equals("null")) {
				tn = addTextNode(directedGraph, file, TextNode.OvalShape);
				files.put(file, tn);

				// Check if all destinations are there and if not create them
				i2 = history.get(file).iterator();
				while (i2.hasNext()) {
					file2 = i2.next();
					if (history.get(file2.split("#")[1]) == null) {
						tn = addTextNode(directedGraph, file2.split("#")[1],
								TextNode.OvalShape);
						files.put(file2.split("#")[1], tn);
					}
				}
			}
		}

		i = history.keySet().iterator();

		// Build Edges
		while (i.hasNext()) {
			file = i.next();

			if (file != null && history.get(file) != null) {
				i2 = history.get(file).iterator();
				while (i2.hasNext()) {
					file2 = i2.next();
					if (!file2.equals("null") && !file2.equals("")) {
						event = file2;
						file2 = file2.split("#")[1];
						if (files.get(file) != null && files.get(file2) != null)
							if (event.startsWith("Transformation")) {
								addDirectedEdge(directedGraph,
										event.split("#")[2].replace("/", "")
												.substring(3), event, files
												.get(file), files.get(file2),
										Color.RED);
							} else {
								addDirectedEdge(directedGraph, "", event, files
										.get(file), files.get(file2),
										Color.BLACK);
							}
					}
				}
			}
		}

		if (ret) {

			// Layout the graph
			main.layoutGraph(new SmartLayout());

			// Display the graph
			SimpleFrame mainFrame = new SimpleFrame(main
					.getMainDisplay(), true);

			// Set the MenuBar
			ActionListener listener = new HistoryGraphEventHandler(main);
			mainFrame.setJMenuBar(new HistoryGraphMenuBar(listener));
			main.getEventPump().setForwarder(listener);

			// Graphic display start
			mainFrame.setVisible(true);
			mainFrame.repaint();

			main.getMainDisplay().setZoomingMode(true);
		}

		return ret;
	}

	// Internal Methods
	// ================

	/**
	 * Add a TextNode to a DirectedGraph
	 * 
	 * @param g
	 *            The DirectedGraph
	 * @param name
	 *            The name of the TextNode
	 * @param shape
	 *            The shape of the node (@see sve.structures.nodes.TextNode)
	 * @return The added TextNode
	 */
	private static CollapsableTextNode addTextNode(DirectedGraph g,
			String name, int shape) {
		CollapsableTextNode tn = new CollapsableTextNode(g, shape);
		tn.setName(name);
		tn.setCaption(new File(name).getName());
		g.addNode(tn);
		return tn;
	}

	/**
	 * Add a DirectedEdge to a DirectedGraph
	 * 
	 * @param g
	 *            The DirectedGraph
	 * @param name
	 *            The name of the DirectedEdge
	 * @param n1
	 *            The source of the edge
	 * @param n2
	 *            The destination of the edge
	 * @return The added DirectedEdge
	 */
	private static BendableDirectedEdge addDirectedEdge(DirectedGraph g,
			String cap, String name, AbstractNode n1, AbstractNode n2, Color col) {
		BendableDirectedEdge de = new BendableDirectedEdge(g, n1, n2);
		de.setName(name);
		de.setCaption(cap);
		de.getLine().setLineColor(col);
		g.addEdge(de);
		return de;
	}
}
