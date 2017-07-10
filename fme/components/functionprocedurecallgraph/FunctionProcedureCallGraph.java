/**
 * Project: fme
 */

package fme.components.functionprocedurecallgraph;

import java.awt.Color;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import sve.ConfigManager;
import sve.SVEMain;
import sve.gui.SimpleFrame;
import sve.layout.graph.SmartLayout;
import sve.layout.graph.TreeLayout;
import sve.structures.abstraction.AbstractEdge;
import sve.structures.abstraction.AbstractNode;
import sve.structures.edges.BendableDirectedEdge;
import sve.structures.edges.DirectedEdge;
import sve.structures.graphs.DirectedGraph;
import sve.structures.nodes.CollapsableTextNode;
import sve.structures.nodes.TextNode;
import fme.components.ComponentRegistry;
import fme.components.tree.TreeGUI;
import fme.config.CM;
import fme.wsl.ast.AST;
import fme.wsl.ast.ASTNode;

/**
 * This class is to show a call graph for an WSL Action System
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class FunctionProcedureCallGraph {

	private SVEMain main;

	/**
	 * The only instance of this class
	 */
	private static FunctionProcedureCallGraph singleton = new FunctionProcedureCallGraph();

	/**
	 * The Frame which displays the graph
	 */
	private SimpleFrame mainFrame;

	/**
	 * The GraphNodes
	 */
	private static HashMap<String, CollapsableTextNode> ca;

	/**
	 * Toggles if selected items in the tree should be shown in the graph
	 */
	private boolean itemSelection = true;

	private static boolean callCountLabel = true;

	private static HashMap<AbstractNode, Vector<BendableDirectedEdge>> connections = new HashMap<AbstractNode, Vector<BendableDirectedEdge>>();

	/**
	 * Return the only instance of this class
	 * 
	 * @return The ActionSystemCallGraph
	 */
	public static FunctionProcedureCallGraph getInstance() {
		return singleton;
	}

	/**
	 * Tries to construct the call graph from the current WSL file
	 * 
	 * @return True if successful / False if no Actionsystem could be found
	 */
	public boolean showCallGraph(boolean allProjectFiles,boolean showExternalCalls) {

		boolean ret = true;
		Vector<WSLCodeArea> areas;
		WSLCodeArea area;
		String call, name, location;
		CollapsableTextNode tn, tn2;
		Iterator<WSLCodeArea> i;
		Iterator<String> i2;
		Vector<String> m, e;
		int in, out;

		main = new SVEMain();
		ca = new HashMap<String, CollapsableTextNode>();

		if (AST.getAST() == null)
			return true;

		Logger
				.getLogger(FunctionProcedureCallGraph.class.getCanonicalName())
				.log(Level.INFO, "Displaying Function/Procedure Call Graph... ");

		areas = FunctionProcedureCallGraphParser.parse(allProjectFiles,showExternalCalls);

		if (areas == null) {
			Logger.getLogger(
					FunctionProcedureCallGraph.class.getCanonicalName()).log(
					Level.SEVERE, "Error occured during parsing process...");
			return false;
		}

		// Set the configuration
		ConfigManager.getConfig().put("graph.ConnectionLine.Color",
				CM.getAsColor("gui.FunctionProcedureCallGraph.LineColor"));
		ConfigManager
				.getConfig()
				.put(
						"graph.ConnectionLine.MarkedColor",
						CM
								.getAsColor("gui.FunctionProcedureCallGraph.MarkedLineColor"));
		ConfigManager
				.getConfig()
				.put(
						"graph.Textnode.BackgroundColor",
						CM
								.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.MODULE"));

		// Set additional menu entries
		m = new Vector<String>();
		e = new Vector<String>();
		m.add("Go to Source");
		e.add("GoSource");
		m.add("Go to Target");
		e.add("GoTarget");
		main.edgeMenuExtention(m, e);

		// Create an abstract graph
		DirectedGraph directedGraph = new DirectedGraph();
		// Initialise the graph
		main.initGraph(directedGraph);

		i = areas.iterator(); // Build Nodes
		while (i.hasNext()) {
			area = i.next(); // Create Node
			name = area.getName();
			if (name.contains(":")) {
				name = name + "()";
			}
			location = area.getFile().getAbsolutePath() + "#"
					+ area.getNode().getRow();
			tn = addTextNode(directedGraph, name, location, TextNode.OvalShape);
			tn.setBackgroundColor(getNodeColor(area.getType()));
			if (area.getType() == WSLCodeArea.MetaWSL_BFUNCTION
					|| area.getType() == WSLCodeArea.MetaWSL_FUNCTION
					|| area.getType() == WSLCodeArea.MetaWSL_PROCEDURE
					|| area.getType() == WSLCodeArea.MODULE) {
				ca.put(area.getLocalName(), tn);
			} else {
				ca.put(area.getName(), tn);
			}
			area.setGnode(tn);
		}

		i = areas.iterator(); // Build Edges
		while (i.hasNext()) {
			area = i.next();
			tn = ca.get(area.getLocalName());
			if (tn != null) {
				i2 = area.getCalls().iterator();
				while (i2.hasNext()) {
					call = i2.next();
					if (ca.get(call) == null) {
						tn2 = addTextNode(directedGraph, call, "extern",
								TextNode.OvalShape);
						ca.put(call, tn2);
						tn2.setBackgroundColor(getNodeColor(area.getCallTypes()
								.get(area.getCalls().indexOf(call))));

					} else
						tn2 = ca.get(call);

					location = area.getFile().getAbsolutePath()
							+ "#"
							+ area.getCallASTNodes().get(
									area.getCalls().indexOf(call)).getRow();
					addDirectedEdge(directedGraph, "", location, tn, tn2,
							Color.BLACK);
				}
			}
		}

		i = areas.iterator(); // Build Nodes
		while (i.hasNext()) {
			area = i.next();
			tn = (CollapsableTextNode) area.getGnode();
			if (callCountLabel) {
				in = 0;
				out = 0;
				Iterator<AbstractEdge> it = tn.getEdgesIncoming().iterator();
				while (it.hasNext()) {
					try {
						in += Integer.parseInt(((DirectedEdge) it.next())
								.getCaption());
					} catch (Exception ex) {

					}
				}
				it = tn.getEdgesOutgoing().iterator();
				while (it.hasNext()) {
					try {
						out += Integer.parseInt(((DirectedEdge) it.next())
								.getCaption());
					} catch (Exception ex) {

					}
				}

			} else {
				in = tn.getEdgesIncoming().size();
				out = tn.getEdgesOutgoing().size();
			}

			tn.setCaption(tn.getCaption()[0] + "\nIN:" + in + " OUT:" + out);
		}

		if (ret) {

			// Layout the graph
			try {
				main.layoutGraph(new SmartLayout());
			} catch (Exception ex) {
				main.layoutGraph(new TreeLayout());
			}

			// Display the graph
			mainFrame = new SimpleFrame(main.getMainDisplay(), true);

			// Set the MenuBar
			ActionListener listener = new FunctionProcedureCallGraphEventHandler(
					main);
			mainFrame.setJMenuBar(new FunctionProcedureCallGraphMenuBar(
					listener));
			main.getEventPump().setForwarder(listener);

			// Graphic display start
			mainFrame.setVisible(true);
			mainFrame.repaint();
		}

		return ret;
	}

	/**
	 * Try to show an item from a selected element in the tree
	 */
	public void showItemFromTree() {
		try {
			ASTNode node = ((TreeGUI) ComponentRegistry
					.getGUIComponent("TreeGUI")).getSelectedNode();

			if (!itemSelection || !mainFrame.isVisible())
				return;

			if (node.getSpecificType().contains("Func")
					|| node.getSpecificType().contains("Proc")) {
				node = node.getChildAt(0);
			}
			if (node.getSpecificType().equals("T_Name")) {
				if (ca.get(node.getValue()) != null) {
					main.getMainDisplay().getVirtualSpaceManager()
							.centerOnGlyph(
									ca.get(node.getValue())
											.getGlyphRepresentation(),
									main.getMainDisplay().getCamera(), 500);
				}
			}
		} catch (NullPointerException e) {

		}
	}

	/**
	 * Set if selected tree items should be shown in the graph
	 * 
	 * @param itemSelection
	 *            True: The selected tree items should be shown / False: Ignore
	 *            selections
	 */
	public void setItemSelection(boolean itemSelection) {
		this.itemSelection = itemSelection;
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
	private static CollapsableTextNode addTextNode(DirectedGraph g, String cap,
			String name, int shape) {
		CollapsableTextNode tn = new CollapsableTextNode(g, shape);
		tn.setName(name);
		tn.setCaption(cap);
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
		BendableDirectedEdge de = null, t;
		int num;
		if (!callCountLabel) {
			de = new BendableDirectedEdge(g, n1, n2);
			de.setName(name);
			de.setCaption(cap);
			de.getLine().setLineColor(col);
			g.addEdge(de);
		} else {

			if (connections.get(n1) != null) {
				Iterator<BendableDirectedEdge> it = connections.get(n1)
						.iterator();
				while (it.hasNext()) {
					t = it.next();
					if (t.getEnd2().equals(n2)) {
						de = t;
						num = Integer.parseInt(de.getCaption());
						de.setCaption("" + (num + 1));
					}
				}
			} else {
				connections.put(n1, new Vector<BendableDirectedEdge>());
			}
			if (de == null) {
				de = new BendableDirectedEdge(g, n1, n2);
				de.setName(name);
				de.setCaption("1");
				de.getLine().setLineColor(col);
				g.addEdge(de);
				connections.get(n1).add(de);
			}
		}
		return de;
	}

	private Color getNodeColor(int type) {
		Color nc;
		if (type == WSLCodeArea.PROCEDURE)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.PROCEDURE");
		else if (type == WSLCodeArea.FUNCTION)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.FUNCTION");
		else if (type == WSLCodeArea.BFUNCTION)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.BFUNCTION");
		else if (type == WSLCodeArea.MetaWSL_PROCEDURE)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.MetaWSL_PROCEDURE");
		else if (type == WSLCodeArea.MetaWSL_FUNCTION)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.MetaWSL_FUNCTION");
		else if (type == WSLCodeArea.MetaWSL_BFUNCTION)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.MetaWSL_BFUNCTION");
		else if (type == WSLCodeArea.External_PROCEDURE)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.External_PROCEDURE");
		else if (type == WSLCodeArea.External_FUNCTION)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.External_FUNCTION");
		else if (type == WSLCodeArea.External_BFUNCTION)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.External_BFUNCTION");
		else if (type == WSLCodeArea.External_A_PROCEDURE)
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.External_A_PROCEDURE");
		else
			nc = CM
					.getAsColor("gui.FunctionProcedureCallGraph.NodeColor.MODULE");
		return nc;
	}
}
