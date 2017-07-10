/**
 * Project: fme
 */

package fme.components.actionsystemcallgraph;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import net.claribole.zvtm.engine.Location;
import sve.ConfigManager;
import sve.SVEMain;
import sve.gui.SimpleFrame;
import sve.layout.graph.SmartLayout;
import sve.layout.graph.TreeLayout;
import sve.structures.abstraction.AbstractNode;
import sve.structures.edges.BendableDirectedEdge;
import sve.structures.graphs.DirectedGraph;
import sve.structures.nodes.CollapsableTextNode;
import sve.structures.nodes.TextNode;
import fme.components.ComponentRegistry;
import fme.components.tree.TreeGUI;
import fme.config.CM;
import fme.wsl.ast.AST;
import fme.wsl.ast.ASTNode;
import fme.wsl.ast.ASTNodePreOrderTreeIterator;

/**
 * This class is to show a call graph for an WSL Action System
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class ActionSystemCallGraph {

	/**
	 * The only instance of this class
	 */
	private static ActionSystemCallGraph singleton = new ActionSystemCallGraph();

	private SVEMain main;

	private Location oldCameraLocation;

	private Dimension oldDimension;

	private Point oldLocation;

	private static HashMap<String, ASTNode> actionASTNodes;

	private static HashMap<String, AbstractNode> actionNodes;

	/**
	 * Return the only instance of this class
	 * 
	 * @return The ActionSystemCallGraph
	 */
	public static ActionSystemCallGraph getInstance() {
		return singleton;
	}

	/**
	 * Tries to construct the call graph from the current WSL file
	 * 
	 * @return True if successful / False if no Actionsystem could be found
	 */
	public boolean showCallGraph() {
		boolean ret = false;
		String action, target;
		ASTNode node;
		CollapsableTextNode tn;
		HashMap<String, CollapsableTextNode> actions = new HashMap<String, CollapsableTextNode>();
		HashMap<String, Integer> actionsIN = new HashMap<String, Integer>();
		HashMap<String, Integer> actionsOUT = new HashMap<String, Integer>();
		HashMap<String, HashSet<String>> actionsVAR = new HashMap<String, HashSet<String>>();
		Vector<String> m, e;

		main = new SVEMain();

		ActionSystemCallGraphEventHandler listener = new ActionSystemCallGraphEventHandler(
				main);
		actionASTNodes = new HashMap<String, ASTNode>();
		actionNodes = new HashMap<String, AbstractNode>();

		if (AST.getAST() == null)
			return true;

		// Set the configuration
		ConfigManager.getConfig().put("graph.ConnectionLine.Color",
				CM.getAsColor("gui.ActionSystemCallGraph.LineColor"));
		ConfigManager.getConfig().put("graph.ConnectionLine.MarkedColor",
				CM.getAsColor("gui.ActionSystemCallGraph.MarkedLineColor"));
		ConfigManager.getConfig().put("graph.Textnode.BackgroundColor",
				CM.getAsColor("gui.ActionSystemCallGraph.NodeColor"));

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

		ASTNodePreOrderTreeIterator ti = new ASTNodePreOrderTreeIterator(AST
				.getAST());

		// The node to start an action system
		tn = addTextNode(directedGraph, "(Start)", TextNode.RectangularShape);
		actions.put("(Start)", tn);

		// The node to terminate an action system
		tn = addTextNode(directedGraph, "Z", TextNode.RectangularShape);
		actions.put("Z", tn);
		actionsIN.put("Z", new Integer(0));
		actionsOUT.put("Z", new Integer(0));
		actionsVAR.put("Z", new HashSet<String>());

		action = null;
		while (ti.hasNext()) {
			node = ti.next();

			// Create Node
			if (node.getSpecificType().equals("T_Action")) {
				action = node.getChildren().get(0).getValue();
				tn = addTextNode(directedGraph, action,
						TextNode.RectangularShape);
				actions.put(action, tn);
				actionsIN.put(action, new Integer(0));
				actionsOUT.put(action, new Integer(0));
				actionsVAR.put(action, new HashSet<String>());
				actionASTNodes.put(action, node);
				actionNodes.put(action, tn);
			}
			if (node.getSpecificType().equals("T_Var_Lvalue")
					|| node.getSpecificType().equals("T_Variable")) {
				if (action != null) {
					actionsVAR.get(action).add(node.getValue());
				}
			}
		}

		ti.renew();

		action = null;
		while (ti.hasNext()) {
			node = ti.next();

			// Get the source node
			if (node.getSpecificType().equals("T_Action")) {
				action = node.getChildren().get(0).getValue();
			}

			// Create edge
			if (node.getSpecificType().equals("T_Call") && action != null) {
				target = node.getValue();
				addDirectedEdge(directedGraph, "", actions.get(action), actions
						.get(target));
				actionsIN.put(target, new Integer(actionsIN.get(target)
						.intValue() + 1));
				actionsOUT.put(action, new Integer(actionsOUT.get(action)
						.intValue() + 1));
			}

			// Create first edge
			if (node.getSpecificType().equals("T_A_S")) {
				action = node.getChildren().get(0).getValue();
				addDirectedEdge(directedGraph, "", actions.get("(Start)"),
						actions.get(action));
				ret = true;
			}
		}

		Iterator<String> it = actionsIN.keySet().iterator();
		while (it.hasNext()) {
			action = it.next();
			// System.out.println(action+" IN:"+actionsIN.get(action)+" OUT:"+actionsOUT.get(action)+" VAR:"+actionsVAR.get(action));
			tn = actions.get(action);
			// tn.setCaption(tn.getCaption()[0]+"\nIN:"+actionsIN.get(action)+"\nOUT:"+actionsOUT.get(action));
		}

		if (ret) {

			// Layout the graph
			try {
				main.layoutGraph(new SmartLayout());
			} catch (Exception ex) {
				main.layoutGraph(new TreeLayout());
			}

			// Display the graph
			SimpleFrame mainFrame = new SimpleFrame(main.getMainDisplay(), true);

			mainFrame.addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent e) {
					oldCameraLocation = main.getMainDisplay().getCamera()
							.getLocation();
					oldDimension = ((SimpleFrame) e.getSource()).getSize();
					oldLocation = ((SimpleFrame) e.getSource()).getLocation();
				}
			});

			// Set the MenuBar
			mainFrame.setJMenuBar(new ActionSystemCallGraphMenuBar(listener));
			main.getEventPump().setForwarder(listener);

			if (oldDimension != null && oldLocation != null) {
				mainFrame.setLocation(oldLocation);
				mainFrame.setSize(oldDimension);
			}
			
			// Set camera to previous position
			if (oldCameraLocation != null) {
				main.getMainDisplay().getCamera().posx = oldCameraLocation.vx;
				main.getMainDisplay().getCamera().posy = oldCameraLocation.vy;
				main.getMainDisplay().getCamera().altitude = oldCameraLocation.alt;
			}

			// Graphic display start
			mainFrame.setVisible(true);
			mainFrame.repaint();
		}

		return ret;
	}

	/**
	 * Get the cooresponding ASTNode to an Action name
	 * 
	 * @param name
	 *            The Action name
	 * @return The corresponding ASTNode
	 */
	public ASTNode getActionASTNode(String name) {
		return actionASTNodes.get(name);
	}

	/**
	 * Get the cooresponding GraphNode to an Action name
	 * 
	 * @param name
	 *            The Action name
	 * @return The corresponding ASTNode
	 */
	public AbstractNode getActionNode(String name) {
		return actionNodes.get(name);
	}

	/**
	 * Try to show an item from a selected element in the tree
	 */
	public void showItemFromTree() {

		String name = "";
		try {
			if (actionNodes != null) {
				ASTNode node = ((TreeGUI) ComponentRegistry
						.getGUIComponent("TreeGUI")).getSelectedNode();

				if (!main.getMainDisplay().getJPanel().isVisible())
					return;

				if (node.getSpecificType().equals("T_Action")) {
					name = node.getChildAt(0).getValue();
				} else if (node.getSpecificType().equals("T_Name")
						&& node.getParent() != null
						&& node.getParent().getSpecificType()
								.equals("T_Action")) {
					name = node.getValue();
				}

				if (actionNodes.get(name) != null) {
					main.getMainDisplay().getVirtualSpaceManager()
							.centerOnGlyph(
									actionNodes.get(name)
											.getGlyphRepresentation(),
									main.getMainDisplay().getCamera(), 500);
				}
			}
		} catch (NullPointerException e) {

		}
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
	private CollapsableTextNode addTextNode(DirectedGraph g, String name,
			int shape) {
		CollapsableTextNode tn = new CollapsableTextNode(g, shape);
		tn.setName(name);
		tn.setCaption(name);
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
	private BendableDirectedEdge addDirectedEdge(DirectedGraph g, String name,
			AbstractNode n1, AbstractNode n2) {
		BendableDirectedEdge de = new BendableDirectedEdge(g, n1, n2);
		de.setName(name);
		de.setCaption(name);
		g.addEdge(de);
		return de;
	}
}
