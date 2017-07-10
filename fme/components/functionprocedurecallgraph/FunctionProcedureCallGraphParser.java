package fme.components.functionprocedurecallgraph;

import java.io.File;
import java.util.Iterator;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import fme.components.ProjectManager;
import fme.components.console.Console;
import fme.gui.MainFrame;
import fme.wsl.ast.AST;
import fme.wsl.ast.ASTNode;
import fme.wsl.ast.ASTNodePreOrderTreeIterator;

/**
 * This class gathers the information for the Function/Procedure Call Graph
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 * 
 */
public class FunctionProcedureCallGraphParser {

	private static boolean parseExternCalls = true;

	private FunctionProcedureCallGraphParser() {

	}

	private static File file;

	private static String currentModule;

	private static Vector<WSLCodeArea> areas;

	public static Vector<WSLCodeArea> parse(boolean allProjectFiles,boolean showExternalCalls) {

		Iterator<File> i;
		String name;
		WSLCodeArea area;
		ASTNode root;

		parseExternCalls = showExternalCalls;
		
		areas = new Vector<WSLCodeArea>();

		if (ProjectManager.getProjectManager().getFiles() == null) {
			JOptionPane.showMessageDialog(MainFrame.getMainFrame(),
					"Please load a project to analyse", "Message",
					JOptionPane.ERROR_MESSAGE);
			return null;
		}

		if (allProjectFiles) {
			File cf = Console.getInstance().getWorkingFile();
			i = ProjectManager.getProjectManager().getFiles().iterator();
			while (i.hasNext()) {

				file = i.next();

				Logger.getLogger(
						FunctionProcedureCallGraphParser.class
								.getCanonicalName()).log(Level.INFO,
						"File:" + file);

				root = parseFile(file);
				if (root == null)
					return null;

				name = file.getName().toUpperCase().replace(".WSL", "");
				currentModule = name;
				area = new WSLCodeArea(name, WSLCodeArea.MODULE, file, root);
				analyse(area, new ASTNodePreOrderTreeIterator(root));
			}
			Console.getInstance().loadFile(cf);
		} else {
			file = Console.getInstance().getWorkingFile();
			root = parseFile(file);
			if (root == null)
				return null;

			name = file.getName().toUpperCase().replace(".WSL", "");
			area = new WSLCodeArea(name, WSLCodeArea.MODULE, file, root);
			currentModule = name;
			analyse(area, new ASTNodePreOrderTreeIterator(root));
		}

		return areas;
	}

	// Internal Methods
	// ================

	private static void analyse(WSLCodeArea area, ASTNodePreOrderTreeIterator it) {
		ASTNode node;

		Logger.getLogger(FunctionProcedureCallGraphParser.class.getCanonicalName()).log(
				Level.INFO, "Analysing Code Area: " + area + "");

		while (it.hasNext()) {
			node = it.next();
			if (node.getSpecificType().equals("T_Proc_Call")) {
				area.addCall(node.getChildAt(0).getValue(),
						WSLCodeArea.PROCEDURE, node);
			} else if (node.getSpecificType().equals("T_Funct_Call")) {
				area.addCall(currentModule+":"+node.getChildAt(0).getValue(),
						WSLCodeArea.FUNCTION, node);
			} else if (node.getSpecificType().equals("T_BFunct_Call")) {
				area.addCall(currentModule+":"+node.getChildAt(0).getValue(),
						WSLCodeArea.BFUNCTION, node);
			} else if (node.getSpecificType().equals("T_MW_Proc_Call")) {
				area.addCall(node.getChildAt(0).getValue(),
						WSLCodeArea.MetaWSL_PROCEDURE, node);
			} else if (node.getSpecificType().equals("T_MW_Funct_Call")) {
				area.addCall(node.getChildAt(0).getValue(),
						WSLCodeArea.MetaWSL_FUNCTION, node);
			} else if (node.getSpecificType().equals("T_MW_BFunct_Call")) {
				area.addCall(node.getChildAt(0).getValue(),
						WSLCodeArea.MetaWSL_BFUNCTION, node);
			}
			if (parseExternCalls) {
				if (node.getSpecificType().equals("T_X_Proc_Call")) {
					area.addCall(node.getChildAt(0).getValue(),
							WSLCodeArea.External_PROCEDURE, node);
				} else if (node.getSpecificType().equals("T_X_Funct_Call")) {
					area.addCall(node.getChildAt(0).getValue(),
							WSLCodeArea.External_FUNCTION, node);
				} else if (node.getSpecificType().equals("T_X_BFunct_Call")) {
					area.addCall(node.getChildAt(0).getValue(),
							WSLCodeArea.External_BFUNCTION, node);
				} else if (node.getSpecificType().equals("T_A_Proc_Call")) {
					area.addCall(node.getChildAt(0).getValue(),
							WSLCodeArea.External_A_PROCEDURE, node);
				}
			}
			// Scan Functions
			if (node.getSpecificType().equals("T_BFunct")) {
				analyse(new WSLCodeArea(area.getName() + ":"
						+ node.getChildAt(0).getValue(), WSLCodeArea.BFUNCTION,
						file, node), new ASTNodePreOrderTreeIterator(node
						.getChildAt(3)));
				it.skipChildren();
			} else if (node.getSpecificType().equals("T_Funct")) {
				analyse(new WSLCodeArea(area.getName() + ":"
						+ node.getChildAt(0).getValue(), WSLCodeArea.FUNCTION,
						file, node), new ASTNodePreOrderTreeIterator(node
						.getChildAt(3)));
				it.skipChildren();
			} else if (node.getSpecificType().equals("T_Proc")) {
				analyse(new WSLCodeArea(area.getName() + ":"
						+ node.getChildAt(0).getValue(), WSLCodeArea.PROCEDURE,
						file, node), new ASTNodePreOrderTreeIterator(node
						.getChildAt(3)));
				it.skipChildren();
			} else if (node.getSpecificType().equals("T_MW_BFunct")) {
				analyse(new WSLCodeArea(area.getName() + ":"
						+ node.getChildAt(0).getValue(),
						WSLCodeArea.MetaWSL_BFUNCTION, file, node),
						new ASTNodePreOrderTreeIterator((ASTNode) node
								.getChildAt(3), node.getChildAt(4)));
				it.skipChildren();
			} else if (node.getSpecificType().equals("T_MW_Funct")) {
				analyse(new WSLCodeArea(area.getName() + ":"
						+ node.getChildAt(0).getValue(),
						WSLCodeArea.MetaWSL_FUNCTION, file, node),
						new ASTNodePreOrderTreeIterator(node.getChildAt(3),
								node.getChildAt(4)));
				it.skipChildren();
			} else if (node.getSpecificType().equals("T_MW_Proc")) {
				analyse(new WSLCodeArea(area.getName() + ":"
						+ node.getChildAt(0).getValue(),
						WSLCodeArea.MetaWSL_PROCEDURE, file, node),
						new ASTNodePreOrderTreeIterator(node.getChildAt(3)));
				it.skipChildren();
			} else if (node.getSpecificType().equals("T_Var_Lvalue")
					|| node.getSpecificType().equals("T_Variable")) {
				area.addVariable(node.getValue());
			}

		}

		areas.add(area);

		Logger.getLogger(FunctionProcedureCallGraphParser.class.getCanonicalName()).log(
				Level.INFO, "Code Area: " + area + " done ...");
	}

	private static ASTNode parseFile(File file) {

		// SimpleNode tree = null;
		ASTNode ret;

		Logger.getLogger(
				FunctionProcedureCallGraphParser.class.getCanonicalName()).log(
				Level.INFO, "Parsing WSL file... ");

		Console.getInstance().loadFile(file);

		ret = AST.getAST();

		return ret;
	}
}
