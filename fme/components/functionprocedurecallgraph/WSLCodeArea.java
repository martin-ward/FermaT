package fme.components.functionprocedurecallgraph;

import java.io.File;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import sve.structures.nodes.CollapsableTextNode;
import fme.wsl.ast.ASTNode;

public class WSLCodeArea {

    public static final int MODULE = 0, PROCEDURE = 1, FUNCTION = 2,
        BFUNCTION = 2, MetaWSL_PROCEDURE = 3, MetaWSL_FUNCTION = 4,
        MetaWSL_BFUNCTION = 5, External_PROCEDURE = 6, External_FUNCTION = 7,
        External_BFUNCTION = 8, External_A_PROCEDURE = 9;

    private int type;

    private String name;

    private Vector<String> calls;

    private Vector<Integer> callTypes;

    private Vector<ASTNode> callASTNodes;

    private Vector<String> variables;

    private CollapsableTextNode gnode;
    
    private File file;

    private ASTNode node;

    public WSLCodeArea(String name, int type, File file, ASTNode node) {
        this.name = name;
        this.type = type;
        this.file = file;
        calls = new Vector<String>();
        callTypes = new Vector<Integer>();
        callASTNodes = new Vector<ASTNode>();
        variables = new Vector<String>();
        this.node = node;
    }

    public Vector<String> getCalls() {
        return calls;
    }

    public Vector<Integer> getCallTypes() {
        return callTypes;
    }

    public void addCall(String call, int type, ASTNode node) {
        Logger.getLogger(this.getClass().getCanonicalName()).log(
            Level.INFO,
            "Add call to: " + call + " (Type:" + typeToString(type) + ")");
        calls.add(call);
        callTypes.add(new Integer(type));
        callASTNodes.add(node);
    }
    
    public String getName() {
        return name;
    }

    public String getLocalName() {
        return name.split(":")[name.split(":").length - 1];
    }

    public int getType() {
        return type;
    }

    public File getFile() {
        return file;
    }

    public Vector<ASTNode> getCallASTNodes() {
        return callASTNodes;
    }

    public ASTNode getNode() {
        return node;
    }

    public String toString() {

        return name + " (" + typeToString(type) + ")";
    }

    public void addVariable(String v) {
        variables.add(v);
    }

    public Vector<String> getVariables() {
        return variables;
    }

    public CollapsableTextNode getGnode() {
        return gnode;
    }

    public void setGnode(CollapsableTextNode gnode) {
        this.gnode = gnode;
    }

    // Internal Methods
    // ================

    private String typeToString(int type) {

        if (type == WSLCodeArea.PROCEDURE)
            return "Procedure";
        else if (type == WSLCodeArea.FUNCTION)
            return "Function";
        else if (type == WSLCodeArea.BFUNCTION)
            return "Boolean Function";
        else if (type == WSLCodeArea.MetaWSL_PROCEDURE)
            return "MetaWSL Procedure";
        else if (type == WSLCodeArea.MetaWSL_FUNCTION)
            return "MetaWSL Function";
        else if (type == WSLCodeArea.MetaWSL_BFUNCTION)
            return "MetaWSL Boolean Function";
        else if (type == WSLCodeArea.External_PROCEDURE)
            return "External Procedure";
        else if (type == WSLCodeArea.External_FUNCTION)
            return "External Function";
        else if (type == WSLCodeArea.External_BFUNCTION)
            return "External Boolean Function";
        else if (type == WSLCodeArea.External_A_PROCEDURE)
            return "External A Procedure";
        else
            return "Module";
    }
}
