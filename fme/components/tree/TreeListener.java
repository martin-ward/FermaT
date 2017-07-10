/**
 * Project: fme
 */

package fme.components.tree;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import fme.components.ComponentRegistry;
import fme.components.console.Console;
import fme.components.editor.EditorGUI;
import fme.config.CM;
import fme.wsl.ast.AST;
import fme.wsl.ast.ASTNode;
import fme.wsl.lexer.LexerToken;
import fme.wsl.lexer.WSLLexer;

/**
 * This class listens for events from the tree toolbar
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class TreeListener implements ActionListener, TreeSelectionListener {

    /**
     * The TreeGUI object which instanciates this handler
     */
    private TreeGUI treeGUI;

    private boolean noUpdate = false;

    /**
     * The Constructor
     * 
     * @param treeGUI
     *            The TreeGUI object which instanciates this handler
     */
    public TreeListener(TreeGUI treeGUI) {
        this.treeGUI = treeGUI;
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
        Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
            "Action Event:" + e.getActionCommand());

        if (e.getActionCommand().equals("TreeToolBar:expandTree")
            || e.getActionCommand().equals("Menu:expandTree"))
            treeGUI.expandTree();
        else if (e.getActionCommand().equals("TreeToolBar:collapseTree")
            || e.getActionCommand().equals("Menu:collapseTree"))
            treeGUI.collapseTree();
    }

    /**
     * (non-Javadoc)
     * 
     * @see javax.swing.event.TreeSelectionListener#valueChanged(javax.swing.event.TreeSelectionEvent)
     */
    public void valueChanged(TreeSelectionEvent e) {

        Vector<LexerToken> tokenList = WSLLexer.getTokens();
        int st, en;

        if (noUpdate || Console.getInstance().isDocumentErroneous()
            || AST.getAST() == null)
            return;

        ASTNode astNode = ((ASTNode) ((JTree) e.getSource())
            .getLastSelectedPathComponent());

        Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
            "Click on Tree Node:" + astNode.getSpecificType());

        treeGUI.updateStatusBar();

        EditorGUI editor = (EditorGUI) ComponentRegistry
            .getComponent("EditorGUI");

        // Mark the AST Node in the source code

        // Mark nothing cause the AST node is not in the source code
        if (astNode.getLexerToken().size() == 0) {
            ComponentRegistry.updateGUI(ComponentRegistry.TREE_MARKING);
            return;
        }
        // Mark a block
        else if (astNode.getLexerToken().size() == 2) {
            st = tokenList.get(astNode.getLexerToken().get(0).intValue())
                .getPosStart();
            en = tokenList.get(astNode.getLexerToken().get(1).intValue())
                .getPosEnd();

            Logger.getLogger(this.getClass().getCanonicalName()).log(
                Level.INFO, "  Marking block from:" + st + " to " + en);
        }
        // Mark a single statement
        else {
            st = tokenList.get(astNode.getLexerToken().get(0).intValue())
                .getPosStart();
            en = tokenList.get(astNode.getLexerToken().get(0).intValue())
                .getPosEnd();

            Logger.getLogger(this.getClass().getCanonicalName()).log(
                Level.INFO, "  Marking single token from:" + st + " to " + en);
        }
        noUpdate = true;
        editor.markText(st, en, CM.getAsColor("gui.Editor.BlockColor"));
        editor.showPosition(st);

        // Ensure that no other marking event has overlayed this
        treeGUI.expandTreeToRow(astNode.getRow());
        editor.markText(st, en, CM.getAsColor("gui.Editor.BlockColor"));
        editor.showPosition(st);
        noUpdate = false;

        ComponentRegistry.updateGUI(ComponentRegistry.TREE_MARKING);
        ComponentRegistry.updateGUI(ComponentRegistry.EDITOR_MARKING);
    }

    /**
     * Disable Component
     */
    public void disable() {
        noUpdate = true;
    }

    /**
     * Enable Component
     */
    public void enable() {
        noUpdate = false;
    }
}
