/**
 * Project: fme
 */

package fme.components.tree;

import java.awt.BorderLayout;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

import fme.components.abstraction.GUIComponent;
import fme.wsl.ast.AST;
import fme.wsl.ast.ASTNode;

/**
 * This class is the graphical representation of the AST of a WSL program
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class TreeGUI extends GUIComponent {

    /**
     * Defined for serialization
     */
    private static final long serialVersionUID = -3401595009203751689L;

    /**
     * The tree
     */
    private JTree tree;

    /**
     * The event handler
     */
    private TreeListener listener;

    /**
     * The status bar
     */
    private JLabel statusBar;

    /**
     * A ScrollPane for the tree
     */
    private JScrollPane scrollPane;

    /**
     * (non-Javadoc)
     * 
     * @see fme.components.abstraction.GUIComponent#initComponent()
     */
    protected boolean initComponent() {

        listener = new TreeListener(this);

        getInternalFrame().setJMenuBar(new TreeMenuBar(listener));

        setToolBar(new TreeToolBar(listener));
        setLayout(new BorderLayout());
        tree = new JTree(new DefaultMutableTreeNode("empty"));
        tree.putClientProperty("JTree.lineStyle", "Angled");
        tree.addTreeSelectionListener(listener);
        scrollPane = new JScrollPane(tree);
        add(scrollPane);

        statusBar = new JLabel(" ");
        add(statusBar, BorderLayout.SOUTH);

        return true;
    }

    /**
     * (non-Javadoc)
     * 
     * @see fme.components.abstraction.GUIComponent#getName()
     */
    public String getName() {
        return "Abstract Syntax Tree";
    }

    /**
     * (non-Javadoc)
     * 
     * @see fme.components.abstraction.GUIComponent#update()
     */
    public void update() {
        remove(scrollPane);
        tree = new JTree(AST.getAST());
        tree.putClientProperty("JTree.lineStyle", "Angled");
        tree.addTreeSelectionListener(listener);
        scrollPane = new JScrollPane(tree);
        add(scrollPane);
        revalidate();
    }

    public void disableTree() {
        remove(scrollPane);
        tree = new JTree(new DefaultMutableTreeNode("disabled"));
        tree.putClientProperty("JTree.lineStyle", "Angled");
        tree.addTreeSelectionListener(listener);
        scrollPane = new JScrollPane(tree);
        add(scrollPane);
        revalidate();
    }

    /**
     * Get the JTree
     * 
     * @return The tree
     */
    public JTree getTree() {
        return tree;
    }

    /**
     * Expands the whole tree
     */
    public void expandTree() {
        Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
            "Expand tree");
        for (int i = 0; i < tree.getRowCount(); i++)
            tree.expandRow(i);
    }

    /**
     * Expands tree to a specific row
     */
    public void expandTreeToRow(int row) {
        Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
            "Expand tree");
        for (int i = 0; i < row + 1; i++)
            tree.expandRow(i);
        tree.setSelectionRow(row);
    }

    /**
     * Expands tree to a specific row (specified by the FermaT engine)
     */
    public void expandTreeToPosn(String[] posn) {
        expandTreeToRow(gotoNode(posn, AST.getAST(), 0).getRow());
    }

    /**
     * Collapse the whole tree
     */
    public void collapseTree() {
        Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
            "Collapse tree");
        for (int i = 0; i < tree.getRowCount(); i++)
            tree.collapseRow(i);
    }

    public ASTNode getSelectedNode() {
        if (tree.getLastSelectedPathComponent() instanceof ASTNode)
            return (ASTNode) tree.getLastSelectedPathComponent();
        else
            return null;
    }

    public void updateStatusBar() {
        statusBar.setText(" Tree Row="
            + ((ASTNode) tree.getLastSelectedPathComponent()).getRow());
    }

    /**
     * Disable Component
     */
    public void disable() {
        listener.disable();
    }

    /**
     * Enable Component
     */
    public void enable() {
        listener.enable();
    }

    // Internal Methods
    // ================

    private ASTNode gotoNode(String[] posn, ASTNode parent, int pos) {
        int child;
        if (pos == posn.length)
            return parent;
        child = Integer.parseInt(posn[pos]);
        return gotoNode(posn, parent.getChildAt(child - 1), pos + 1);

    }
}
