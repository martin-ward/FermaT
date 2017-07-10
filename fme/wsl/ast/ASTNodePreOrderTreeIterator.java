/**
 * Project: fme
 */

package fme.wsl.ast;

import java.util.Iterator;
import java.util.Vector;

import javax.swing.tree.TreeNode;

/**
 * This iterator travels through the whole tree (top down and from left to
 * right)
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class ASTNodePreOrderTreeIterator implements Iterator<TreeNode> {

    private Vector<ASTNode> nodes;

    private Iterator<ASTNode> i;

    private ASTNode lastNode;

    /**
     * The Constructor
     * 
     * @param node
     *            The top tree node
     */
    public ASTNodePreOrderTreeIterator(ASTNode node) {
        nodes = new Vector<ASTNode>();
        nodes.add(node);
        fillNodes(node);
        i = nodes.iterator();
    }

    public ASTNodePreOrderTreeIterator(ASTNode node1, ASTNode node2) {
        nodes = new Vector<ASTNode>();
        nodes.add(node1);
        fillNodes(node1);
        nodes.add(node2);
        fillNodes(node2);
        i = nodes.iterator();
    }

    private void fillNodes(ASTNode n) {
        for (int i = 0; i < n.getChildren().size(); i++) {
            nodes.add(n.getChildren().get(i));
            fillNodes(n.getChildren().get(i));
        }
    }

    public boolean hasNext() {
        return i.hasNext();
    }

    public ASTNode next() {
        lastNode = i.next();
        return lastNode;
    }

    public void remove() {
        i.remove();
    }

    public void skipChildren() {
        int cc = lastNode.getAllChildrenCount();
        for (int j = 0; j < cc; j++) {
            i.next();
        }
    }

    public void renew() {
        i = nodes.iterator();
    }

    public Vector<ASTNode> getAllNodes() {
        return nodes;
    }

}
