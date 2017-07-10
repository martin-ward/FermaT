/**
 * Project: fuml
 */

package fme.components.tree;

import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;

/**
 * This class represents the toolbar above the graphical tree
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class TreeToolBar
    extends JToolBar {

  /**
   * Field for identifying a class
   */
  private static final long serialVersionUID = 2919158343769786933L;

  /**
   * The tool tip text for the single buttons
   */
  private String[] toolTipText = { "Expand Tree", "Collapse Tree" }; 

  /**
   * The icon for the single buttons
   */
  private String[] icon = { "icons/tree_expand.gif", "icons/tree_collapse.gif" }; 
  
  /**
   * The action command for the single buttons
   */
  private String[] actionCommand = { "TreeToolBar:expandTree", "TreeToolBar:collapseTree" }; 
  
  /**
   * The Constructor
   */
  public TreeToolBar(ActionListener listener) {
    JButton button;

    this.setFloatable(false);
    this.setMinimumSize(new Dimension(100, 250));
    
    for(int i=0;i<toolTipText.length;i++) {
      button = new JButton();
      button.setToolTipText(toolTipText[i]);
      button.setIcon(new ImageIcon(icon[i]));
      button.setActionCommand(actionCommand[i]);
      button.addActionListener(listener);
      add(button);      
    }
  }
}
