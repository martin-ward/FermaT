/**
 * Project: fuml
 */

package fme.components.catalogue;

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
public class CatalogueToolBar
    extends JToolBar {

  /**
   * Field for identifying a class
   */
  private static final long serialVersionUID = 2919158343769786933L;

  /**
   * The tool tip text for the single buttons
   */
  private String[] toolTipText = { "Apply Transformation","Apply Transformation With Argument", "Test Transformation", "Test All Transformation", "Description of Transformation" }; 

  /**
   * The icon for the single buttons
   */
  private String[] icon = { "icons/apply.gif","icons/apply2.gif", "icons/info1.gif", "icons/info2.gif", "icons/info3.gif" }; 
  
  /**
   * The action command for the single buttons
   */
  private String[] actionCommand = { "CatalogueToolBar:ApplyTransformation","CatalogueToolBar:ApplyTransformationWithArgument", "CatalogueToolBar:TestTransformation", "CatalogueToolBar:TestAllTransformation", "CatalogueToolBar:HelpTransformation"}; 
  
  /**
   * The Constructor
   */
  public CatalogueToolBar(ActionListener listener) {
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
