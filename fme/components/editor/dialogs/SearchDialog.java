/**
 * Project: fme
 */

package fme.components.editor.dialogs;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import fme.gui.MainFrame;

public class SearchDialog
    extends JDialog {

  private static final long serialVersionUID = 5097382550149870190L;

  private JPanel jPanel = null;

  private JTextField jTextField = null;

  private JPanel jPanel2 = null;

  private JLabel jLabel = null;

  private JLabel jLabel1 = null;

  private JPanel jPanel3 = null;

  private JCheckBox jCheckBox = null;

  private JCheckBox jCheckBox1 = null;

  private JButton jButton = null;

  private JButton jButton1 = null;

  private JButton jButton11 = null;

  private JCheckBox jCheckBox11 = null;

  private JLabel jLabel11 = null;
  
  /**
   * The singleton instance of this dialog
   */
  private static SearchDialog singleton;

  /**
   * The Actionlistener of this dialog
   */
  private ActionListener listener;

  private JLabel label11 = null;

  private JTextField jTextField1 = null;

  private JLabel label1 = null;

  private JButton jButton2 = null;

  /**
   * The Constructor
   * 
   * @param listener The ActionListener of this dialog
   */
  private SearchDialog(ActionListener listener) {
    super(MainFrame.getMainFrame(), "Find / Replace", false);
    this.listener = listener;
    initialize();
  }

  /**
   * Show the dialog
   */
  public static void showDialog(ActionListener listener) {
    if (singleton == null) {
      singleton = new SearchDialog(listener);
    }
    singleton.setVisible(true);
  }
  
  /**
   * Get the dialog
   */
  public static SearchDialog getDialog() {
    return singleton;
  }
  /**
   * Initialize the dialog
   */
  private void initialize() {
    this.setSize(new Dimension(272, 293));
    this.setLocation(MainFrame.getMainFrame().getX() + 200, MainFrame
        .getMainFrame().getY() + 100);
    this.setContentPane(getJPanel());
  }

  /**
   * This method initializes jPanel
   * 
   * @return javax.swing.JPanel
   */
  private JPanel getJPanel() {
    if (jPanel == null) {
      jPanel = new JPanel();
      jPanel.setLayout(null);
      jPanel.add(getJButton(), null);
      jPanel.add(getJButton1(), null);
      jPanel.add(getJButton11(), null);
      jPanel.add(getJPanel3(), null);
      jPanel.add(getJButton2(), null);
      jPanel.add(getJPanel2(), null);
    }
    return jPanel;
  }

  /**
   * This method initializes jTextField
   * 
   * @return javax.swing.JTextField
   */
  public JTextField getJTextField() {
    if (jTextField == null) {
      jTextField = new JTextField();
      jTextField.addActionListener(listener);
    }
    return jTextField;
  }

  /**
   * This method initializes jPanel2
   * 
   * @return javax.swing.JPanel
   */
  private JPanel getJPanel2() {
    if (jPanel2 == null) {
      GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
      gridBagConstraints2.gridx = 0;
      gridBagConstraints2.gridy = 2;
      GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
      gridBagConstraints1.gridx = 0;
      gridBagConstraints1.gridy = 1;
      GridBagConstraints gridBagConstraints = new GridBagConstraints();
      gridBagConstraints.gridx = 0;
      gridBagConstraints.gridy = 0;
      GridBagConstraints gridBagConstraints31 = new GridBagConstraints();
      gridBagConstraints31.gridy = 1;
      gridBagConstraints31.fill = GridBagConstraints.HORIZONTAL;
      gridBagConstraints31.insets = new Insets(0, 0, 0, 71);
      gridBagConstraints31.gridx = 1;
      GridBagConstraints gridBagConstraints30 = new GridBagConstraints();
      gridBagConstraints30.gridy = 2;
      gridBagConstraints30.fill = GridBagConstraints.HORIZONTAL;
      gridBagConstraints30.gridx = 1;
      GridBagConstraints gridBagConstraints29 = new GridBagConstraints();
      gridBagConstraints29.gridy = 0;
      gridBagConstraints29.fill = GridBagConstraints.HORIZONTAL;
      gridBagConstraints29.gridx = 1;
      jLabel11 = new JLabel();
      jLabel11.setText("Case Sensitive Search");
      jLabel1 = new JLabel();
      jLabel1.setText("Mark / Replace all");
      jLabel = new JLabel();
      jLabel.setText("Regular Expression");
      jPanel2 = new JPanel();
      jPanel2.setBorder(BorderFactory.createTitledBorder("Options"));
      jPanel2.setLayout(new GridBagLayout());
      jPanel2.setBounds(new Rectangle(14, 76, 235, 89));
      jPanel2.add(jLabel, gridBagConstraints29);
      jPanel2.add(jLabel1, gridBagConstraints30);
      jPanel2.add(jLabel11, gridBagConstraints31);
      jPanel2.add(getJCheckBox(), gridBagConstraints);
      jPanel2.add(getJCheckBox1(), gridBagConstraints1);
      jPanel2.add(getJCheckBox11(), gridBagConstraints2);
    }
    return jPanel2;
  }

  /**
   * This method initializes jPanel3
   * 
   * @return javax.swing.JPanel
   */
  private JPanel getJPanel3() {
    if (jPanel3 == null) {
      GridBagConstraints gridBagConstraints16 = new GridBagConstraints();
      gridBagConstraints16.gridx = 0;
      gridBagConstraints16.anchor = GridBagConstraints.EAST;
      gridBagConstraints16.gridy = 0;
      label1 = new JLabel();
      label1.setText("Find :");
      GridBagConstraints gridBagConstraints15 = new GridBagConstraints();
      gridBagConstraints15.fill = GridBagConstraints.BOTH;
      gridBagConstraints15.gridy = 1;
      gridBagConstraints15.weightx = 1.0;
      gridBagConstraints15.gridx = 1;
      GridBagConstraints gridBagConstraints14 = new GridBagConstraints();
      gridBagConstraints14.fill = GridBagConstraints.BOTH;
      gridBagConstraints14.gridy = 0;
      gridBagConstraints14.weightx = 1.0;
      gridBagConstraints14.gridx = 1;
      GridBagConstraints gridBagConstraints12 = new GridBagConstraints();
      gridBagConstraints12.gridx = 0;
      gridBagConstraints12.gridy = 1;
      label11 = new JLabel();
      label11.setText("Replace :");
      jPanel3 = new JPanel();
      jPanel3.setLayout(new GridBagLayout());
      jPanel3.setBounds(new Rectangle(16, 8, 234, 59));
      jPanel3.add(label11, gridBagConstraints12);
      jPanel3.add(getJTextField(), gridBagConstraints14);
      jPanel3.add(getJTextField1(), gridBagConstraints15);
      jPanel3.add(label1, gridBagConstraints16);
    }
    return jPanel3;
  }

  /**
   * This method initializes jCheckBox
   * 
   * @return javax.swing.JCheckBox
   */
  public JCheckBox getJCheckBox() {
    if (jCheckBox == null) {
      jCheckBox = new JCheckBox();
    }
    return jCheckBox;
  }

  /**
   * This method initializes jCheckBox1
   * 
   * @return javax.swing.JCheckBox
   */
  public JCheckBox getJCheckBox1() {
    if (jCheckBox1 == null) {
      jCheckBox1 = new JCheckBox();
    }
    return jCheckBox1;
  }

  /**
   * This method initializes jButton
   * 
   * @return javax.swing.JButton
   */
  private JButton getJButton() {
    if (jButton == null) {
      jButton = new JButton();
      jButton.addActionListener(listener);
      jButton.setText("Find");
      jButton.setBounds(new Rectangle(15, 185, 109, 26));
    }
    return jButton;
  }

  /**
   * This method initializes jButton1
   * 
   * @return javax.swing.JButton
   */
  private JButton getJButton1() {
    if (jButton1 == null) {
      jButton1 = new JButton();
      jButton1.addActionListener(listener);
      jButton1.setText("Cancel");
      jButton1.setBounds(new Rectangle(16, 223, 107, 26));
    }
    return jButton1;
  }

  /**
   * This method initializes jButton11
   * 
   * @return javax.swing.JButton
   */
  private JButton getJButton11() {
    if (jButton11 == null) {
      jButton11 = new JButton();
      jButton11.addActionListener(listener);
      jButton11.setText("Help");
      jButton11.setBounds(new Rectangle(142, 222, 107, 26));
    }
    return jButton11;
  }

  /**
   * This method initializes jCheckBox11
   * 
   * @return javax.swing.JCheckBox
   */
  public JCheckBox getJCheckBox11() {
    if (jCheckBox11 == null) {
      jCheckBox11 = new JCheckBox();
    }
    return jCheckBox11;
  }

  /**
   * This method initializes jTextField1	
   * 	
   * @return javax.swing.JTextField	
   */
  public JTextField getJTextField1() {
    if (jTextField1 == null) {
      jTextField1 = new JTextField();
    }
    return jTextField1;
  }

  /**
   * This method initializes jButton2	
   * 	
   * @return javax.swing.JButton	
   */
  private JButton getJButton2() {
    if (jButton2 == null) {
      jButton2 = new JButton();
      jButton2.addActionListener(listener);
      jButton2.setText("Replace");
      jButton2.setBounds(new Rectangle(143, 185, 106, 26));
    }
    return jButton2;
  }

} // @jve:decl-index=0:visual-constraint="10,10"
