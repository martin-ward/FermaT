/**
 * Project: fme
 */

package fme.components.console;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import fme.components.abstraction.GUIComponent;
import fme.config.CM;

/**
 * This class is the graphical representation of the FermaT console
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class ConsoleGUI
    extends GUIComponent {

  /**
   * Defined for serialization
   */
  private static final long serialVersionUID = -3401595009203751689L;

  /**
   * The text area for the engine output
   */
  private JTextArea textarea;

  /**
   * The input field for the user commands
   */
  private JTextField inputField;

  /**
   * A scroll pane for the engine output
   */
  private JScrollPane textareaScrollPane;

  /**
   * (non-Javadoc)
   * @see fme.components.abstraction.GUIComponent#initComponent()
   */
  protected boolean initComponent() {

    setLayout(new BorderLayout());

    textarea = new JTextArea();
    textarea.setFont(Font.decode((String) CM.getAsString("gui.Console.Font")));
    textarea.setBackground((Color) CM.get("gui.Console.BackgroundColor"));
    textarea.setForeground((Color) CM.get("gui.Console.FontColor"));
    textarea.setCaretColor((Color) CM.get("gui.Console.CaretColor"));
    textarea.setEditable(false);

    textareaScrollPane = new JScrollPane(textarea);

    inputField = new JTextField();
    inputField.addActionListener(new ConsoleInputListener());

    add(textareaScrollPane, BorderLayout.CENTER);
    add(inputField, BorderLayout.SOUTH);

    return true;
  }

  /**
   * (non-Javadoc)
   * @see fme.components.abstraction.GUIComponent#getName()
   */
  public String getName() {
    return "Console";
  }

  /**
   * Get the text area for the console output
   * 
   * @return The text area of the console
   */
  public JTextArea getTextArea() {
    return textarea;
  }

  /**
   * Clears the console
   */
  public void clearConsole() {
    Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
        "Clearing the console");
    
    textarea.setText("");    
  }
  
  /**
   * (non-Javadoc)
   * 
   * @see fme.components.abstraction.GUIComponent#update()
   */
  public void update() {
  }
}
