/**
 * Project: fme
 */

package fme.components.console;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import fme.components.ComponentRegistry;

/**
 * This class listens for console events from the user and processes the typed
 * command to the console process in the background.
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class ConsoleInputListener implements ActionListener {

  /**
   * The Constructor 
   */
  public ConsoleInputListener() {
  }

  /**
   * (non-Javadoc)
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    ((ConsoleGUI) ComponentRegistry.getGUIComponent("ConsoleGUI"))
        .getTextArea().append(">" + e.getActionCommand() + "\n");
    ConsoleObserver.getConsoleObserver().sendCommand(
        e.getActionCommand() + "\n");
  }
}