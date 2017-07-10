/**
 * Project: fuml
 */

package fme.components.actionsystemcallgraph;

import java.awt.Event;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.JPopupMenu.Separator;

/**
 * This class represents a menu bar
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau </A>
 */
public class ActionSystemCallGraphMenuBar extends JMenuBar {

	/**
	 * Field for identifying a class
	 */
	private static final long serialVersionUID = 2L;

	/**
	 * Type of MenuItem's
	 */
	private final int LABEL = 1;

	private final int LABEL_WITH_SEPARATOR = 2;

	private final int CHECKBOX_LABEL = 3;

	/**
	 * Title for each Menu
	 */
	private String[] menuTitle = { "Diagram", "Layout" };

	/**
	 * Key Events for the Menu
	 */
	private int[] menuKeyEvent = { KeyEvent.VK_D, KeyEvent.VK_L };

	/**
	 * Title for each Menu Item
	 */
	private String[][] menuItemTitle = {
			{ "Export Screenshot", "Export Diagram", "Print Diagram" },
			{ "Tree Layout", "Circle Layout", "Grid Layout",
					"Flow Layout (Edges)" } };

	/**
	 * Icon for each Menu Item (the value null will associate no icon)
	 */
	private String[][] menuItemIcon = { { null, null, null },
			{  null, null, null, null } };

	/**
	 * To use the Menu with the Keyboard (the value -1 will associate no key)
	 */
	private int[][] menuItemKeyEvent = {
			{ KeyEvent.VK_S, KeyEvent.VK_D, KeyEvent.VK_P },
			{  -1, -1, -1, -1 } };

	/**
	 * Hot Keys for Menu Items pressed in combination with CTRL (the value -1
	 * will associate no key)
	 */
	private int[][] menuAccelerator = { { -1, -1, -1 }, {  -1, -1, -1, -1 } };

	/**
	 * Names for events
	 */
	private String[][] event = {
			{ "Menu:ExportScreenshot", "Menu:ExportDiagram", "Menu:Print" },
			{  "Menu:LayoutTree", "Menu:LayoutCircle",
					"Menu:LayoutGrid", "Menu:LayoutFlow" } };

	/**
	 * Type of the menues (Label, checkbox,...)
	 */
	private int[][] menuItemType = { { LABEL, LABEL, LABEL },
			{  LABEL, LABEL, LABEL, LABEL } };

	/**
	 * The Constructor
	 */
	public ActionSystemCallGraphMenuBar(ActionListener listener) {

		JMenuItem menuItem;

		for (int i = 0; i < menuTitle.length; i++) {

			JMenu menu = new JMenu(menuTitle[i]);
			if (menuKeyEvent[i] != -1)
				menu.setMnemonic(menuKeyEvent[i]);

			for (int j = 0; j < menuItemTitle[i].length; j++) {

				if (menuItemType[i][j] == LABEL_WITH_SEPARATOR) {
					menu.add(new Separator());
				}

				if (menuItemType[i][j] == CHECKBOX_LABEL) {
					menuItem = new JCheckBoxMenuItem(menuItemTitle[i][j]);
					((JCheckBoxMenuItem) menuItem).setState(true);
				} else {
					menuItem = new JMenuItem(menuItemTitle[i][j]);
				}

				if (menuItemIcon[i][j] != null
						&& !menuItemIcon[i][j].equals(""))
					menuItem.setIcon(new ImageIcon(menuItemIcon[i][j]));
				else
					menuItem.setIcon(new ImageIcon(""));

				if (menuItemKeyEvent[i][j] != -1)
					menuItem.setMnemonic(menuItemKeyEvent[i][j]);

				if (menuAccelerator[i][j] != -1)
					menuItem.setAccelerator(KeyStroke.getKeyStroke(
							menuAccelerator[i][j], Event.CTRL_MASK));

				menuItem.setActionCommand(event[i][j]);

				menuItem.addActionListener(listener);

				menu.add(menuItem);

			}
			add(menu);
		}
	}
}
