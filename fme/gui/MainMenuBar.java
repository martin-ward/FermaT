/**
 * Project: fuml
 */

package fme.gui;

import java.awt.Event;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.JPopupMenu.Separator;

import fme.components.UndoManager;

/**
 * This class represents a menu bar
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau </A>
 */
public class MainMenuBar extends JMenuBar {

	private static Vector<JMenuItem> menuItems = new Vector<JMenuItem>();

	/**
	 * Field for identifying a class
	 */
	private static final long serialVersionUID = 2L;

	/**
	 * Type of MenuItem's
	 */
	private final int LABEL = 1;

	private final int LABEL_WITH_SEPARATOR = 2;

	/**
	 * Title for each Menu
	 */
	private String[] menuTitle = { "Project", "File", "Edit", "Diagram",
			"Window", "Help" };

	/**
	 * Key Events for the Menu
	 */
	private int[] menuKeyEvent = { KeyEvent.VK_P, KeyEvent.VK_F, KeyEvent.VK_E,
			KeyEvent.VK_A, KeyEvent.VK_D, KeyEvent.VK_W, KeyEvent.VK_H };

	/**
	 * Title for each Menu Item
	 */
	private String[][] menuItemTitle = {

			{ "New Project", "Open Project", "Save Project",
					"Show Project Manager", "Close Project", "Exit" },

			{ "Save code as intermediate version",
			  "Save code and delete intermediate versions",
			  "Save code as WSL file", "Run WSL File", "Show Metrics" },

			{ "Undo", "Redo" },

			{ "ActionSystem CallGraph", "Function/Procedure Call Graph",
					"Function/Procedure Call Graph (Whole Project)" },

			{ "Show all Windows", "Default Alignment", "Abreast Alignment",
					"Box Alignment" },

			{ "Info" } };

	/**
	 * Icon for each Menu Item (the value null will associate no icon)
	 */
	private String[][] menuItemIcon = {

	{ null, null, null, null, null, null },

	{ null, null, null, null, null },

	{ null, null },

	{ null, null, null },

	{ null, null, null, null },

	{ null } };

	/**
	 * To use the Menu with the Keyboard (the value -1 will associate no key)
	 */
	private int[][] menuItemKeyEvent = {

			{ KeyEvent.VK_N, KeyEvent.VK_O, KeyEvent.VK_S, -1,
					KeyEvent.VK_C, KeyEvent.VK_E },

			{ KeyEvent.VK_S, -1, -1, -1, KeyEvent.VK_M },

			{ KeyEvent.VK_U, KeyEvent.VK_R },

			{ -1, -1, -1 },

			{ -1, -1, -1, -1 },

			{ KeyEvent.VK_I } };

	/**
	 * Hot Keys for Menu Items pressed in combination with CTRL (the value -1
	 * will associate no key)
	 */
	private int[][] menuAccelerator = {

	{ KeyEvent.VK_N, KeyEvent.VK_P, -1, -1, -1, KeyEvent.VK_Q },

	{ KeyEvent.VK_S, -1, -1, KeyEvent.VK_E, KeyEvent.VK_M },

	{ KeyEvent.VK_Z, KeyEvent.VK_Y },

	{ -1, -1, -1 },

	{ -1, KeyEvent.VK_W, -1, -1 },

	{ -1 } };

	/**
	 * Names for events
	 */
	private String[][] event = {

			{ "Menu:NewProject", "Menu:OpenProject", "Menu:SaveProject",
					"Menu:ShowProjectManager", "Menu:CloseProject", "Menu:Exit" },

			{ "Menu:QuickSaveWSLFile", "Menu:SaveFinalWSLFile",
			  "Menu:ExportWSLFile", "Menu:Run", "Menu:Metrics" },

			{ "Menu:Undo", "Menu:Redo" },

			{ "Menu:ActionSystemCallGraph", "Menu:FunctionProcedureCallGraph",
					"Menu:FunctionProcedureCallGraphWholeProject" },

			{ "Menu:ShowAllWindows", "Menu:WindowsDefault",
					"Menu:WindowsAbreast", "Menu:WindowsBox" },

			{ "Menu:Info" } };

	/**
	 * Type of the menues (Label, checkbox,...)
	 */
	private int[][] menuItemType = {

	{ LABEL, LABEL, LABEL, LABEL, LABEL, LABEL },

	{ LABEL, LABEL, LABEL, LABEL, LABEL },

	{ LABEL, LABEL },

	{ LABEL, LABEL, LABEL },

	{ LABEL, LABEL, LABEL, LABEL },

	{ LABEL } };

	/**
	 * The Constructor
	 */
	public MainMenuBar(ActionListener listener) {

		JMenuItem menuItem;

		for (int i = 0; i < menuTitle.length; i++) {

			JMenu menu = new JMenu(menuTitle[i]);
			if (menuKeyEvent[i] != -1)
				menu.setMnemonic(menuKeyEvent[i]);

			for (int j = 0; j < menuItemTitle[i].length; j++) {

				if (menuItemType[i][j] == LABEL_WITH_SEPARATOR) {
					menu.add(new Separator());
				}
				if (menuItemType[i][j] == LABEL
						|| menuItemType[i][j] == LABEL_WITH_SEPARATOR) {
					menuItem = new JMenuItem(menuItemTitle[i][j]);

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

					// Register Menu items for enabling/disabling
					menuItems.add(menuItem);

					// Register the buttons to the EventTracker (for
					// enabling/disabling)
					if (menuItem.getText().equals("Undo")) {
						UndoManager.getInstance().registerUndoButton(menuItem);
					} else if (menuItem.getText().equals("Redo")) {
						UndoManager.getInstance().registerRedoButton(menuItem);
					}
				}
			}
			add(menu);
		}
		setEnableNonProjectMenues(false);
	}

	public static void setEnableNonProjectMenues(boolean e) {
		if (!e) {
			for (int i = 6; i < 16; i++) {
				MainMenuBar.disableMenu(i);
			}
		} else {
			for (int i = 6; i < 16; i++) {
				if (i != 11 && i != 12)
					MainMenuBar.enableMenu(i);
			}
		}
	}

	public static void disableMenu(int i) {
		try {
			menuItems.get(i).setEnabled(false);
		} catch (Exception e) {

		}
	}

	public static void enableMenu(int i) {
		menuItems.get(i).setEnabled(true);
	}
}
