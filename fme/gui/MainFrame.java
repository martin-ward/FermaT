package fme.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowStateListener;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;

import fme.components.ComponentRegistry;
import fme.components.MainEventHandler;
import fme.components.abstraction.GUIComponent;
import fme.config.CM;

/**
 * This class represents the main frame of the Fermat Maintenance Environment
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau </A>
 */
public class MainFrame extends JFrame {

	/**
	 * For serialization
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * The desktop pane of the main frame
	 */
	private JDesktopPane desktopPane;

	/**
	 * The main event handler
	 */
	private MainEventHandler eventHandler;

	/**
	 * The project toolbar
	 */
	private ProjectToolBar projectToolBar;

	/**
	 * The menu bar
	 */
	private JMenuBar menuBar;

	/**
	 * The only instance of this class (singleton design pattern)
	 */
	private static MainFrame singleton = new MainFrame();

	/**
	 * The Constructor
	 */
	private MainFrame() {
		// Set the title
		super("FermaT Maintenance Environment");

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Initialising GUI ...");

		eventHandler = new MainEventHandler();
		menuBar = new MainMenuBar(eventHandler);
		desktopPane = new JDesktopPane();

		// Set initial size
		setSize(CM.getAsInt("gui.MainFrame.InitWidth"), CM
				.getAsInt("gui.MainFrame.InitHeight"));
		setLocation(CM.getAsInt("gui.MainFrame.InitXPos"), CM
				.getAsInt("gui.MainFrame.InitYPos"));

		desktopPane.setBackground(CM.getAsColor("gui.MainFrame.Background"));
		desktopPane.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);

		// Handling for window closing event
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				eventHandler.actionPerformed(new ActionEvent(this, 0,
						"Menu:Exit"));
			}
		});

		addWindowStateListener(new WindowStateListener() {
			public void windowStateChanged(WindowEvent e) {
				((MainFrame) e.getSource()).validate();
				eventHandler.actionPerformed(new ActionEvent(this, 0,
						"Menu:WindowsDefault"));
			}
		});

		addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent arg0) {
				eventHandler.actionPerformed(new ActionEvent(this, 0,
						"Menu:WindowsDefault"));
			}
		});

		setJMenuBar(menuBar);
		projectToolBar = new ProjectToolBar(eventHandler);
		getContentPane().add(projectToolBar, BorderLayout.PAGE_START);
		getContentPane().add(new JScrollPane(desktopPane));
	}

	/**
	 * Get the only instance of this MainFrame
	 * 
	 * @return The main frame
	 */
	public static MainFrame getMainFrame() {
		return singleton;
	}

	/**
	 * Get the EventHandler of this MainFrame
	 * 
	 * @return Returns the DesktopPane of this MainFrame
	 */
	public JDesktopPane getDesktopPane() {
		return desktopPane;
	}

	/**
	 * Get the EventHandler of this MainFrame
	 * 
	 * @return Returns the EventHandler of this MainFrame
	 */
	public MainEventHandler getEventHandler() {
		return eventHandler;
	}

	/**
	 * Get the ProjectToolBar of this MainFrame
	 * 
	 * @return Returns the ProjectToolBar of this MainFrame
	 */
	public ProjectToolBar getProjectToolBar() {
		return projectToolBar;
	}

	/**
	 * Show all components defined in the ComponentRegistry
	 */
	public void showComponents() {
		int i = 0;
		Iterator<GUIComponent> it = ComponentRegistry.getGUIComponents()
				.iterator();

		while (it.hasNext()) {
			GUIComponent c = it.next();
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.INFO, "Showing component " + c.getName());
			desktopPane.add(c.getInternalFrame());
			c.getInternalFrame().setVisible(true);
			c.getInternalFrame().setLocation(
					CM.getAsInt("gui.InternalFrame.InitXPos") + i,
					CM.getAsInt("gui.InternalFrame.InitYPos") + i);
			i += CM.getAsInt("gui.InternalFrame.Gap");
		}

		// Apply the default layout
		eventHandler.actionPerformed(new ActionEvent(this, 0,
				"Menu:WindowsDefault"));
	}

	/**
	 * Show a single component defined in the ComponentRegistry
	 * 
	 * @param component
	 */
	public void showComponent(String component) {
		ComponentRegistry.getGUIComponent(component).getInternalFrame()
				.setVisible(true);
	}
}
