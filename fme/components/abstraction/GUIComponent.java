/**
 * Project: fme
 */

package fme.components.abstraction;

import java.awt.BorderLayout;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JInternalFrame;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import fme.config.CM;

/**
 * This class is an abstract GUI component of the FME. Every component is
 * displayed in an own internal window.
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public abstract class GUIComponent extends JPanel {

	private static final long serialVersionUID = 8803155117799057753L;

	private JInternalFrame frame;

    private JToolBar toolBar;

    /**
     * The Constructor
     */
    public GUIComponent() {
        Logger.getLogger(this.getClass().getCanonicalName()).log(
            Level.INFO,
            "Initializing component: " + getName() + " ("
                + this.getClass().getCanonicalName() + ")");

        frame = new JInternalFrame(getName(), // name
            true, // resizable
            false, // closable
            true, // maximizable
            true);// iconifiable);

        frame.setFrameIcon(new ImageIcon());
        frame.getContentPane().add(this);

        frame.setSize(CM.getAsInt("gui.InternalFrame.InitWidth"), CM
            .getAsInt("gui.InternalFrame.InitHeight"));

        initComponent();

        if (toolBar != null)
            frame.getContentPane().add(toolBar, BorderLayout.PAGE_START);
    }

    /**
     * This routine initializes the component
     * 
     * @return True if the initialization was successful
     */
    protected abstract boolean initComponent();

    /**
     * Get the Name of the component
     * 
     * @return The name
     */
    public abstract String getName();

    /**
     * Update all sub components of this component
     */
    public abstract void update();

    /**
     * Get the internal frame of this component
     * 
     * @return The internal frame
     */
    public JInternalFrame getInternalFrame() {
        return frame;
    }

    /**
     * Set a toolbar for this component
     * 
     * @param bar
     *            The toolbar
     */
    public void setToolBar(JToolBar bar) {
        toolBar = bar;
    }
}
