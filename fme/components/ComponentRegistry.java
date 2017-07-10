/**
 * Project: fme
 */

package fme.components;

import java.util.HashMap;
import java.util.Vector;

import fme.components.abstraction.GUIComponent;
import fme.components.actionsystemcallgraph.ActionSystemCallGraph;
import fme.components.catalogue.Catalogue;
import fme.components.catalogue.CatalogueGUI;
import fme.components.console.Console;
import fme.components.console.ConsoleGUI;
import fme.components.console.ConsoleObserver;
import fme.components.editor.EditorGUI;
import fme.components.functionprocedurecallgraph.FunctionProcedureCallGraph;
import fme.components.historygraph.HistoryGraph;
import fme.components.tree.TreeGUI;

/**
 * This class is holds and manages all components of the FME
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class ComponentRegistry {

    // Update constants
    // ================

    /**
     * Update everything
     */
    public final static int NEW_FILE_LOADED = 1;

    public final static int TREE_MARKING = 2;

    public final static int EDITOR_MARKING = 3;

    public final static int NEW_FILE_WITH_ERROR_LOADED = 4;

    // Object variables
    // ================

    /**
     * The components of the FME
     */
    private static HashMap<String, Object> components = new HashMap<String, Object>();

    /**
     * The components for the GUI of the FME
     */
    private static HashMap<String, GUIComponent> guiComponents = new HashMap<String, GUIComponent>();

    /**
     * The Constructor
     */
    private ComponentRegistry() {
    }

    /**
     * Get a component
     * 
     * @param name
     *            The name of the component
     * @return The component
     */
    public static Object getComponent(String name) {
        Object ret = components.get(name);
        if (ret == null)
            return guiComponents.get(name);
        return ret;
    }

    /**
     * Get a GUI component
     * 
     * @param name
     *            The name of the component
     * @return The component
     */
    public static GUIComponent getGUIComponent(String name) {
        return guiComponents.get(name);
    }

    /**
     * Get a list of all GUI components
     * 
     * @return A list of components
     */
    public static Vector<GUIComponent> getGUIComponents() {
        return new Vector<GUIComponent>(guiComponents.values());
    }

    // Methods which control the components
    // ====================================

    /**
     * This method is executed when the FME starts
     */
    public static void startup() {

        // Start the console observer
        ConsoleObserver.start();
        // Init the transformation catalogue
        Catalogue.initCatalogue();
    }

    /**
     * This method is to register all components
     */
    public static void register() {

        // ******************
        // The Console
        // ******************

        // The console GUI
        guiComponents.put("ConsoleGUI", new ConsoleGUI());
        // A high level interface to the console
        components.put("Console", Console.getInstance());
        // A low level interface to the console
        components.put("ConsoleObserver", ConsoleObserver.getConsoleObserver());

        // *********************************************
        // Transformation Sequence Manager
        // *********************************************

        // The handler of the Transformation Sequence Manager
        components.put("automaton.AutomatonHandler", model.AutomatonHandler.getInstance());
        components.put("visualization.MainFrame", visualization.MainFrame.getInstance());

        // ******************
        // The Tree
        // ******************

        // The console GUI
        guiComponents.put("TreeGUI", new TreeGUI());

        // ******************
        // The Editor
        // ******************

        // The console GUI
        guiComponents.put("EditorGUI", new EditorGUI());

        // ******************
        // The Catalogue
        // ******************

        // The transformation catalogue GUI
        guiComponents.put("CatalogueGUI", new CatalogueGUI());
        // The transformation catalogue
        components.put("Catalogue", Catalogue.getInstance());

        // ******************
        // ActionSystem CallGraph
        // ******************

        // Action System CallGraph
        components.put("ActionSystemCallGraph", ActionSystemCallGraph
            .getInstance());

        // ******************
        // FunctionProcedure CallGraph
        // ******************

        // Action System CallGraph
        components.put("FunctionProcedureCallGraph", FunctionProcedureCallGraph
            .getInstance());

        // ******************
        // HistoryGraph
        // ******************

        // History Graph
        components.put("HistoryGraph", HistoryGraph.getInstance());
    }

    /**
     * This method should be executed when a component has changed something
     * 
     * @param updateMethod
     *            The scope of the update (for a complete update of all
     *            components use ComponentRegistry.COMPLETE_UPDATE)
     */
    public static void updateGUI(int updateMethod) {

        if (updateMethod == NEW_FILE_LOADED) {
            // Update the Tree
            guiComponents.get("TreeGUI").update();
            // Update the Editor
            guiComponents.get("EditorGUI").update();
            // Update the Catalogue
            guiComponents.get("CatalogueGUI").update();
        }
        if (updateMethod == NEW_FILE_WITH_ERROR_LOADED) {
            // Update the Tree
            ((TreeGUI) guiComponents.get("TreeGUI")).disableTree();
            // Update the Editor
            ((EditorGUI) guiComponents.get("EditorGUI")).displayFileWithError();
        } else if (updateMethod == TREE_MARKING) {
            ((Console) components.get("Console")).updateTreePosition(""
                + ((TreeGUI) guiComponents.get("TreeGUI")).getSelectedNode()
                    .getFermaTTreePath());
            ((FunctionProcedureCallGraph) components
                .get("FunctionProcedureCallGraph")).showItemFromTree();
            ((ActionSystemCallGraph) components.get("ActionSystemCallGraph"))
                .showItemFromTree();
            // Delete all Transformation applicable markings
            ((CatalogueGUI) guiComponents.get("CatalogueGUI"))
                .removeSelection();
        } else if (updateMethod == EDITOR_MARKING) {
            // Delete all Transformation applicable markings
            ((CatalogueGUI) guiComponents.get("CatalogueGUI"))
                .removeSelection();

        }

    }

    /**
     * Disable Editor and Tree
     */
    public static void disableItemSelectionComponents() {
        ((TreeGUI) guiComponents.get("TreeGUI")).disable();
        ((EditorGUI) guiComponents.get("EditorGUI")).disable();
    }

    /**
     * Enable Editor and Tree
     */
    public static void enableItemSelectionComponents() {
        ((TreeGUI) guiComponents.get("TreeGUI")).enable();
        ((EditorGUI) guiComponents.get("EditorGUI")).enable();
    }
}
