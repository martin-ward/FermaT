/**
 * Project: fuml
 */

package fme.config;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * This class is the ConfigManager. It holds the configurations for the FME
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class CM {

    /**
     * The line separator (system dependent)
     */
    public static final String ls = System.getProperty("line.separator");

    /**
     * The static reference to the singleton of this class
     */
    private static CM singleton = new CM();

    /**
     * The current configuration
     */
    private HashMap<String, Object> config;

    /**
     * The Constructor
     */
    private CM() {

        String key;
        Iterator<String> i;

        config = new HashMap<String, Object>();

        // Changeable standard values
        // ==========================

        // config.put("StartDirectory", "_Home_");
        config.put("StartDirectory", "_CurrentDir_");
        config.put("Look_and_Feel", "Substance");

        config.put("gui.MainFrame.InitWidth", new Integer(800));
        config.put("gui.MainFrame.InitHeight", new Integer(600));
        config.put("gui.MainFrame.InitXPos", new Integer(50));
        config.put("gui.MainFrame.InitYPos", new Integer(50));
        config.put("gui.MainFrame.Background", new Color(0xC0, 0xC0, 0xFF));

        config.put("gui.InternalFrame.InitWidth", new Integer(450));
        config.put("gui.InternalFrame.InitHeight", new Integer(350));
        config.put("gui.InternalFrame.InitXPos", new Integer(100));
        config.put("gui.InternalFrame.InitYPos", new Integer(50));
        config.put("gui.InternalFrame.Gap", new Integer(50));

        config.put("gui.Console.Font", "Courier-BOLD-12");
        config.put("gui.Console.FontColor", Color.WHITE);
        config.put("gui.Console.BackgroundColor", Color.BLACK);
        config.put("gui.Console.CaretColor", Color.PINK);

        config.put("gui.Editor.Font", "Courier-BOLD-12");
        config.put("gui.Editor.FontLarge", "Courier-BOLD-18");
        config.put("gui.Editor.FontColor", Color.BLACK);
        config.put("gui.Editor.BackgroundColor", Color.WHITE);
        config.put("gui.Editor.CaretColor", Color.PINK);
        config.put("gui.Editor.BlockColor", Color.ORANGE);
        config.put("gui.Editor.BlockColorUnidentified", Color.MAGENTA);
        config.put("gui.Editor.SearchColor", Color.CYAN);

        config.put("gui.TypeSystemEditor.InitWidth", new Integer(800));
        config.put("gui.TypeSystemEditor.InitHeight", new Integer(600));
        config.put("gui.TypeSystemEditor.InitXPosOffset", new Integer(50));
        config.put("gui.TypeSystemEditor.InitYPosOffset", new Integer(50));
        config.put("gui.TypeSystemEditor.Font", "Courier-PLAIN-14");
        config.put("gui.TypeSystemEditor.SearchMarking", Color.CYAN);
        config.put("gui.TypeSystemEditor.VariableMarking", Color.ORANGE);
        config.put("gui.TypeSystemEditor.BlockMarking", Color.YELLOW);
        config.put("gui.TypeSystemEditor.Editor.InitWidth", new Integer(800));
        config.put("gui.TypeSystemEditor.Editor.InitHeight", new Integer(600));
        config.put("gui.TypeSystemEditor.Editor.InitXPosOffset", new Integer(
            100));
        config.put("gui.TypeSystemEditor.Editor.InitYPosOffset", new Integer(
            100));

        config.put("gui.TypeTransformationCatalogue.InitWidth",
            new Integer(250));
        config.put("gui.TypeTransformationCatalogue.InitHeight", new Integer(
            300));
        config.put("gui.TypeTransformationCatalogue.InitXPosOffset",
            new Integer(300));
        config.put("gui.TypeTransformationCatalogue.InitYPosOffset",
            new Integer(80));

        config.put("gui.ProjectManager.InitXPos", new Integer(150));
        config.put("gui.ProjectManager.InitYPos", new Integer(50));
        config.put("gui.ProjectManager.InitWidth", new Integer(450));
        config.put("gui.ProjectManager.InitHeight", new Integer(300));

        config.put("gui.HistoryGraph.LineColor", Color.BLACK);
        config.put("gui.HistoryGraph.MarkedLineColor", Color.CYAN);
        config.put("gui.HistoryGraph.NodeColor", Color.YELLOW);

        config.put("gui.ActionSystemCallGraph.LineColor", Color.BLACK);
        config.put("gui.ActionSystemCallGraph.MarkedLineColor", Color.CYAN);
        config.put("gui.ActionSystemCallGraph.NodeColor", Color.YELLOW);

        config.put("gui.FunctionProcedureCallGraph.LineColor", Color.BLACK);
        config
            .put("gui.FunctionProcedureCallGraph.MarkedLineColor", Color.CYAN);
        config.put("gui.FunctionProcedureCallGraph.NodeColor.MODULE",
            Color.YELLOW);
        config.put("gui.FunctionProcedureCallGraph.NodeColor.PROCEDURE",
            Color.GREEN);
        config.put("gui.FunctionProcedureCallGraph.NodeColor.FUNCTION",
            Color.GREEN);
        config.put("gui.FunctionProcedureCallGraph.NodeColor.BFUNCTION",
            Color.GREEN);
        config.put(
            "gui.FunctionProcedureCallGraph.NodeColor.MetaWSL_PROCEDURE",
            Color.CYAN);
        config.put("gui.FunctionProcedureCallGraph.NodeColor.MetaWSL_FUNCTION",
            Color.CYAN);
        config.put(
            "gui.FunctionProcedureCallGraph.NodeColor.MetaWSL_BFUNCTION",
            Color.CYAN);
        config.put(
            "gui.FunctionProcedureCallGraph.NodeColor.External_PROCEDURE",
            Color.GRAY);
        config.put(
            "gui.FunctionProcedureCallGraph.NodeColor.External_FUNCTION",
            Color.GRAY);
        config.put(
            "gui.FunctionProcedureCallGraph.NodeColor.External_BFUNCTION",
            Color.GRAY);
        config.put(
            "gui.FunctionProcedureCallGraph.NodeColor.External_A_PROCEDURE",
            Color.DARK_GRAY);

        config.put("fme.components.console.ConsoleObserver", "default");

        // Parse config XML file to override
        // =================================

        if (new File("fme-config.xml").exists()) {

            try {
                XMLReader xmlReader = XMLReaderFactory.createXMLReader();
                DefaultHandler handler = new ConfigFileParser();
                xmlReader.setContentHandler(handler);
                xmlReader.parse("fme-config.xml");
            } catch (SAXException e) {
                Logger.getLogger(this.getClass().getCanonicalName()).log(
                    Level.SEVERE, "SAX: XML Parser Error");
            } catch (IOException e) {
                Logger.getLogger(this.getClass().getCanonicalName()).log(
                    Level.SEVERE, "SAX: IO Error");
            }
        }
        
        // Set Version of FME
        // ==================
        config.put("version", "0.1");

        // Platform dependent values
        // =========================
        if (!System.getProperty("os.name").toLowerCase().contains("win")) {
            config.put("ConsoleCommand", "engine/fermat_console.sh noecho");
            config.put("ExecutionCommand", "engine/exec.sh noecho");
            config.put("MetricsCommand", "engine/metrics.sh noecho");
        } else {
            config.put("ConsoleCommand", "engine/fermat_console.bat noecho");
            config.put("ExecutionCommand", "engine/exec.bat noecho");
            config.put("MetricsCommand", "engine/metrics.bat noecho");
        }

        // Output config
        // =============
        i = config.keySet().iterator();
        while (i.hasNext()) {
            key = i.next();
            Logger.getLogger(CM.class.getCanonicalName()).log(Level.INFO,
                "Config: " + key + " -> " + config.get(key));
        }
    }

    /**
     * Gets the current configuration
     * 
     * @return Contains all configurationparameters
     */
    public static HashMap<String, Object> getConfig() {
        return singleton.config;
    }

    /**
     * Gets a value
     * 
     * @return The value
     */
    public static Object get(String key) {
        return singleton.config.get(key);
    }

    /**
     * Gets a value
     * 
     * @return The value
     */
    public static int getAsInt(String key) {
        return ((Integer) singleton.config.get(key)).intValue();
    }

    /**
     * Gets a value
     * 
     * @return The value
     */
    public static Color getAsColor(String key) {
        return ((Color) singleton.config.get(key));
    }

    /**
     * Gets a value
     * 
     * @return The value
     */
    public static String getAsString(String key) {
        return (String) singleton.config.get(key);
    }

    // Internal Class
    // ==============

    private class ConfigFileParser extends DefaultHandler {

        public ConfigFileParser() {
        }

        public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
            try {
                if (qName.equals("Config")) {

                    if (attributes.getValue(1).equals("String")) {
                        config.put(attributes.getValue(0), attributes
                            .getValue(2));
                    } else if (attributes.getValue(1).equals("Integer")) {
                        config.put(attributes.getValue(0), Integer
                            .parseInt(attributes.getValue(2)));
                    } else if (attributes.getValue(1).equals("Color")) {
                        int r, g, b;
                        r = Integer
                            .parseInt(attributes.getValue(2).split("#")[0]);
                        g = Integer
                            .parseInt(attributes.getValue(2).split("#")[1]);
                        b = Integer
                            .parseInt(attributes.getValue(2).split("#")[2]);
                        config.put(attributes.getValue(0), new Color(r, g, b));
                    }
                }
            } catch (Exception ex) {
                Logger.getLogger(CM.class.getCanonicalName()).log(
                    Level.WARNING,
                    "Couldn't read config:" + attributes.getValue(1));
            }
        }
    }
}
