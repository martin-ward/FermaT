/**
 * Project: fme
 */

package fme.components.catalogue;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class Catalogue {

    /**
     * The default catalogue of aviable transformations
     */
    private static String defaultTransformationCatalogue = "engine/wsl_trans/transformation_table.xml";

    public static Catalogue singleton = new Catalogue();

    public static boolean initialised = false;

    private static Vector<String> names = new Vector<String>();

    private static HashMap<String, String> engineName = new HashMap<String, String>();

    private static HashMap<String, String> description = new HashMap<String, String>();

    private static HashMap<String, String> keywords = new HashMap<String, String>();

    private Catalogue() {
    }

    public static void initCatalogue() {
        if (!initialised)
            singleton
                .parseTransformationCatalogue(defaultTransformationCatalogue);
        initialised = true;
    }

    public static Catalogue getInstance() {
        initCatalogue();
        return singleton;
    }

    public HashMap<String, String> getDescription() {
        return description;
    }

    public HashMap<String, String> getEngineName() {
        return engineName;
    }

    public HashMap<String, String> getKeywords() {
        return keywords;
    }

    public Vector<String> getNames() {
        return names;
    }

    // Private Methods
    // ===============

    /**
     * Parse the WSL grammer from a specified file
     * 
     * @param filename
     *            The WSL grammar file
     */
    private void parseTransformationCatalogue(String filename) {

        System.setProperty("org.xml.sax.driver",
            "org.apache.xerces.parsers.SAXParser");

        try {
            XMLReader xmlReader = XMLReaderFactory.createXMLReader();
            DefaultHandler handler = new TransformationCatalogueParser();
            xmlReader.setContentHandler(handler);
            xmlReader.parse(new File(filename).toURI().toURL().toString());
        } catch (SAXException e) {
            Logger.getLogger(this.getClass().getCanonicalName()).log(
                Level.SEVERE, "SAX: XML Parser Error");
        } catch (IOException e) {
            Logger.getLogger(this.getClass().getCanonicalName()).log(
                Level.SEVERE, "SAX: IO Error");
        }
    }

    // Internal Classes
    // ================

    private static class TransformationCatalogueParser extends DefaultHandler {

        public TransformationCatalogueParser() {
        }

        public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
            if (qName.equals("Transformation")) {
                // add the transformation index to the name
                names.add(attributes.getValue(1));
                engineName.put(attributes.getValue(1), attributes.getValue(0));
                description.put(attributes.getValue(1), attributes.getValue(2));
                keywords.put(attributes.getValue(1), attributes.getValue(3));
            }
        }
    }
}
