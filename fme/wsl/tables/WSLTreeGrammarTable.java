/**
 * Project: fme
 */

package fme.wsl.tables;

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

/**
 * This class holds the WSL grammar
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */
public class WSLTreeGrammarTable {

  /**
   * The default WSL grammar file
   */
  private static String defaultWSLGrammer = "engine/wsl_def/tree_grammar.xml";

  /**
   * The grammar as list
   */
  private static Vector<Vector<String>> grammar;

  /**
   * The grammar as lookup table (via syntax names)
   */
  private static HashMap<String, Vector<String>> grammarSyntaxNameLookup;

  /**
   * The grammar as lookup table (via id numbers)
   */
  private static HashMap<Integer, Vector<String>> grammarIDLookup;

  /**
   * Parse the WSL grammer from the default file
   */
  public static void parseWSLGrammer() {
    parseWSLGrammer(defaultWSLGrammer);
  }

  /**
   * Parse the WSL grammer from a specified file
   * 
   * @param filename The WSL grammar file
   */
  public static void parseWSLGrammer(String filename) {

    grammar = new Vector<Vector<String>>();
    grammarSyntaxNameLookup = new HashMap<String, Vector<String>>();
    grammarIDLookup = new HashMap<Integer, Vector<String>>();

    System.setProperty("org.xml.sax.driver",
        "org.apache.xerces.parsers.SAXParser");

    try {
      XMLReader xmlReader = XMLReaderFactory.createXMLReader();
      DefaultHandler handler = new WSLGrammarParser(grammar, grammarIDLookup,
          grammarSyntaxNameLookup);
      xmlReader.setContentHandler(handler);
      xmlReader.parse(new File(filename).toURI().toURL().toString());
    }
    catch (SAXException e) {
      Logger.getLogger(WSLTreeGrammarTable.class.getCanonicalName()).log(Level.SEVERE,
          "SAX: XML Parser Error");
    }
    catch (IOException e) {
      Logger.getLogger(WSLTreeGrammarTable.class.getCanonicalName()).log(Level.SEVERE,
          "SAX: IO Error");
    }
  }

  /**
   * Get the grammar as list
   * 
   * @return The grammar
   */
  public static Vector<Vector<String>> getGrammar() {
    return grammar;
  }

  /**
   * Get the grammar as lookup table (via syntax names)
   * 
   * @return The grammar
   */
  public static Vector<String> getGrammarOfNode(String syntaxName) {
    return grammarSyntaxNameLookup.get(syntaxName);
  }
  
  /**
   * Get the grammar as lookup table (via id numbers)
   * 
   * @return The grammar
   */
  public static Vector<String> getGrammarOfNode(int id) {
    return grammarIDLookup.get(new Integer(id));
  }

  // Internal Classes
  // ================

  private static class WSLGrammarParser
      extends DefaultHandler {

    private Vector<Vector<String>> grammar;

    private HashMap<String, Vector<String>> grammarSyntaxNameLookup;

    private HashMap<Integer, Vector<String>> grammarIDLookup;

    public WSLGrammarParser(Vector<Vector<String>> grammar,
        HashMap<Integer, Vector<String>> grammarIDLookup,
        HashMap<String, Vector<String>> grammarSyntaxNameLookup) {
      this.grammar = grammar;
      this.grammarIDLookup = grammarIDLookup;
      this.grammarSyntaxNameLookup = grammarSyntaxNameLookup;
    }

    public void startElement(String uri, String localName, String qName,
        Attributes attributes) throws SAXException {
      Vector<String> tupel;
      if (qName.equals("ASTNode")) {

        tupel = new Vector<String>();
        for (int i = 0; i < 7; i++) {
          tupel.add(attributes.getValue(i));
        }
        grammar.add(tupel);
        grammarIDLookup.put(new Integer(attributes.getValue(0)),tupel);
        grammarSyntaxNameLookup.put(attributes.getValue(2), tupel);
      }
    }
  }
}
