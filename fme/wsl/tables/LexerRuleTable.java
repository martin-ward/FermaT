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
public class LexerRuleTable {

	/**
	 * The default WSL grammar file
	 */
	private static String defaultLexerTable = "engine/wsl_analysing/lexer_table.xml";

	/**
	 * Number of checks in the lexer table
	 */
	private static int size = 0;

	/**
	 * The grammar as list
	 */
	private static Vector<Vector<String>> lexerTable;

	/**
	 * The grammar as lookup table (via syntax names)
	 */
	private static HashMap<String, Vector<String>> lexerCheckNumberLevelLookup;

	/**
	 * The grammar as lookup table (via id numbers)
	 */
	private static HashMap<Integer, Vector<String>> lexerCheckNumberLookup;

	/**
	 * Parse the WSL grammer from the default file
	 */
	public static void parseLexerTable() {
		parseLexerTable(defaultLexerTable);
	}

	/**
	 * Parse the WSL grammer from a specified file
	 * 
	 * @param filename
	 *            The WSL grammar file
	 */
	public static void parseLexerTable(String filename) {

		lexerTable = new Vector<Vector<String>>();
		lexerCheckNumberLevelLookup = new HashMap<String, Vector<String>>();
		lexerCheckNumberLookup = new HashMap<Integer, Vector<String>>();

		System.setProperty("org.xml.sax.driver",
				"org.apache.xerces.parsers.SAXParser");

		try {
			XMLReader xmlReader = XMLReaderFactory.createXMLReader();
			DefaultHandler handler = new LexerTableParser(lexerTable,
					lexerCheckNumberLookup, lexerCheckNumberLevelLookup);
			xmlReader.setContentHandler(handler);
			xmlReader.parse(new File(filename).toURI().toURL().toString());
		} catch (SAXException e) {
			Logger.getLogger(LexerRuleTable.class.getCanonicalName()).log(Level.SEVERE,
					"SAX: XML Parser Error");
		} catch (IOException e) {
			Logger.getLogger(LexerRuleTable.class.getClass().getCanonicalName()).log(Level.SEVERE,
					"SAX: IO Error");
		}
	}

	/**
	 * Get the lexer table
	 * 
	 * @return The lexer table
	 */
	public static Vector<Vector<String>> getLexerTable() {
		return lexerTable;
	}

	/**
	 * Get the size of the table
	 * 
	 * @return The size
	 */
	public static int getSize() {
		return size;
	}

	/**
	 * Get a check on a specific level (e.g. 12.0.2 will get check 12 on level
	 * 0.2)
	 * 
	 * @return The check of this number on level 0
	 */
	public static Vector<String> getLexerCheck(String syntaxName) {
		return lexerCheckNumberLevelLookup.get(syntaxName);
	}

	/**
	 * Get a check (level 0)
	 * 
	 * @return The check of this number on level 0
	 */
	public static Vector<String> getLexerFirstCheck(int id) {
		return lexerCheckNumberLookup.get(new Integer(id));
	}

	// Internal Classes
	// ================

	private static class LexerTableParser extends DefaultHandler {

		private Vector<Vector<String>> lexerTable;

		private HashMap<String, Vector<String>> lexerCheckNumberLevelLookup;

		private HashMap<Integer, Vector<String>> lexerCheckNumberLookup;

		public LexerTableParser(Vector<Vector<String>> lexerTable,
				HashMap<Integer, Vector<String>> lexerCheckNumberLookup,
				HashMap<String, Vector<String>> lexerCheckNumberLevelLookup) {
			this.lexerTable = lexerTable;
			this.lexerCheckNumberLookup = lexerCheckNumberLookup;
			this.lexerCheckNumberLevelLookup = lexerCheckNumberLevelLookup;
		}

		public void startElement(String uri, String localName, String qName,
				Attributes attributes) throws SAXException {
			Vector<String> tupel;
			if (qName.equals("Check")) {

				tupel = new Vector<String>();
				for (int i = 0; i < 9; i++) {
					tupel.add(attributes.getValue(i));
				}
				lexerTable.add(tupel);
				if (attributes.getValue(1).equals("0"))
					lexerCheckNumberLookup.put(new Integer(attributes
							.getValue(0)), tupel);
				lexerCheckNumberLevelLookup.put(attributes.getValue(0) + "."
						+ attributes.getValue(1), tupel);
				if (Integer.parseInt(attributes.getValue(0)) > size)
					size = Integer.parseInt(attributes.getValue(0));
			}
		}
	}
}
