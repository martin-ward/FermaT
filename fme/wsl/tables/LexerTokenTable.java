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
public class LexerTokenTable {

    /**
     * The default WSL grammar file
     */
    private static String defaultLexerTokenTable = "engine/wsl_analysing/lexer_token_table.xml";

    /**
     * The grammar as list
     */
    private static Vector<Vector<String>> lexerTokenTable;

    /**
     * The grammar as lookup table (via syntax names)
     */
    private static HashMap<String, Vector<String>> lexerTokenLookup;

    /**
     * The grammar as lookup table (via group names)
     */
    private static HashMap<String, Vector<String>> lexerTokenGroupLookup;

    /**
     * The grammar as lookup table (via singleChars names)
     */
    private static HashMap<Character, Vector<String>> singleCharLookup;

    /**
     * The grammar as lookup table (via sequenceChars names)
     */
    private static HashMap<String, Vector<String>> sequenceCharLookup;

    /**
     * The grammar as lookup table (via sequenceChars names)
     */
    private static HashMap<Integer, Vector<String>> idLookup;

    /**
     * Parse the WSL grammer from the default file
     */
    public static void parseLexerTable() {
        parseLexerTokenTable(defaultLexerTokenTable);
    }

    /**
     * Parse the WSL grammer from a specified file
     * 
     * @param filename
     *            The WSL grammar file
     */
    public static void parseLexerTokenTable(String filename) {
        lexerTokenTable = new Vector<Vector<String>>();
        lexerTokenLookup = new HashMap<String, Vector<String>>();
        lexerTokenGroupLookup = new HashMap<String, Vector<String>>();
        singleCharLookup = new HashMap<Character, Vector<String>>();
        sequenceCharLookup = new HashMap<String, Vector<String>>();
        idLookup = new HashMap<Integer, Vector<String>>();

        System.setProperty("org.xml.sax.driver",
            "org.apache.xerces.parsers.SAXParser");

        try {
            XMLReader xmlReader = XMLReaderFactory.createXMLReader();
            DefaultHandler handler = new LexerTokenTableParser();
            xmlReader.setContentHandler(handler);
            xmlReader.parse(new File(filename).toURI().toURL().toString());
        } catch (SAXException e) {
            Logger.getLogger(LexerTokenTable.class.getCanonicalName()).log(
                Level.SEVERE, "SAX: XML Parser Error");
        } catch (IOException e) {
            Logger.getLogger(
                LexerTokenTable.class.getClass().getCanonicalName()).log(
                Level.SEVERE, "SAX: IO Error");
        }
    }

    /**
     * Get the lexer table
     * 
     * @return The lexer table
     */
    public static Vector<Vector<String>> getLexerTable() {
        return lexerTokenTable;
    }

    /**
     * Get a lexer token table row
     * 
     * @return The lexer token table row
     */
    public static Vector<String> getLexerToken(String token) {
        Vector<String> ret = lexerTokenLookup.get(token);
        if (ret == null) {
            Logger.getLogger(LexerTokenTable.class.getCanonicalName()).log(
                Level.WARNING,
                "The requested LexerToken " + token + " doesn't exist");
        }
        return ret;
    }

    /**
     * Get a lexer token table row
     * 
     * @return The lexer token table row
     */
    public static Vector<String> getLexerToken(Integer id) {
        Vector<String> ret = idLookup.get(id);
        if (ret == null) {
            Logger.getLogger(LexerTokenTable.class.getCanonicalName()).log(
                Level.WARNING,
                "The requested LexerToken id:" + id + " doesn't exist");
        }
        return ret;
    }

    /**
     * Get a list of all token which are in a group
     * 
     * @return The check of this number on level 0
     */
    public static Vector<String> getLexerTokenGroup(String group) {
        Vector<String> ret = lexerTokenGroupLookup.get(group);
        if (ret == null) {
            Logger.getLogger(LexerTokenTable.class.getCanonicalName()).log(
                Level.WARNING,
                "The requested LexerTokenGroup " + group + " doesn't exist");
        }
        return ret;
    }

    /**
     * Get a token from a single character
     * 
     * @return The check of this number on level 0
     */
    public static Vector<String> getLexerTokenSingleChar(char c) {
        return singleCharLookup.get(Character.valueOf(c));
    }

    /**
     * Get a token from a character sequence
     * 
     * @return The check of this number on level 0
     */
    public static Vector<String> getLexerTokenCharSequence(String c) {
        return sequenceCharLookup.get(c);
    }

    /**
     * Return a tokens string representation
     * 
     * @param token
     *            The token name
     * @return The string representation
     */
    public static String getLexerTokenRepresentation(String token) {
        String t[], ret;
        Vector<String> tt = getLexerToken(token);
        if (tt == null) {
            Logger.getLogger(LexerTokenTable.class.getCanonicalName()).log(
                Level.WARNING, "Can't find representation for token " + token);
            return "";
        }
        if (tt.get(1).equals("ReservedWord")) {
            ret = tt.get(2);
        } else {
            t = tt.get(3).split(";");
            ret = "";
            for (int i = 0; i < t.length; i++) {
                ret += (char) Integer.parseInt(t[i]);
            }
        }
        return ret;
    }

    // Internal Classes
    // ================

    private static class LexerTokenTableParser extends DefaultHandler {

        public LexerTokenTableParser() {
        }

        public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
            Vector<String> tupel, groupNames;
            if (qName.equals("GroupItem")) {

                tupel = new Vector<String>();
                for (int i = 0; i < 5; i++) {
                    tupel.add(attributes.getValue(i));
                }
                lexerTokenTable.add(tupel);

                if ((groupNames = lexerTokenGroupLookup.get(attributes
                    .getValue(1))) != null) {

                    groupNames.add(attributes.getValue(0));

                    if (attributes.getValue(1).equals("CompositeToken")) {
                        sequenceCharLookup.put(attributes.getValue(3), tupel);
                    } else if (attributes.getValue(3).length() != 0) {
                        singleCharLookup.put(Character.valueOf((char) Integer
                            .parseInt(attributes.getValue(3))), tupel);
                    } else if (attributes.getValue(2).length() != 0) {
                        sequenceCharLookup.put(attributes.getValue(2), tupel);
                    }
                } else {
                    groupNames = new Vector<String>();
                    groupNames.add(attributes.getValue(0));
                    if (attributes.getValue(1).equals("CompositeToken")) {
                        sequenceCharLookup.put(attributes.getValue(3), tupel);
                    } else if (attributes.getValue(3).length() != 0) {
                        singleCharLookup.put(Character.valueOf((char) Integer
                            .parseInt(attributes.getValue(3))), tupel);
                    } else if (attributes.getValue(2).length() != 0) {
                        sequenceCharLookup.put(attributes.getValue(2), tupel);
                    }
                    lexerTokenGroupLookup.put(attributes.getValue(1),
                        groupNames);
                }
                lexerTokenLookup.put(attributes.getValue(0), tupel);
                if (!attributes.getValue(4).equals("-1")) {
                    idLookup.put(new Integer(attributes.getValue(4)), tupel);
                }
            }
        }
    }
}
