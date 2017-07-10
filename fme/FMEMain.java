/**
 * Project: fme
 */

package fme;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import com.digitprop.tonic.TonicLookAndFeel;

import fme.components.ComponentRegistry;
import fme.config.CM;
import fme.gui.MainFrame;
import fme.logging.DefaultHandler;
import fme.logging.FMELogger;
import fme.wsl.tables.LexerRuleTable;
import fme.wsl.tables.LexerTokenTable;
import fme.wsl.tables.WSLTreeGrammarTable;

/**
 * This class is the main class of the FermaT Maintenance Environment
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class FMEMain {

	/**
	 * The Constructor
	 */
	private FMEMain() {
	}

	/**
	 * This is the main class of FME. It executes the necessary starting
	 * operations.
	 * 
	 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
	 */
	public static void main(String[] args) {

		// Enable logging
		FMELogger.enableFMELogging(new DefaultHandler());

		// Set Look and Feel
		LookAndFeelInfo[] lafs = UIManager.getInstalledLookAndFeels();
		try {
			boolean lfFound = false;
			// First try special Look and Feel
			try {
				if (CM.getAsString("Look_and_Feel").equals("Tonic")) {
					UIManager.setLookAndFeel(new TonicLookAndFeel());
					lfFound = true;
				}
			} catch (Exception e) {

			}

			// Then try windows Look and Feel
			if (!lfFound) {
				for (int i = 0; i < lafs.length; i++) {
					if (lafs[i].getName().toLowerCase().contains("windows")) {
						UIManager.setLookAndFeel(lafs[i].getClassName());
						lfFound = true;
						break;
					}
				}
			}
			// Then try motif Look and Feel
			if (!lfFound) {
				for (int i = 0; i < lafs.length; i++) {
					if (lafs[i].getName().toLowerCase().contains("motif")) {
						UIManager.setLookAndFeel(lafs[i].getClassName());
						lfFound = true;
						break;
					}
				}
			}
			if (!lfFound)
				throw new Exception();
		} catch (Exception e) {
			Logger
					.getLogger(FMEMain.class.getCanonicalName())
					.log(Level.WARNING,
							"Can't find a Look&Feel Manager within the JVM on this machine");
		}

		Logger.getLogger(FMEMain.class.getCanonicalName()).log(Level.INFO,
				"Parsing WSL definition ...");
		// Parsing the WSL defs
		WSLTreeGrammarTable.parseWSLGrammer();
		LexerRuleTable.parseLexerTable();
		LexerTokenTable.parseLexerTable();

		// Show the main frame (start the FME)
		MainFrame.getMainFrame().setVisible(true);

		// Register the components
		ComponentRegistry.register();

		// Start the components
		ComponentRegistry.startup();

		// Show the components
		MainFrame.getMainFrame().showComponents();

		try {
			Thread.sleep(2000);
		} catch (InterruptedException ex) {
		}
	}
}
