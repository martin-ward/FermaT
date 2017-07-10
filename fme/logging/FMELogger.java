/**
 * Project: fuml
 */

package fme.logging;

import java.io.File;
import java.util.logging.Handler;
import java.util.logging.Logger;

/**
 * This class represents the logging subsystem of FME
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class FMELogger {

	/**
	 * Enables the SVE logging subsystem
	 */
	public static void enableFMELogging(Handler handler) {

		(new File("FME_log.txt")).delete();

		Logger.getLogger(Logger.GLOBAL_LOGGER_NAME).setUseParentHandlers(false);
		Logger.getLogger(Logger.GLOBAL_LOGGER_NAME).addHandler(
				new GlobalHandler());

		Logger.getLogger("fme").setUseParentHandlers(false);
		Logger.getLogger("fme").addHandler(handler);
	}

	/**
	 * Disables the SVE logging subsystem
	 */
	public static void disableFMELogging() {
		Logger.getLogger(Logger.GLOBAL_LOGGER_NAME).setUseParentHandlers(true);
		Logger.getLogger("fme").setUseParentHandlers(true);
	}
}