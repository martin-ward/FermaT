/**
 * Project: fme
 */

package fme.components.console;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JTextArea;

import fme.components.ComponentRegistry;
import fme.config.CM;

/**
 * This class observes the console. It receives and sends commands. The class
 * provides a low level interface to the console
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class ConsoleObserver implements Runnable {

	/**
	 * Log all sended messages in the logging output
	 */
	private static final boolean messageLogging = false;

	/**
	 * The wait time for a reply
	 */
	private static int waitForReply = 100;

	/**
	 * The only instance of this class (singleton design pattern)
	 */
	private static ConsoleObserver singleton;

	/**
	 * The background process for the engine
	 */
	private Process consoleProc;

	/**
	 * The text area of the graphical console
	 */
	private JTextArea graphicConsole;

	// The different streams

	/**
	 * The output stream of the engine
	 */
	private InputStream in, err;

	/**
	 * The input stream of the engine
	 */
	private OutputStream out;

	/**
	 * The buffered output stream of the engine
	 */
	private BufferedReader lineIn, lineErr;

	/**
	 * The buffered input stream of the engine
	 */
	private BufferedWriter lineOut;

	/**
	 * Indicates if the received data should be written to the graphical console
	 * or to an internal buffer
	 */
	private boolean sendToBuffer = false;

	/**
	 * A internal buffer for engine output
	 */
	private StringBuffer buffer;

	/**
	 * A character sequence indicating the expected end of a response
	 */
	private String waitFor;

	/**
	 * Indicates if the engine is writing output
	 */
	private boolean busy;

	/**
	 * Indicates if the engine is writing output
	 */
	private boolean debug = false;

	/**
	 * The Constructor
	 */
	protected ConsoleObserver() {
		sendToBuffer = false;
		busy = false;
	}

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Runnable#run()
	 */
	public void run() {

		int d;
		boolean end = false, update = false;
		String cmd;

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Starting console ...");

		graphicConsole = ((ConsoleGUI) ComponentRegistry
				.getGUIComponent("ConsoleGUI")).getTextArea();

		try {
			Logger.getLogger(this.getClass().getCanonicalName())
					.log(Level.INFO,
							"Executing " + CM.getAsString("ConsoleCommand"));

			consoleProc = Runtime.getRuntime().exec(
					CM.getAsString("ConsoleCommand"));

			in = consoleProc.getInputStream();
			lineIn = new BufferedReader(new InputStreamReader(in));
			err = consoleProc.getErrorStream();
			lineErr = new BufferedReader(new InputStreamReader(err));
			out = consoleProc.getOutputStream();
			lineOut = new BufferedWriter(new PrintWriter(out));

			graphicConsole.append("Initialising FermaT engine ...\n");
			
			// Setting internal variables
			// Necessary for some transformations (e.g. STATIC SLICING)
			cmd = "(define fermat \".\")\n";
			lineOut.write(cmd.replace("\\", "\\\\"));
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.INFO,
					"Initialise engine:"
							+ cmd.replace("\\", "\\\\").replace("\n", ""));
			cmd = "(define perl \"perl\")\n".replace("\\", "\\\\");
			lineOut.write(cmd.replace("\\", "\\\\"));
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.INFO,
					"Initialise engine:"
							+ cmd.replace("\\", "\\\\").replace("\n", ""));
			cmd = "(define ds \"" + File.separator + "\")\n";
			lineOut.write(cmd.replace("\\", "\\\\"));
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.INFO,
					"Initialise engine:"
							+ cmd.replace("\\", "\\\\").replace("\n", ""));
			lineOut.flush();

			d = 0;
			while (!end) {

				// Output standard input
				if (lineIn.ready()) {
					busy = true;
					while (lineIn.ready()) {
						d = lineIn.read();
						if (d == -1) {
							end = true;
							break;
						}
						if (sendToBuffer) {
							buffer.append("" + (char) d);
							if (debug)
								graphicConsole.append("" + (char) d);
						} else
							graphicConsole.append("" + (char) d);
					}
					if (waitFor == null)
						busy = false;
					else {
						if (sendToBuffer && buffer.toString().contains(waitFor))
							busy = false;
						else if (graphicConsole.getText().contains(waitFor))
							busy = false;
					}
					update = true;
				}

				// Output standard error
				if (lineErr.ready()) {
					busy = true;
					if (sendToBuffer)
						buffer.append(CM.ls + "==ERROR Stream================="
								+ CM.ls);
					else
						graphicConsole.append(CM.ls
								+ "==ERROR Stream=================" + CM.ls);
					while (lineErr.ready()) {
						d = lineErr.read();
						if (d == -1) {
							end = true;
							break;
						}
						if (sendToBuffer)
							buffer.append("" + (char) d);
						else
							graphicConsole.append("" + (char) d);
					}
					if (sendToBuffer)
						buffer.append(CM.ls + "==============================="
								+ CM.ls);
					else
						graphicConsole.append(CM.ls
								+ "===============================" + CM.ls);
					busy = false;
					update = true;
				}

				// Update the caret if necessary
				if (update) {
					graphicConsole.setCaretPosition(graphicConsole.getText()
							.length());
					update = false;
				}

				// Sleppe a litte
				try {
					Thread.sleep(waitForReply);
				} catch (InterruptedException e) {
				}
			}

		} catch (IOException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "IOException in the pipe");
		}

		Logger.getLogger(this.getClass().getCanonicalName()).log(Level.INFO,
				"Ending console Thread");
	}

	/**
	 * Sends a string to the console
	 * 
	 * @param cmd
	 *            The string to send
	 */
	public void sendCommand(String cmd) {
		try {
			if (messageLogging)
				Logger.getLogger(this.getClass().getCanonicalName()).log(
						Level.INFO, "Sending :\"" + cmd + "\"");
			lineOut.write(cmd);
			lineOut.flush();
		} catch (IOException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "Can't write command:" + cmd);
		}
		busy = true;
	}

	/**
	 * Starts the console
	 */
	public static void start() {
		if (singleton == null) {
			if (CM.getAsString("fme.components.console.ConsoleObserver")
					.equalsIgnoreCase("default"))
				singleton = new ConsoleObserver();
			else if (CM.getAsString("fme.components.console.ConsoleObserver")
					.equalsIgnoreCase("fast"))
				singleton = new ConsoleObserver2();
			else {
				singleton = new ConsoleObserver();

				Logger.getLogger(singleton.getClass().getCanonicalName())
						.log(Level.SEVERE,
								"Console Observer initialisation failed!");
			}
		}

		new Thread(singleton).start();
	}

	/**
	 * Get the only instance of this class
	 * 
	 * @return The singleton
	 */
	public static ConsoleObserver getConsoleObserver() {
		if (singleton == null) {
			if (CM.getAsString("fme.components.console.ConsoleObserver")
					.equalsIgnoreCase("default")) {
				singleton = new ConsoleObserver();
			} else if (CM.getAsString("fme.components.console.ConsoleObserver")
					.equalsIgnoreCase("fast")) {
				singleton = new ConsoleObserver2();
			} else {
				singleton = new ConsoleObserver();

				Logger.getLogger(singleton.getClass().getCanonicalName())
						.log(Level.SEVERE,
								"Console Observer initialisation failed!");
			}
		}

		return singleton;
	}

	/**
	 * Toggles if the results should be written to the graphical console or to
	 * the back buffer
	 * 
	 * @param sendToBuffer
	 *            True if the output should be written to the back buffer
	 */
	public void setSendToBuffer(boolean sendToBuffer) {
		buffer = new StringBuffer();
		this.sendToBuffer = sendToBuffer;
	}

	/**
	 * Get the back buffer
	 * 
	 * @return The buffer
	 */
	public String getBuffer() {
		return buffer.toString();
	}

	/**
	 * Indicates if the engine is busy with filling its output streams
	 * 
	 * @return True if the engine is busy
	 */
	public boolean isBusy() {
		return busy;
	}

	public void setWaitForReply(int i) {
		waitForReply = i;
	}

	/**
	 * Sets a sequence of characters which indicate the end of an engine's
	 * response
	 * 
	 * @param waitFor
	 *            The sequence of characters
	 */
	public void setWaitForString(String waitFor) {
		this.waitFor = waitFor;
	}
}
