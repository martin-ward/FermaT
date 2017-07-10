package fme.components.console;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;

import javax.swing.JTextArea;

import fme.components.ComponentRegistry;
import fme.config.CM;

public class ConsoleObserver2 extends ConsoleObserver {

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
	 * The Constructor
	 */
	protected ConsoleObserver2() {
		sendToBuffer = false;
		busy = false;
	}

	/* (non-Javadoc)
	 * @see fme.components.console.ConsoleObserver#run()
	 */
	public void run() {

		int d;
		boolean end = false, update = false;

		graphicConsole = ((ConsoleGUI) ComponentRegistry.getGUIComponent("ConsoleGUI")).getTextArea();

		try {
			consoleProc = Runtime.getRuntime().exec(CM.getAsString("ConsoleCommand"));

			in = consoleProc.getInputStream();
			lineIn = new BufferedReader(new InputStreamReader(in));
			err = consoleProc.getErrorStream();
			lineErr = new BufferedReader(new InputStreamReader(err));
			out = consoleProc.getOutputStream();
			lineOut = new BufferedWriter(new PrintWriter(out));

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
						if (sendToBuffer)
							buffer.append("" + (char) d);
						else
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
						buffer.append(CM.ls + "==ERROR Stream=================" + CM.ls);
					else
						graphicConsole.append(CM.ls + "==ERROR Stream=================" + CM.ls);
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
						buffer.append(CM.ls + "===============================" + CM.ls);
					else
						graphicConsole.append(CM.ls + "===============================" + CM.ls);

					busy = false;
					update = true;
				}

				// Update the caret if necessary
				if (update) {
					graphicConsole.setCaretPosition(graphicConsole.getText().length());
					update = false;
				}

				try {
					Thread.sleep(1);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Sends a string to the console
	 * 
	 * @param cmd
	 *            The string to send
	 */
	public void sendCommand(String cmd) {
		try {
			lineOut.write(cmd);
			lineOut.flush();
		} catch (IOException e) {
			e.printStackTrace();
		}
		busy = true;
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