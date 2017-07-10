/**
 * Project: fme
 */

package fme.components.console;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowStateListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import fme.config.CM;
import fme.gui.MainFrame;

/**
 * This class observes the console. It receives and sends commands. The class
 * provides a low level interface to the console
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class ExecutionConsole extends JFrame implements Runnable,
		ActionListener {

	private static final long serialVersionUID = 1L;

	private static final boolean messageLogging = false;

	/**
	 * The command which should be executed
	 */
	private String execCommand;

	/**
	 * The background process for the engine
	 */
	private Process consoleProc;

	/**
	 * The text area of the graphical console
	 */
	private JTextArea graphicConsole;

	/**
	 * The text field for input of the graphical console
	 */
	private JTextField graphicTextField;

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
	private ExecutionConsole() {
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

		Logger.getLogger(this.getClass().getCanonicalName()).log(
				Level.INFO, "Starting console ...");

		try {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.INFO, "Executing " + execCommand);

			consoleProc = Runtime.getRuntime().exec(execCommand);

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
					Thread.sleep(100);
				} catch (InterruptedException e) {
				}
			}

		} catch (IOException e) {
			Logger.getLogger(this.getClass().getCanonicalName()).log(
					Level.SEVERE, "IOException in the pipe");
		}

		Logger.getLogger(this.getClass().getCanonicalName()).log(
				Level.INFO, "Ending console Thread");
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
				Logger.getLogger(this.getClass().getCanonicalName())
						.log(Level.INFO, "Sending :\"" + cmd + "\"");
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
	 * 
	 * @param cmd
	 *            The command to Execute
	 * 
	 * @return An instance of this class
	 */
	public static ExecutionConsole start(String cmd, String title) {
		ExecutionConsole console = new ExecutionConsole();

		console.addWindowStateListener(new WindowStateListener() {
			public void windowStateChanged(WindowEvent e) {
				((JFrame) e.getSource()).dispose();
			}
		});

		console.setTitle(title);
		console.graphicConsole = new JTextArea();
		console.graphicConsole.setEditable(false);
		console.graphicTextField = new JTextField();
		console.graphicTextField.addActionListener(console);
		console.getContentPane().setLayout(new BorderLayout());
		console.getContentPane().add(new JScrollPane(console.graphicConsole),
				BorderLayout.CENTER);
		console.getContentPane().add(console.graphicTextField,
				BorderLayout.SOUTH);

		console.setLocation(MainFrame.getMainFrame().getX() + 50, MainFrame
				.getMainFrame().getY() + 50);
		console.setSize(640, 480);

		console.setVisible(true);
		console.execCommand = cmd;
		new Thread(console).start();
		return console;
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

	/**
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		graphicTextField.setText("");
        graphicConsole.append(e.getActionCommand()+"\n");
        
		if (!System.getProperty("os.name").toLowerCase().contains("win"))
			sendCommand(e.getActionCommand() + "\n");
		else
			sendCommand(e.getActionCommand() + "\r\n");
	}
}
