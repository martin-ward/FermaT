package fme.components;

import java.awt.BorderLayout;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;

/**
 * This class manages basic I/O operations
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 * 
 */
public class IOManager {

	/**
	 * Copy from one stream into another
	 * 
	 * @param fis
	 *            Source stream
	 * @param fos
	 *            Destination stream
	 */
	public static void copy(InputStream fis, OutputStream fos) {
		try {
			byte[] buffer = new byte[0xFFFF];
			for (int len; (len = fis.read(buffer)) != -1;)
				fos.write(buffer, 0, len);
		} catch (IOException e) {
			System.err.println(e);
		} finally {
			if (fis != null)
				try {
					fis.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			if (fos != null)
				try {
					fos.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
		}
	}

	/**
	 * Copy a file
	 * 
	 * @param fis
	 *            Source file
	 * @param fos
	 *            Destination file
	 */
	public static void copyFile(String src, String dest) {
		try {
			FileInputStream in = new FileInputStream(src);
			FileOutputStream out = new FileOutputStream(dest);
			copy(in, out);
			in.close();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Copy a file
	 * 
	 * @param fis
	 *            Source file
	 * @param fos
	 *            Destination file
	 */
	public static void copyFile(File src, File dest) {
		try {
			FileInputStream in = new FileInputStream(src);
			FileOutputStream out = new FileOutputStream(dest);
			copy(in, out);
			in.close();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Move a file
	 * 
	 * @param fis
	 *            Source file
	 * @param fos
	 *            Destination file
	 */
	public static void moveFile(String src, String dest) {
		try {
			copy(new FileInputStream(src), new FileOutputStream(dest));
			new File(src).delete();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Move a file
	 * 
	 * @param fis
	 *            Source file
	 * @param fos
	 *            Destination file
	 */
	public static void moveFile(File src, File dest) {
		try {
			FileInputStream in = new FileInputStream(src);
			FileOutputStream out = new FileOutputStream(dest);
			copy(in, out);
			in.close();
			out.close();
			src.delete();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Extends the given filename with ending <filename>.wsl with
	 * <filename>-0.wsl, <filename>-1.wsl ,... The new filename does not exist
	 * 
	 * @param filename
	 *            The filename to extend
	 * @return The extendet filename
	 */
	public static String extendWSLFileName(String filename) {
		String t, file;
		int i;

		// Construct a new file name
		if (filename.matches(".*-[0-9]+.wsl")) {
			t = filename.substring(filename.lastIndexOf("-") + 1, filename
					.length() - 4);
			file = filename.substring(0, filename.lastIndexOf("-")) + "-"
					+ (Integer.parseInt(t) + 1) + ".wsl";
		} else {
			file = filename.substring(0, filename.length() - 4) + "-0.wsl";
		}

		// Make sure the new file does not exist
		t = file.substring(file.lastIndexOf("-") + 1, file
				.length() - 4);
		i = 1;
		while (new File(file).exists()) {
			file = file.substring(0, file.lastIndexOf("-")) + "-"
					+ (Integer.parseInt(t) + i) + ".wsl";
			i++;
		}

		return file;
	}

	/**
	 * Extends the given filename with ending <filename>.wsl with
	 * <filename>-0.wsl, <filename>-1.wsl ,...
	 * 
	 * @param filename
	 *            The filename to extend
	 * @return The extendet filename
	 */
	public static File extendWSLFileName(File filename) {
		return new File(extendWSLFileName(filename.getAbsolutePath()));
	}

	/**
	 * Save a String to a File
	 * 
	 * @return true if everything is ok / false otherwise
	 */
	public static boolean saveStringToFile(File file, String text) {
		try {
			BufferedWriter out = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(file)));
			out.write(text);
			out.close();
		} catch (IOException ex) {
			return false;
		}
		return true;
	}

	/**
	 * Save a String to a File
	 * 
	 * @return true if everything is ok / false otherwise
	 */
	public static boolean saveStringToFile(String file, String text) {
		return saveStringToFile(new File(file), text);
	}

	/**
	 * Read the first line of a File
	 * 
	 * @return The first line of a File
	 */
	public static String readFirstLine(File file) {
		String ret;

		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					new FileInputStream(file)));
			ret = in.readLine();
			in.close();
		} catch (IOException ex) {
			return null;
		}
		return ret;
	}

	/**
	 * Read the file
	 * 
	 * @return The content of the file as string
	 */
	public static String readFile(File file) {
		StringBuffer ret = new StringBuffer();
		String line;
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					new FileInputStream(file)));
			while ((line = in.readLine()) != null) {
				ret.append(line + "\n");
			}
			in.close();
		} catch (IOException ex) {
			return null;
		}
		return ret.toString();
	}

	private static JDialog progressDialog;

	private static JProgressBar progressBar;

	/**
	 * Shows a progress dialog
	 * 
	 * @param overall
	 *            The total steps this progrss dialog should be able to display
	 */
	public static void showProgressDialog(JFrame c, int overall) {
		if (progressDialog == null) {
			progressDialog = new JDialog(c, "Progress");
			progressBar = new JProgressBar();
			progressDialog.getContentPane().setLayout(new BorderLayout());
			progressDialog.getContentPane().add(progressBar);
			progressDialog.getContentPane().add(new JLabel(" "),
					BorderLayout.NORTH);
			progressDialog.getContentPane().add(new JLabel(" "),
					BorderLayout.SOUTH);
			progressDialog.setSize(200, 70);
		}
		progressBar.setMinimum(0);
		progressBar.setMaximum(overall);
		progressBar.setValue(0);
		progressDialog.setLocation(c.getX()
				+ Math.round(Math.round(c.getWidth() / 2)) - 100, c.getY()
				+ Math.round(Math.round(c.getHeight() / 2)) - 35);
		progressBar.revalidate();
		progressDialog.setVisible(true);
	}

	/**
	 * Adds a tick to the progress dialog
	 */
	public static void tickProgressDialog() {
		if (progressDialog.isVisible()
				&& progressBar.getValue() != progressBar.getMaximum()) {
			progressBar.setValue(progressBar.getValue() + 1);
			progressBar.repaint();
		}
	}

	/**
	 * Hide the progress dialog
	 */
	public static void hideProgressDialog() {
		progressDialog.setVisible(false);
	}

	/**
	 * Check if the progress dialog is visible
	 * 
	 * @return True if the dialog is visible / false otherwise
	 */
	public static boolean isProgressDialogVisible() {
		return progressDialog.isVisible();
	}
}
