/**
 * Project: fuml
 */

package fme.logging;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.logging.Handler;
import java.util.logging.LogRecord;

/**
 * The default handler for the logging
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class NullHandler
    extends Handler {

  /**
   * (non-Javadoc)
   * 
   * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
   */
  public void publish(LogRecord record) {

    String logMSG = record.getLevel().getName() + " " + record.getLoggerName()
        + "." + record.getSourceMethodName() + "(): " + record.getMessage();
 
    // Write log message to file
    try {
      BufferedWriter out = new BufferedWriter(new FileWriter("FME_log.txt",
          true));
      out.write(logMSG + "\r\n");
      out.close();
    }
    catch (Exception e) {
      System.out.println("Logging system failure ...");
      e.printStackTrace();
    }
  }

  /**
   * (non-Javadoc)
   * 
   * @see java.util.logging.Handler#flush()
   */
  public void flush() {
  }

  /**
   * (non-Javadoc)
   * 
   * @see java.util.logging.Handler#close()
   */
  public void close() {
  }
}
