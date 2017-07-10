/**
 * Project: fuml
 */

package fme.logging;

import java.util.logging.Handler;
import java.util.logging.LogRecord;

/**
 * The global handler for the logging
 * 
 * @author <A href="http://www.ladkau.de" target=newframe>M. Ladkau</A>
 */

public class GlobalHandler
    extends Handler {

  /**
   * (non-Javadoc)
   * 
   * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
   */
  public void publish(LogRecord record) {

    System.out.println("==============");
    System.out.println("GLOBAL LOGGER:");
    System.out.println("==============");
    System.out.println("" + record.getMessage());
    System.out.println("");
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
