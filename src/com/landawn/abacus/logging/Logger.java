/**
 * Copyright (c) 2004-2011 QOS.ch
 * All rights reserved.
 *
 * Permission is hereby granted, free  of charge, to any person obtaining
 * a  copy  of this  software  and  associated  documentation files  (the
 * "Software"), to  deal in  the Software without  restriction, including
 * without limitation  the rights to  use, copy, modify,  merge, publish,
 * distribute,  sublicense, and/or sell  copies of  the Software,  and to
 * permit persons to whom the Software  is furnished to do so, subject to
 * the following conditions:
 *
 * The  above  copyright  notice  and  this permission  notice  shall  be
 * included in all copies or substantial portions of the Software.
 *
 * THE  SOFTWARE IS  PROVIDED  "AS  IS", WITHOUT  WARRANTY  OF ANY  KIND,
 * EXPRESS OR  IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE  WARRANTIES OF
 * MERCHANTABILITY,    FITNESS    FOR    A   PARTICULAR    PURPOSE    AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

package com.landawn.abacus.logging;

/**
 * The org.slf4j.Logger interface is the main user entry point of SLF4J API. It is expected that logging takes place
 * through concrete implementations of this interface.
 * 
 * <h3>Typical usage pattern:</h3>
 * 
 * <pre>
 * import org.slf4j.Logger;
 * import org.slf4j.LoggerFactory;
 * 
 * public class Wombat {
 * 
 *   <span style="color:green">final static Logger logger = LoggerFactory.getLogger(Wombat.class);</span>
 *   Integer t;
 *   Integer oldT;
 * 
 *   public void setTemperature(Integer temperature) {
 *     oldT = t;        
 *     t = temperature;
 *     <span style="color:green">logger.debug("Temperature set to {}. Old temperature was {}.", t, oldT);</span>
 *     if(temperature.intValue() > 50) {
 *       <span style="color:green">logger.info("Temperature has risen above 50 degrees.");</span>
 *     }
 *   }
 * }
 * </pre>
 * 
 * 
 * 
 * @author Ceki G&uuml;lc&uuml;
 */
public interface Logger {

    /**
     * Case insensitive String constant used to retrieve the name of the root logger.
     * 
     * @since 1.3
     */
    final public String ROOT_LOGGER_NAME = "ROOT";

    public String getName();

    /**
     * Is the logger instance enabled for the TRACE level?
     * 
     * @return True if this Logger is enabled for the TRACE level, false otherwise.
     * 
     * @since 1.4
     */
    public boolean isTraceEnabled();

    /**
     * Log a message at the TRACE level.
     * 
     * @param msg
     *            the message string to be logged
     * @since 1.4
     */
    public void trace(String msg);

    /**
     * Log a message at the TRACE level according to the specified format and arguments.
     * 
     * <p>
     * This form avoids superfluous object creation when the logger is disabled for the TRACE level.
     * </p>
     * 
     * @param format
     *            the format string
     * @param args
     *            an array of arguments
     * 
     * @since 1.4
     */
    public void trace(String format, Object... args);

    /**
     * Log an exception (throwable) at the TRACE level with an accompanying message.
     * 
     * @param msg
     *            the message accompanying the exception
     * @param t
     *            the exception (throwable) to log
     * 
     * @since 1.4
     */
    public void trace(String msg, Throwable t);

    /**
     * Is the logger instance enabled for the DEBUG level?
     * 
     * @return True if this Logger is enabled for the DEBUG level, false otherwise.
     * 
     */
    public boolean isDebugEnabled();

    /**
     * Log a message at the DEBUG level.
     * 
     * @param msg
     *            the message string to be logged
     */
    public void debug(String msg);

    /**
     * Log a message at the DEBUG level according to the specified format and arguments.
     * 
     * <p>
     * This form avoids superfluous object creation when the logger is disabled for the DEBUG level.
     * </p>
     * 
     * @param format
     *            the format string
     * @param args
     *            an array of arguments
     */
    public void debug(String format, Object... args);

    /**
     * Log an exception (throwable) at the DEBUG level with an accompanying message.
     * 
     * @param msg
     *            the message accompanying the exception
     * @param t
     *            the exception (throwable) to log
     */
    public void debug(String msg, Throwable t);

    /**
     * Is the logger instance enabled for the INFO level?
     * 
     * @return True if this Logger is enabled for the INFO level, false otherwise.
     */
    public boolean isInfoEnabled();

    /**
     * Log a message at the INFO level.
     * 
     * @param msg
     *            the message string to be logged
     */
    public void info(String msg);

    /**
     * Log a message at the INFO level according to the specified format and arguments.
     * 
     * <p>
     * This form avoids superfluous object creation when the logger is disabled for the INFO level.
     * </p>
     * 
     * @param format
     *            the format string
     * @param args
     *            an array of arguments
     */
    public void info(String format, Object... args);

    /**
     * Log an exception (throwable) at the INFO level with an accompanying message.
     * 
     * @param msg
     *            the message accompanying the exception
     * @param t
     *            the exception (throwable) to log
     */
    public void info(String msg, Throwable t);

    /**
     * Is the logger instance enabled for the WARN level?
     * 
     * @return True if this Logger is enabled for the WARN level, false otherwise.
     */
    public boolean isWarnEnabled();

    /**
     * Log a message at the WARN level.
     * 
     * @param msg
     *            the message string to be logged
     */
    public void warn(String msg);

    /**
     * Log a message at the WARN level according to the specified format and arguments.
     * 
     * <p>
     * This form avoids superfluous object creation when the logger is disabled for the WARN level.
     * </p>
     * 
     * @param format
     *            the format string
     * @param args
     *            an array of arguments
     */
    public void warn(String format, Object... args);

    /**
     * Log an exception (throwable) at the WARN level with an accompanying message.
     * 
     * @param msg
     *            the message accompanying the exception
     * @param t
     *            the exception (throwable) to log
     */
    public void warn(String msg, Throwable t);

    /**
     * Is the logger instance enabled for the ERROR level?
     * 
     * @return True if this Logger is enabled for the ERROR level, false otherwise.
     */
    public boolean isErrorEnabled();

    /**
     * Log a message at the ERROR level.
     * 
     * @param msg
     *            the message string to be logged
     */
    public void error(String msg);

    /**
     * Log a message at the ERROR level according to the specified format and arguments.
     * 
     * <p>
     * This form avoids superfluous object creation when the logger is disabled for the ERROR level.
     * </p>
     * 
     * @param format
     *            the format string
     * @param args
     *            an array of arguments
     */
    public void error(String format, Object... args);

    /**
     * Log an exception (throwable) at the ERROR level with an accompanying message.
     * 
     * @param msg
     *            the message accompanying the exception
     * @param t
     *            the exception (throwable) to log
     */
    public void error(String msg, Throwable t);

}
