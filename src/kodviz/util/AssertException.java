/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.util;


/**
 * Exception used to indicate an {@link Dbg#chk assertion failure}.
 * Assertion failures throw exceptions, rather than calling {@link System#exit},
 * to allow us to contain failures and continue running even if one command
 * fails.
 * <p>
 * Note that {@link AssertException} extends {@link RuntimeException}, so it
 * does not have to be explicitly declared/caught.  However, if you plan to
 * recover from an {@link AssertException}, you need to make sure that
 * any system resources get properly released in case of the exception.
 *
 * @see Dbg
 * @see EnvFailureException
 */
public class AssertException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	/**
     * An exception that indicated the original error.
     * May be <code>null</code> if the original assertion did not include an exception.
     */
    private Throwable _thrown;
    
    public AssertException() { }
    public AssertException(String msg_) { super(msg_); }
    public AssertException(String msg_, Throwable thrown_) { this(msg_); _thrown = thrown_; }

    /**
     * @return the exception that indicated the original error.
     *         May be <code>null</code> if the original assertion did not include an exception.
     */
    public Throwable getThrown() { return _thrown; }

    public String toString() {
	return super.toString() + " " + _thrown;
    }
}


