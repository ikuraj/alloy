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
 * An exception indicating the failure of some operation due to
 * correctable environmental conditions -- e.g. out of memory,
 * out of disk space, directory not writeable, etc.  This is
 * different from {@link AssertException}, which indicates
 * an internal program error for which we should post
 * a bug report.
 *
 * @see AssertException
 */
public class EnvFailureException extends Exception {

	private static final long serialVersionUID = 1L;

	/**
     * An exception that indicated the original error.
     * May be <code>null</code> if the original assertion did not include an exception.
     */
    private Throwable _thrown;
    
    public EnvFailureException() { }
    public EnvFailureException(String msg_) { super(msg_); }
    public EnvFailureException(String msg_, Throwable thrown_) { this(msg_); _thrown = thrown_; }

    /**
     * @return the exception that indicated the original error.
     *         May be <code>null</code> if the original assertion did not include an exception.
     */
    public Throwable getThrown() { return _thrown; }

    public String toString() {
	return super.toString() + " " + _thrown;
    }
}  // class EnvFailureException



    
