/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

/**
 * Immutable; represents a syntax error that should be reported to the user.
 *
 * <p><b>Invariant:</b> pos!=null && msg!=null
 */

public final class ErrorSyntax extends Err {

    /** This silences javac's warning about missing serialVersionUID. */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new syntax error.
     * @param msg - the actual error message
     */
    public ErrorSyntax(String msg) {
        super(null, msg, null);
    }

    /**
     * Constructs a new syntax error with "cause" as the underlying cause.
     * @param msg - the actual error message
     * @param cause - if nonnull, it is the cause of this exception
     */
    public ErrorSyntax(String msg, Throwable cause) {
        super(null, msg, cause);
    }

    /**
     * Constructs a new syntax error.
     * @param pos - the filename/line/row information (can be null if unknown)
     * @param msg - the actual error message
     */
    public ErrorSyntax(Pos pos, String msg) {
        super(pos, msg, null);
    }

    /** Two Err objects are equal if the type, position, and message are the same. */
    @Override public boolean equals(Object other) {
        if (this==other) return true;
        if (!(other instanceof ErrorSyntax)) return false;
        Err that = (Err) other;
        return pos.equals(that.pos) && msg.equals(that.msg);
    }

    /** Returns a hash code consistent with equals() */
    @Override public int hashCode() {
        return msg.hashCode();
    }

    /** Returns a textual description of the error. */
    @Override public String toString() {
        if (pos==Pos.UNKNOWN) {
            return "Syntax error:\n"+msg;
        }
        if (pos.filename.length()>0) {
            return "Syntax error in "+pos.filename+" at line "+pos.y+" column "+pos.x+":\n"+msg;
        }
        return "Syntax error at line "+pos.y+" column "+pos.x+":\n"+msg;
    }
}
