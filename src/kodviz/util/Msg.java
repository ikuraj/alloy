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

/*
 * Alloy Analyzer
 * Copyright (c) 2001
 * Massachusetts Institute of Technology
 *
 * Class: Msg
 * owner: msridhar
 * Interface status: stable
 * Implementation status: stable
 *
 * a wrapper for a String message and a source {@link Location}
 */

public class Msg {

    public String message;

    /** allow no location to be specified */
    public Msg(String message_) {
    	message = message_;
    }

    public String getMessage () {
        return message;
    }

    public String toString() {
        return message;
    }
}

