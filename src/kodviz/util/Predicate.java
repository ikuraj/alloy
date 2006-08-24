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
 * Interface for defining an arbitrary predicate on {@link Object}s.
 */
public abstract class Predicate {
    /** Test whether an {@link Object} satisfies this {@link Predicate} */
    public abstract boolean test(Object obj_);

    /** Return a predicate that is a negation of this predicate */
    Predicate not() {
	final Predicate originalPredicate = this;
	return new Predicate() {
		public boolean test(Object obj_) { return !originalPredicate.test(obj_); }
	    };
    }

    /** Return a predicate that is a conjunction of this predicate and another predicate */
    Predicate and(final Predicate conjunct_) {
	final Predicate originalPredicate = this;
	return new Predicate() {
		public boolean test(Object obj_) {
		    return originalPredicate.test(obj_) && conjunct_.test(obj_);
		}
	    };
    }

    /** Return a predicate that is a conjunction of this predicate and another predicate */
    Predicate or(final Predicate disjunct_) {
	final Predicate originalPredicate = this;
	return new Predicate() {
		public boolean test(Object obj_) {
		    return originalPredicate.test(obj_) || disjunct_.test(obj_);
		}
	    };
    }
}  // class Predicate

