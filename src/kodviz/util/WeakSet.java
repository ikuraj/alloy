/*
 * Alloy Analyzer
 * Copyright (c) 2003 Massachusetts Institute of Technology
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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

// WeakSet is a wrapper for a set with weak references to the objects it contains.

@SuppressWarnings("unchecked")
public class WeakSet /* implements Set */{
    final private Set mySet;

    public WeakSet() {
        mySet = new HashSet();
    }

    public void add(final Object o) {
        for (Iterator iter = mySet.iterator(); iter.hasNext();) {
            WeakReference ref = (WeakReference) iter.next();
            Object obj = ref.get();
            if (obj == null) {
                iter.remove();
            } else if (o.equals(obj)) {
                return;
            }
        }
        mySet.add(new WeakReference(o));
    }

    public void remove(final Object o) {
        for (Iterator iter = mySet.iterator(); iter.hasNext();) {
            WeakReference ref = (WeakReference) iter.next();
            Object obj = ref.get();
            if (obj == null || o.equals(obj)) {
                iter.remove();
            }
        }
    }

    public Iterator iterator() {
        return new Iterator() {
            ArrayList list = new ArrayList(mySet);

            int i = 0;

            public Object next() {
                WeakReference ref = (WeakReference) list.get(i);
                i++;
                return ref.get();
            }

            public boolean hasNext() {
                while (i < list.size()) {
                    if (((WeakReference) list.get(i)).get() != null) {
                        return true;
                    }
                    i++;
                }
                return false;
            }

            public void remove() {
            }
        };
    }
}