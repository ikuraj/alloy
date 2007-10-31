/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * Mutable; this implements a graph with nodes and directed edges; null node is allowed.
 *
 * <p> Note: it uses n1==n2 for comparing nodes rather than using n1.equals(n2)
 *
 * <p><b>Thread Safety:</b>  unsafe
 *
 * @param <N> - the node type
 */

public final class DirectedGraph<N> {

    /** This field maps each node X to a list of "neighbor nodes" that X can reach by following one or more directed edges. */
    private final IdentityHashMap<N,List<N>> nodeToTargets = new IdentityHashMap<N,List<N>>();

    /** Constructs an empty graph. */
    public DirectedGraph () { }

    /**
     * Adds a directed edge from start node to end node (if there wasn't such an edge already).
     */
    public void addEdge (final N start, final N end) {
        List<N> targets = nodeToTargets.get(start);
        if (targets == null) {
            targets = new ArrayList<N>();
            nodeToTargets.put(start, targets);
        } else {
            for (int i=targets.size()-1; i>=0; i--) {
                if (targets.get(i) == end) { return; }
            }
        }
        targets.add(end);
    }

    /**
     * Returns whether there is a directed path from start node to end node by following 0 or more directed edges.
     */
    public boolean hasPath (final N start, final N end) {
        if (start == end) { return true; }
        List<N> todo = new ArrayList<N>();
        IdentityHashMap<N,Object> visited = new IdentityHashMap<N,Object>();
        // The correctness and guaranteed termination relies on four invariants:
        // (1) Nothing is ever removed from "visited".
        // (2) Every time we add X to "visited", we also simultaneously add X to "todo".
        // (3) Every time we add X to "todo", we also simultaneously add X to "visited".
        // (4) Nothing is added to "todo" more than once
        visited.put(start, null);
        todo.add(start);
        while(!todo.isEmpty()) {
            List<N> targets = nodeToTargets.get(todo.remove(todo.size()-1));
            if (targets!=null) {
                for (int i=targets.size()-1; i>=0; i--) {
                    N next=targets.get(i);
                    if (next == end) {
                        addEdge(start, end); // This caches the result, so that hasPath(start,end) will return true immediately
                        return true;
                    }
                    if (!visited.containsKey(next)) {
                        visited.put(next, null);
                        todo.add(next);
                    }
                }
            }
        }
        return false;
    }
}
