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

import java.util.List;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Map;

/** Mutable; this implements a graph with nodes and directed edges; null node is allowed.
 *
 * <p> Note: it uses n1==n2 for comparing nodes rather than using n1.equals(n2)
 *
 * @param <N> - the node type
 */

public final class DirectedGraph<N> {

   /** This field maps each node X to a list of "neighbor nodes" that X can reach by following one or more directed edges. */
   private final Map<N,List<N>> nodeToTargets = new IdentityHashMap<N,List<N>>();

   /** Constructs an empty graph. */
   public DirectedGraph () { }

   /** Add a directed edge from start node to end node (if there wasn't such an edge already). */
   public void addEdge (final N start, final N end) {
      List<N> targets = nodeToTargets.get(start);
      if (targets == null) {
         targets = new ArrayList<N>();
         targets.add(end);
         nodeToTargets.put(start, targets);
      } else {
         for (int i = targets.size()-1; i >= 0; i--) if (targets.get(i) == end) return;
         targets.add(end);
      }
   }

   /** Returns whether there is a directed path from start node to end node by following 0 or more directed edges (breath-first). */
   public boolean hasPath (final N start, final N end) {
      if (start == end) return true;
      List<N> todo = new ArrayList<N>();
      Map<N,N> visited = new IdentityHashMap<N,N>();
      // The correctness and guaranteed termination relies on following three invariants:
      // (1) Every time we add X to "visited", we also simultaneously add X to "todo".
      // (2) Every time we add X to "todo", we also simultaneously add X to "visited".
      // (3) Nothing is ever removed.
      visited.put(start, start);
      todo.add(start);
      for(int k = 0; k < todo.size(); k++) {
         List<N> targets = nodeToTargets.get(todo.get(k));
         if (targets == null) continue;
         for (int i = targets.size()-1; i >= 0; i--) {
            N next = targets.get(i);
            if (next == end) { addEdge(start, end); return true; } // Cache so that hasPath(start,end) returns true immediately
            if (!visited.containsKey(next)) { visited.put(next, next); todo.add(next); }
         }
      }
      return false;
   }
}
