package kodviz.graph;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.TreeMap;

import kodviz.dotviz.DotOrientation;


/**
 * A Cartoon represents all the possible projections. In addition, it 
 * contains information about font and graph orientation.
 */

public class Cartoon {

    /*
     * A mapping of indexes of each to a certain graph. The keys of the map is
     * an List of Indexers, sorted in ascending order as defined by the Indexer
     * class.  The values are the Graphs associated with a particular List of
     * Indexers. If there are no Indexers (the List is empty), then there is no
     * projection on any type.
     *
     * @specfield indices : Indexer //A list of the Indexers being projected on
     * @specfield assocGraph: Graph //A graph associated with an indices
     */
    private SortedMap indexToGraph;

    private String fontName;

    private int fontSize;

    private DotOrientation orientation;

    /*
     * Rep Invariant:
     *  for all Cartoons C:
     *    all fields != null &&
     *    for all i,j: indices in indexToGraph =>
     *      i!=null && j!=null && i.length = j.length &&
     *      i and j have assocGraph in indexToGraph.
     */

    /**
     * Creates a blank cartoon. Default font size is 14. Default
     * orientation is up.  
     */
    @SuppressWarnings("unchecked")
    public Cartoon() {
        //define new Comparator for list of Indexers
        indexToGraph = new TreeMap(new Comparator() {
            public int compare(Object o1, Object o2) {
                List indices1 = (List)o1;
                List indices2 = (List)o2;
                int length = indices1.size();

                for (int i = 0; i < length; i++) {
                    Indexer i1 = (Indexer)indices1.get(i);
                    Indexer i2 = (Indexer)indices2.get(i);
                    if (i1.compareTo(i2) == 0) {
                        continue;
                    }
                    else {
                        return i1.compareTo(i2);
                    }
                }
                //This should only happen if there is 1 graph (i.e.,
                //no projections
                return 0;
            }
        });

        fontName = null;
        fontSize = 14;
        orientation = DotOrientation.VERTICAL;
    }

    /**
     * Adds a mapping from an list of Indices to a particular Graph if the
     * mapping does not exist. The list of indices identifies the type that is
     * projected and the sequence in the state of the type. If there are no
     * Indexers (the List is empty), then there are no projections on any
     * type. The list of indices should be sorted based on the compareTo method
     * of Indexer.
     */
    @SuppressWarnings("unchecked")
    public void assocGraphWithIndices(List indices, Graph graph) {
        indexToGraph.put(indices, graph);
    }

    public void setOrientation(DotOrientation orientation) {
        this.orientation = orientation;
    }

    /**
     * Obsolete--Viz font is no longer set using this field.  It uses the global text font
     * @return
     */
    public void setFontName(String name) {
        this.fontName = name;
    }

    /**
     * Obsolete--Viz font is no longer set using this field.  It uses the global text font
     * @return
     */
    public void setFontSize(int fontSize) {
        this.fontSize = fontSize;
    }

    /**
     * Returns the graph associated with a particular
     * array of indices.  Can return null.
     */
    public Graph getGraph(List indices) {
        if (!indexToGraph.containsKey(indices)) {
            throw new NoSuchElementException();
        }
        Object ret = indexToGraph.get(indices);
        return (ret==null ? null : (Graph)ret);
    }

    public DotOrientation getOrientation() {
        return orientation;
    }

    /**
     * Obsolete--Viz font is no longer set using this field.  It uses the global text font
     * @return
     */
    public String getFontName() {
        return fontName;
    }

    /**
     * Obsolete--Viz font is no longer set using this field.  It uses the global text font
     * @return
     */
    public int getFontSize() {
        return fontSize;
    }

    /**
     * Checks if the specified List of Indices are in this Cartoon.
     */
    public boolean containsIndices(List indices) {
        return indexToGraph.containsKey(indices);
    }

    /**
     * Checks if the specified graph is in this cartoon.
     */
    public boolean containsGraph(Graph g) {
        return indexToGraph.containsValue(g);
    }

    /**
     * Returns a List of all Indexer lists that are associated with a graph
     * in ascending order.
     */
    @SuppressWarnings("unchecked")
    public List getAllIndices() {
        return Collections.unmodifiableList(new LinkedList(indexToGraph.keySet()));
    }

    /**
     * Returns a List of all Graphs in the Cartoon, ordered based on their 
     * indexers in ascending order.
     */
    @SuppressWarnings("unchecked")
    public List getAllGraphs() {
        return Collections.unmodifiableList(new LinkedList(indexToGraph.values()));
    }

    /**
     * Returns a String representation of this Cartoon.
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append("Cartoon\n");
        Iterator entries = indexToGraph.entrySet().iterator();
        while (entries.hasNext()) {
            Map.Entry nextEntry = (Map.Entry)entries.next();
            s.append(
                nextEntry.getKey().toString() + " -> " + nextEntry.getValue().toString() + "\n\n");
        }
        return s.toString();
    }
}
