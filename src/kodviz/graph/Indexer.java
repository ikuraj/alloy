package kodviz.graph;

/**
 * An Indexer is an abstraction for states. An Indexer keeps track of
 * the type of the nodes being projected and the and the indexes
 * (states) of those types.  
 */
public class Indexer implements Comparable {

    private String type;
    private int index;

    /**
     * Creates a new Indexer with specified type and index value.
     */
    public Indexer(String type, int index) {
        this.type = type;
        this.index = index;
    }

    /**
     * Returns the type of the indexer.
     */
    public String getType() {
        return type;
    }

    /**
     * Returns the index value.  A value of -1 means that there are no valid 
     * atoms to choose from for this type.  This usually occurs when the type
     * has no atoms
     */
    public int getIndex() {
        return index;
    }

    /**
     * Two indexers are compared based first on their type, then on their 
     * actual index values.
     */
    public int compareTo(Object o) {
        Indexer i = (Indexer)o;
        if (this.getType().compareTo(i.getType()) == 0) {
            if (this.getIndex() < i.getIndex()) {
                return -1;
            }
            else if (this.getIndex() == i.getIndex()) {
                return 0;
            }
            else {
                return 1;
            }
        }
        else {
            return this.getType().compareTo(i.getType());
        }
    }

    /**
     * Two indexers are equal if they represent the same type and index.
     */
    public boolean equals(Object o) {
        if (o == null || !(o instanceof Indexer)) {
            return false;
        }
        Indexer test = (Indexer)o;
        if (this.type.equals(test.type) && this.index == test.index) {
            return true;
        }
        return false;
    }

    /**
     * Returns a hash code for this indexer.
     */
    public int hashCode() {
        return type.hashCode() + index;
    }

    /**
     * Returns a String represenation of this indexer.
     */
    public String toString() {
        return "Index on " + type + " State:" + index;
    }
}
