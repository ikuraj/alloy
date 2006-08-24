package kodviz.dotviz;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * typesafe enum for representing edge's arrow heads
 */
@SuppressWarnings("serial")
public class DotArrowHead extends DotProperty implements Serializable {

    public static final DotArrowHead NORMAL = new DotArrowHead("Normal","normal");
    public static final DotArrowHead DOT = new DotArrowHead("Filled dot","dot");
    public static final DotArrowHead ODOT  = new DotArrowHead("Unfilled dot","odot");
    public static final DotArrowHead INV = new DotArrowHead("Inverted arrow","inv");
    public static final DotArrowHead INVDOT = new DotArrowHead("Inverted arrow, filled ot","invdot");
    public static final DotArrowHead INVODOT = new DotArrowHead("Inverted arrow, unfilled dot","invodot");
    public static final DotArrowHead NONE  = new DotArrowHead("None","none");
    
    /*********serializing code**************/   
    // Ordinal of next suit to be created
    private static int nextOrdinal = 0;
    // Assign an ordinal to this suit
    private final int ordinal = nextOrdinal++;
    
    private static final DotArrowHead[] VALS = 
    { NORMAL, DOT, ODOT, INV, INVDOT, INVODOT, NONE };


    //
    // CONSTRUCTOR
    //
    
    private DotArrowHead(String displayedText_, String dotText_) {
	_displayedText = displayedText_;
	_dotText = dotText_;
	_displayedName = "Arrowhead type";
	_dotName = "arrowhead";
    }
        
        
    public static List getValidValues() {
	return Collections.unmodifiableList(Arrays.asList(VALS));
    }

    /* for serializing purposes */
    private Object readResolve()
            throws ObjectStreamException {
        return VALS[ordinal]; // Canonicalize
    }
}
