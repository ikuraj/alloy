package kodviz.dotviz;

import java.io.ObjectStreamException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Type-safe enum. pattern for orientation of graph (Cartoon). Currently, 
 * DOT allows us to layout graphs in a vertical or horizontal rank. Note that
 * the selected orientation determines how nodes selected to be on the 
 * same rank are laid out.
 */
public class DotOrientation extends DotProperty {
    
	private static final long serialVersionUID = 1L;
	private static final String DISPLAYED_NAME = "Orientation";
    private static final String DOT_NAME = "rankdir";
    
    public static final DotOrientation HORIZONTAL = new DotOrientation("Left to Right", "LR");
    public static final DotOrientation VERTICAL = new DotOrientation("Top to Bottom", "TB");

    /*********serializing code**************/   
    // Ordinal of next suit to be created
    private static int nextOrdinal = 0;
    // Assign an ordinal to this suit
    private final int ordinal = nextOrdinal++;
    
    private static final DotOrientation[] VALS = 
    { HORIZONTAL, VERTICAL };


    private DotOrientation(String displayedText_, String dotText_) {
	_displayedText = displayedText_;
	_dotText = dotText_;
	_displayedName = DISPLAYED_NAME;
	_dotName = DOT_NAME;
	_icon = null;
    }

    public static List getValidValues() {
	return Collections.unmodifiableList(Arrays.asList(VALS));
    }
    
    private Object readResolve()
            throws ObjectStreamException {
        return VALS[ordinal]; // Canonicalize
    }	
}
	



