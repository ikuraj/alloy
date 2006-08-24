package kodviz.dotviz;

import java.io.ObjectStreamException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * typesafe enum for the dir keyword
 */
public class DotDirection extends DotProperty{
    
	private static final long serialVersionUID = 1L;

	private static int nextOrdinal = 0;
    private static final int ordinal = nextOrdinal++;

    //
    // CONSTRUCTOR
    //
    
    private DotDirection(String displayedText_, String dotText_) {
	_displayedText = displayedText_;
	_dotText = dotText_;
	_displayedName = "Arrow direction";
	_dotName = "dir";
	_icon = null;
    }
    
    public static final DotDirection FORWARD = new DotDirection("Forward","forward");
    public static final DotDirection BACK = new DotDirection("Backward","back");
    public static final DotDirection BOTH = new DotDirection("Both","both");
    public static final DotDirection NONE = new DotDirection("None","none");

    private static final DotDirection[] VALS = {FORWARD, BACK, BOTH, NONE};
    
    @SuppressWarnings("unchecked")
    public static List getValidValues() {
	List values = new ArrayList(4);

	values.add(FORWARD);
	values.add(BACK);
	values.add(BOTH);
	values.add(NONE);

	return Collections.unmodifiableList(values);
    }

    // used for deserialization
    private Object readResolve() throws ObjectStreamException {
	return VALS[ordinal];
    }
}
