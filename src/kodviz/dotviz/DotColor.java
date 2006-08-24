package kodviz.dotviz;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.ImageIcon;

@SuppressWarnings("serial")
public class DotColor extends DotProperty implements Serializable {

    public static final String DISPLAYED_NAME = "Color";
    public static final String DOT_NAME = "color";

    // added for the "contrasting" colors between background and text
    // THIS IS BAD....will fix
    private String _labelColorText;

    /*********serializing code**************/   
    // Ordinal of next color to be created
    private static int nextOrdinal = 0;
    // Assign an ordinal to this color
    private final int ordinal = nextOrdinal++;

    public static final DotColor WHITE = new DotColor("White","white","black", "icons/ColorIcons/white.gif");
    public static final DotColor GRAY  = new DotColor("Gray","lightgray","black", "icons/ColorIcons/gray.gif");
    public static final DotColor BLACK  = new DotColor("Black","black","white", "icons/ColorIcons/black.gif");
    
    public static final DotColor STANDARD_RED  = new DotColor("Red","red","white", "icons/ColorIcons/red.gif");
    public static final DotColor STANDARD_GREEN  = new DotColor("Green","green2","black", "icons/ColorIcons/green.gif");
    public static final DotColor STANDARD_BLUE  = new DotColor("Blue","blue","white", "icons/ColorIcons/blue.gif");
    //public static final DotColor BROWN  = new DotColor("Brown","brown","white", "icons/ColorIcons/brown.gif");
    public static final DotColor STANDARD_YELLOW  = new DotColor("Yellow","yellow","black", "icons/ColorIcons/yellow.gif");
    
    // new colors added to support Palette feature. We should probably change the icons so they reflect the correct color
    public static final DotColor CLASSIC_RED = new DotColor("Red","palevioletred","black","icons/ColorIcons/palevioletred.gif");
    public static final DotColor CLASSIC_GREEN = new DotColor("Green","limegreen","black","icons/ColorIcons/limegreen.gif");
    public static final DotColor CLASSIC_BLUE = new DotColor("Blue","cornflowerblue","black","icons/ColorIcons/cornflowerblue.gif");
    public static final DotColor CLASSIC_YELLOW = new DotColor("Yellow","gold","black","icons/ColorIcons/gold.gif");

    public static final DotColor MARTHA_RED = new DotColor("Red","salmon","black","icons/ColorIcons/salmon.gif");
    public static final DotColor MARTHA_GREEN = new DotColor("Green","darkolivegreen2","black","icons/ColorIcons/darkolivegreen2.gif");
    public static final DotColor MARTHA_BLUE = new DotColor("Blue","cadetblue","black","icons/ColorIcons/cadetblue.gif");
    public static final DotColor MARTHA_YELLOW = new DotColor("Yellow","lightgoldenrod","black","icons/ColorIcons/lightgoldenrod.gif");
    
    public static final DotColor NEON_RED = new DotColor("Red","magenta","black", "icons/ColorIcons/magenta.gif");
    public static final DotColor NEON_GREEN = new DotColor("Green","chartreuse2","black","icons/ColorIcons/chartreuse2.gif");
    public static final DotColor NEON_BLUE  = new DotColor("Blue","cyan","black", "icons/ColorIcons/cyan.gif");
    public static final DotColor NEON_YELLOW = new DotColor("Yellow","yellow","black","icons/ColorIcons/yellow.gif");
       
    private static final DotColor[] VALS = 
       {WHITE, GRAY, BLACK, STANDARD_RED, STANDARD_GREEN, STANDARD_BLUE, STANDARD_YELLOW, CLASSIC_RED, CLASSIC_GREEN,
       CLASSIC_BLUE, CLASSIC_YELLOW, MARTHA_RED, MARTHA_GREEN, MARTHA_BLUE, MARTHA_YELLOW, 
       NEON_RED, NEON_GREEN, NEON_BLUE, NEON_YELLOW};

    //
    // CONSTRUCTOR
    //

    /**
     * labelColorText_ is the corresponding text color, use white or black
     */
    private DotColor(String displayedText_, String dotText_, String labelColorText_, String iconPath_) {
	_displayedText = displayedText_;
	_dotText = dotText_;
	_displayedName = DISPLAYED_NAME;
	_dotName = DOT_NAME;
	_labelColorText = labelColorText_;
	_icon = new ImageIcon(ImageHandler.loadImage(iconPath_));
    }

    /**
     * gets the label color text
     */
    public String getLabelColorText() {
	return _labelColorText;
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
    






