package kodviz.alloyviz;

import java.awt.Font;
import java.io.Serializable;

import kodviz.util.Params;

import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotOrientation;
import kodviz.dotviz.DotPalette;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;


/**
 * GeneralView represents the settings for visualization that are not dependent
 * on a specific model. These settings are graph font size, orientation, and 
 * the default Node and Edge visualization settings.
 */
public class GeneralView implements Serializable {

	private static final long serialVersionUID = 1L;

	private String fontName;

    private int fontSize;

    private DotOrientation orientation;

    private NodeViz nodeViz;

    private EdgeViz edgeViz;

    private DotPalette nodePalette, edgePalette;

    /**
     * Constructs a general view with the default font size of 14,
     * orientation up-down, and settings for nodes and edges. By
     * default, all node and edge rankings are ignored. Edge weights are 0
     * and edge attributes are ignored. Bidirectional edges are merged. All
     * node and edge labels are the empty string.
     */
    public GeneralView() {
        // need to fix fontName and fontSize to use preferences
        fontName = null;
        fontSize = 14;
        orientation = DotOrientation.VERTICAL;
        nodeViz =
            new NodeViz(
                Boolean.TRUE,
                null,
                DotColor.WHITE,
                DotShape.ELLIPSE,
                DotStyle.SOLID,
                Boolean.FALSE,
                Boolean.TRUE,
                Boolean.TRUE,
                Boolean.FALSE,
                Boolean.FALSE,
                Boolean.TRUE);
        edgeViz =
            new EdgeViz(
                Boolean.TRUE,
                null,
                DotColor.BLACK,
                DotStyle.SOLID,
                0,
                Boolean.FALSE,
                Boolean.FALSE,
                Boolean.TRUE,
                Boolean.TRUE,
                Boolean.FALSE);
        nodePalette = DotPalette.CLASSIC;
        edgePalette = DotPalette.CLASSIC;
        // null means that there is no default label for Nodes or Edges (if you
        // try to use the default, no label will appear at all.

    }

    public GeneralView copy() {
        GeneralView gv = new GeneralView();
        gv.fontName = this.fontName;
        gv.fontSize = this.fontSize;
        gv.orientation = this.orientation;
        gv.nodeViz = this.nodeViz.copy();
        gv.edgeViz = this.edgeViz.copy();
        gv.nodePalette = this.nodePalette;
        gv.edgePalette = this.edgePalette;
        return gv;
    }

    // Accessors
    public DotPalette getNodePalette() {
        return nodePalette;
    }

    public DotPalette getEdgePalette() {
        return edgePalette;
    }

    /**
     * Obsolete--Viz font is no longer set using this view field.  It uses the global text font
     * @return
     */
    public String getGeneralFontName() {
        return fontName;
    }

    /**
     * Obsolete--Viz font is no longer set using this view field.  It uses the global text font
     * @return
     */
    public int getGeneralFontSize() {
        return fontSize;
    }

    public DotOrientation getGeneralOrientation() {
        return orientation;
    }

    public NodeViz getGeneralNodeViz() {
        return nodeViz;
    }

    public EdgeViz getGeneralEdgeViz() {
        return edgeViz;
    }

    //Mutators

    public void resetView() {
        // initialize font to the current global prefs
        Font textFont = Params.glob.getFontParam("GUI", "textfont");        
        setGeneralFontName(textFont.getFamily());
        setGeneralFontSize(textFont.getSize());

        setGeneralOrientation(DotOrientation.VERTICAL);
        setGeneralNodeViz(
            new NodeViz(
                Boolean.TRUE,
                null,
                DotColor.WHITE,
                DotShape.ELLIPSE,
                DotStyle.SOLID,
                Boolean.FALSE,
                Boolean.TRUE,
                Boolean.TRUE,
                Boolean.FALSE,
                Boolean.FALSE,
                Boolean.TRUE));
        setGeneralEdgeViz(
            new EdgeViz(
                Boolean.TRUE,
                null,
                DotColor.BLACK,
                DotStyle.SOLID,
                0,
                Boolean.FALSE,
                Boolean.FALSE,
                Boolean.TRUE,
                Boolean.TRUE,
                Boolean.FALSE));
        setNodePalette(DotPalette.CLASSIC);
        setEdgePalette(DotPalette.CLASSIC);
    }

    public void setNodePalette(DotPalette dp) {
        this.nodePalette = dp;
    }

    public void setEdgePalette(DotPalette dp) {
        this.edgePalette = dp;
    }

    /**
     * Obsolete--Viz font is no longer set using this view field.  It uses the global text font
     * @return
     */
    public void setGeneralFontName(String name) {
        this.fontName = name;
    }

    /**
     * Obsolete--Viz font is no longer set using this view field.  It uses the global text font
     * @return
     */
    public void setGeneralFontSize(int fontSize) {
        this.fontSize = fontSize;
    }

    public void setGeneralOrientation(DotOrientation orientation) {
        this.orientation = orientation;
    }

    public void setGeneralNodeViz(NodeViz nodeViz) {
        this.nodeViz = nodeViz;
    }

    public void setGeneralEdgeViz(EdgeViz edgeViz) {
        this.edgeViz = edgeViz;
    }

    /**
     * Two GeneralViews are equal if they have the same font size,
     * orientation, and viz settings for nodes and edges.  
     */
    public boolean equals(Object o) {
        if (o == null || !(o instanceof GeneralView)) {
            return false;
        }
        GeneralView gv = (GeneralView)o;
        if (this.fontSize == gv.fontSize
            && this.fontName.equals(gv.fontName)
            && this.nodePalette.equals(gv.nodePalette)
            && this.edgePalette.equals(gv.edgePalette)
            && this.orientation == gv.orientation
            && this.nodeViz == gv.nodeViz
            && this.edgeViz == gv.edgeViz) {
            return true;
        }
        return false;
    }

    public int hashCode() {
        return 7
            + 3 * fontSize
            + 11 * orientation.hashCode()
            + 13 * nodeViz.hashCode()
            + 17 * nodeViz.hashCode();
    }

    public String toString() {
        return "General View:\n"
            + "Font size: "
            + fontSize
            + "\nOrientation: "
            + orientation
            + "\nNode Theme: "
            + nodePalette.getDisplayedText()
            + "\nEdge Theme: "
            + edgePalette.getDisplayedText()
            + "\n"
            + nodeViz
            + "\n"
            + edgeViz;
    }
}
