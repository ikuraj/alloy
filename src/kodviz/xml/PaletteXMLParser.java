package kodviz.xml;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import ext.nanoxml.XMLElement;
import ext.nanoxml.XMLParseException;

import kodviz.alloyviz.AlloyModule;
import kodviz.alloyviz.AlloyNodeElement;
import kodviz.alloyviz.AlloyRelation;
import kodviz.alloyviz.AlloySet;
import kodviz.alloyviz.AlloyType;
import kodviz.alloyviz.EdgeViz;
import kodviz.alloyviz.GeneralView;
import kodviz.alloyviz.InstanceProjectionFrame;
import kodviz.alloyviz.InstanceProjector;
import kodviz.alloyviz.Model;
import kodviz.alloyviz.ModelView;
import kodviz.alloyviz.NodeViz;
import kodviz.alloyviz.ProjectionFrame;
import kodviz.alloyviz.View;
import kodviz.alloyviz.ViewPalette;
import kodviz.alloyviz.VizInstance;
import kodviz.alloyviz.VizMap;
import kodviz.alloyviz.VizResolver;
import kodviz.alloyviz.VizState;
import kodviz.dotviz.DotColor;
import kodviz.dotviz.DotOrientation;
import kodviz.dotviz.DotPalette;
import kodviz.dotviz.DotPaletteManager;
import kodviz.dotviz.DotProperty;
import kodviz.dotviz.DotShape;
import kodviz.dotviz.DotStyle;

/**
 * PaletteXMLParser is a tool for importing and exporting palettes
 * from and to XML documents. Refer to palette.xsd for the XML schema.
 * The principal methods are <code>writeXML(ViewPalette, String)</code> and
 * <code>readXML(File)</code>.
 * @author jbaek
 * 
 * NOTE: When this package is migrated onto Alloy 3, some methods in
 * the existing viz package must be modified to gain visibility of some
 * methods/fields. See TODO-LIST for detail.
 */

@SuppressWarnings("unchecked")
public class PaletteXMLParser {

	final private VizState _vizState;
	private VizInstance _vizInstance;
	private Model _vizModel; // underlying solution
	private View _current; // default view
	final static DotPaletteManager _colorResolver =
		new DotPaletteManager();
	private DotPalette _newNodeTheme, _oldNodeTheme;
	private DotPalette _newEdgeTheme, _oldEdgeTheme;
	
	/**
	 * A wrapper around a 3rd party XML parser. If a new XML Parser is used,
	 * simply change the methods in this class, and modify the constructors
	 * used.
	 * @author jbaek
	 */
	private static class XMLTag {
		final String tagname;
		private XMLElement source; // using Nanoxml
		
		XMLTag(XMLElement x) throws PaletteXMLParseException {
			this(x, null);
		}
		
		/**
		 * Constructs a new XMLTag instance, and checks if the XMLElement
		 * matches the provided tag type.
		 * @param x the XML Element to be wrapped
		 * @param expected_tag the expected top-level tag
		 * @throws PaletteXMLParseException if the top-level tag does not match
		 * the provided tag name, or if the XML element does not have
		 * attribute "name."
		 */
		XMLTag(XMLElement x, String expected_tag)
		throws PaletteXMLParseException {
			XMLTag._verifyTopLevelTag(x, expected_tag);
			tagname = x.getName();
			source = x;
		}
		/**
		 * Returns the children tags
		 * @return a vector containing the children tags. Each element of the
		 *  vector is of the original XML parser type.
		 */
		Vector getChildren() { return source.getChildren(); }
		
		/**
		 * Returns the PCDATA content of the tag
		 * @return the PCDATA content of the tag, null if it doesn't exist
		 */
		String getContent() { return source.getContent(); }
		
		/**
		 * Returns the specified attribute.
		 * @param attribute the name of the attribute
		 * @return the value of the attribute if it exists and is non-empty,
		 *  null otherwise
		 */
		String getAttribute(String attribute) {
			String ret = (String)(source.getAttribute(attribute));
			if (ret == null || ret.length() == 0) return null;
			return ret;
		}
		/**
		 * Returns the tag name.
		 * @return the tag name
		 */
		String getTagName() { return tagname; }
		
		/**
		 * Returns the value of the attribute 'name.'
		 * @return the value of the attribute 'name,' or null if the value is
		 *  empty or does not exist
		 */
		String getName() { return getAttribute("name"); }
		
		/**
		 * Returns the string representation of the XML element
		 * @return the string representation of the XML element
		 */
		public String toString() { return source.toString(); }
		
		/**
		 * Verifies that the given XML tag has the specified top-level tag.
		 * No verification is done if <code>tag</code> is null.
		 * @param e the XMLElement representing the XML tag to be checked
		 * @param expected_name the expected name of the tag
		 * @throws PaletteXMLParseException if <code>e.getName()</code> is
		 *  not equal to <code>expected_name</code>
		 */
		private static void _verifyTopLevelTag(XMLElement e, String tag)
		throws PaletteXMLParseException {
			if (tag == null) return;
			if (e.getName() == null || !e.getName().equals(tag))
				throw new PaletteXMLParseException("Expected a(n) " + tag
						+ " tag, found " + e.getName() + " instead.");
		}
	}

	private final static int TRISTATE = 0;
	private final static int BOOLEAN = 1;
	private final static int STRING = 2;
	private final static int DOTCOLOR_NODE = 3;
	private final static int DOTCOLOR_EDGE = 9;
	private final static int DOTSTYLE = 4;
	private final static int DOTSHAPE = 5;
	private final static int INTEGER = 6;
	private final static int DOTORIENTATION = 7;
	private final static int DOTPALETTE = 8;
	private final static String UNIVERSAL_VIZ = "all";
	
	private final static String TRISTATE_TRUE = Boolean.TRUE.toString();
	private final static String TRISTATE_FALSE = Boolean.FALSE.toString();
	private final static String TRISTATE_INHERIT = "inherited";
	
	/**
	 * Constructs a new palette parser with the given underlying
	 * model of the solution and a default view. A new copy of the
	 * <code>View</code> object is created for this instance of
	 * <code>PaletteXMLParser</code>.
	 * @requires the supplied view must correspond to the current model of
	 *  the given <code>VizInstance</code> object
	 * @param model the underlying model of the Alloy solution
	 * @param view the default view currently in use
	 */
	public PaletteXMLParser(VizState viz, View view) {
		_vizState = viz;
		setDefaultView(view);
	}
	
	/**
	 * Creates a new instance of the solution based on the
	 * projected types in the current default view
	 */
	private void resetProjectedInstance(ProjectionFrame pf) {
		InstanceProjectionFrame ipf = pf.instantiate(_vizState.getInstance());
		InstanceProjector ip = new InstanceProjector(
				_vizState.getInstance(),
				ipf);
		_vizInstance = ip.getProjectedInstance();
		_vizModel = _vizInstance.getModel();
	}
	
	/**
	 * Creates a new instance based on the projection of the default
	 * view; if the default view is null, create an unprojected instance.
	 * The projected types will be added to pf.
	 */
	private void resetDefaultProjection(ProjectionFrame pf) {
		pf.clear();
		if (_current != null) {
			Set _current_types = _current.getModelView()
				.getProjectionFrame().getProjectedTypes();
			for (Iterator j = _current_types.iterator(); j.hasNext();
				pf.projectOn((AlloyType)(j.next()))); 
			resetProjectedInstance(pf);
		}
	}
	/**
	 * Sets the default view for this parser, and rests the
	 * internal vizInstance and Model object to the unprojected model.
	 * @param view the default view
	 */
	public void setDefaultView(View view) {
		if (view != null) {
			_current = view.copy();
		} else {
			_current = null;
		}
		_vizInstance = _vizState.getInstance();
		_vizModel = _vizState.getUnprojectedModel();
		resetThemeData();
	}
	
	/**
	 * Sets the default node and edge theme from which to extract color
	 */
	private void resetThemeData() {
		_newNodeTheme = _oldNodeTheme = (_current != null) ? _current
				.getGeneralView().getNodePalette() : DotPalette.CLASSIC;
		_newEdgeTheme = _oldEdgeTheme = (_current != null) ? _current
				.getGeneralView().getEdgePalette() : DotPalette.CLASSIC;
	}
	
	/**
	 * Returns an XML document form of the specified palette. This palette
	 * must correspond to the <code>Model</code> object with which
	 * the PaletteXMLParser is instantiated. Otherwise, correct parsing is
	 * not guaranteed. 
	 * @param v the <code>ViewPalette</code> instance to be parsed into XML
	 * @return a string that contains the XML document
	 * @throws PaletteXMLParseException if it is detected that the view does
	 *  not correspond to the <code>Model</code> object
	 */
	public String writeXML(ViewPalette v, String palettename)
	throws PaletteXMLParseException {
		XMLElement palette_tag = new XMLElement();
		palette_tag.setName("palette");
		palette_tag.setAttribute("name", palettename);
		
		Iterator i = v.getAllViews().iterator();
		while (i.hasNext()) {
			View _next = (View)(i.next());
			XMLElement view_tag = new XMLElement();
			view_tag.parseString(this.writeViewXML(_next,
					(_next == v.getCurrentView())));			
			palette_tag.addChild(view_tag);
		}
		return palette_tag.toString();
	}
	
	/**
	 * Returns an XML document by calling <code>writeXML(ViewPalette, String)
	 * </code> and reconciliates the result with the given XML document.
	 * In other words, overwrite only as much as needed, and preserve
	 * as much as possible from the old document.
	 * @see writeXML(ViewPalette, String)
	 * @param oldData the existing document to be overwritten
	 * @requires The provided palette can contain at most one view, and
	 *  the same applies to <code>oldData</code>. If topTag.this is not met,
	 *  or if the old file is not an XML document, the old data is overwritten
	 *  entirely
	 */
	public static String reconcilePalettes_Alloy4(String newData,
			String oldData) {
		XMLElement oldXML = new XMLElement();
		XMLElement newXML = new XMLElement();
		Map<String, XMLElement> oldTags = new HashMap<String, XMLElement>();
		try {
			newXML.parseString(newData);
			oldXML.parseString(oldData);
		} catch (XMLParseException e) {
			e.printStackTrace();
			return newData; // old file can't be read 
		}
		try {
			XMLTag oldTopTag = new XMLTag(oldXML, "palette");
			if (oldTopTag.getChildren().size() != 1 ||
					newXML.getChildren().size() != 1) return newData;
			XMLTag oldViewTag = new XMLTag(
					(XMLElement)(oldTopTag.getChildren().get(0)), "view");
			XMLElement newViewTag = (XMLElement)(newXML.getChildren().get(0));
			Iterator i = oldViewTag.getChildren().iterator();
			while (i.hasNext()) {
				XMLElement elt = (XMLElement)(i.next());
				if (elt.getName().equals("node") ||
						elt.getName().equals("edge")) {
					String _target = (String)(elt.getAttribute("target"));
					if (_target == null) _target = UNIVERSAL_VIZ;
					if (_target.equals(UNIVERSAL_VIZ))
						_target = _target + "+" + elt.getName(); 
					oldTags.put(_target, elt);
				}
			}
			Iterator j = newViewTag.getChildren().iterator();
			while (j.hasNext()) {
				XMLElement elt = (XMLElement)(j.next());
				if (elt.getName().equals("node") ||
						elt.getName().equals("edge")) {
					String _target = (String)(elt.getAttribute("target"));
					if (_target == null) _target = UNIVERSAL_VIZ;
					if (_target.equals(UNIVERSAL_VIZ))
						_target = _target + "+" + elt.getName(); 
					oldTags.remove(_target);
				}
			}
			for (Iterator k = oldTags.keySet().iterator(); k.hasNext();
				newViewTag.addChild(oldTags.get((String)(k.next()))));
			return newXML.toString();
		} catch (PaletteXMLParseException e) {
			return newData;
		}
	}
	
	/**
	 * Returns a XML document form of the specified view. This view
	 * must correspond to the <code>Model</code> object with which
	 * <code>PaletteXMLParser</code> is instantiated. Otherwise, correct
	 * parsing is not guaranteed. 
	 * @param v the <code>View</code> instance to be parsed into XML form
	 * @return a string that contains the XML document
	 * @throws PaletteXMLParseException if it is detected that the view does
	 *  not correspond to the <code>Model</code> object
	 */
	public String writeViewXML(View v, boolean isDefault)
	throws PaletteXMLParseException {
		setDefaultView(v);
		resetProjectedInstance(v.getModelView().getProjectionFrame());
		Model _model = _vizModel;
		XMLElement ret = new XMLElement();
		ret.setName("view");
		if (isDefault)
			ret.setAttribute("default", "true");
		ret.setAttribute("name", v.getName());
		// write global tags and projections
		ret.addChild(writeGlobalXML(v));
		ret.addChild(writeProjectionXML(v));
		// parse general node/edge setting
		GeneralView gv = v.getGeneralView();
		NodeViz _generalNodeViz = gv.getGeneralNodeViz();
		EdgeViz _generalEdgeViz = gv.getGeneralEdgeViz();
		ret.addChild(writeNodeXML(_generalNodeViz, UNIVERSAL_VIZ));
		ret.addChild(writeEdgeXML(_generalEdgeViz, UNIVERSAL_VIZ, null));
		// parse specific node settings
		VizMap _vizmap = v.getModelView().getVizMap();
		Iterator nodeIte = _vizmap.getNodes().iterator();
		while (nodeIte.hasNext()) {
			AlloyNodeElement elm = (AlloyNodeElement)(nodeIte.next());
			String name = null;
			if (!(elm.getName().contains(AlloyXMLParser._SEPARATOR))) {
				if (elm instanceof AlloyType) {
					name = _getQualifiedTypeName((AlloyType)elm, false);
					if (name == null) {
						throw new PaletteXMLParseException("Cannot resolve "
								+ "the module for type " + elm.getName());
					}
				} else { // of AlloySet class
					Iterator i = _model.getModules().iterator();
					while (i.hasNext()) {
						AlloyModule am = (AlloyModule) i.next();
			            if (am.getSets().contains(elm)) {
			            	name = am.getName() +
			            		AlloyXMLParser._SEPARATOR + elm.getName();
			            	break;
			            }
			       }
				}
				if (name == null) {
					throw new PaletteXMLParseException("Cannot resolve "
							+ "the module for type " + elm.getName());
				}
			} else name = elm.getName();
			NodeViz _specificNodeViz = _vizmap.getNodeViz(elm);
			ret.addChild(writeNodeXML(_specificNodeViz, name));			
		}
		Iterator edgeIte = _vizmap.getEdges().iterator();
		while (edgeIte.hasNext()) {
			AlloyRelation elm = (AlloyRelation)(edgeIte.next());
			List _types = elm.getTypes();
			String _parsedTypes = _typesToString(_types, false);
			if (_parsedTypes == null)
				throw new PaletteXMLParseException("Cannot resolve the type"
						+ " of the relation " + elm.getName());
			String name = null;
			if (!(elm.getName().contains(AlloyXMLParser._SEPARATOR))) {
				Iterator i = _model.getModules().iterator();
				while (i.hasNext()) {
					AlloyModule am = (AlloyModule) i.next();
					if (am.getRelations().contains(elm)) {
						name = am.getName() +
							AlloyXMLParser._SEPARATOR + elm.getName();
			            break;
					}
				}
				if (name == null) {
					throw new PaletteXMLParseException("Cannot resolve "
							+ "the module for relation " + elm.getName());
				}
			} else name = elm.getName();
			EdgeViz _specificEdgeViz = _vizmap.getEdgeViz(elm);
			ret.addChild(writeEdgeXML(_specificEdgeViz, name, _parsedTypes));			
		}
		return ret.toString();
	}
	
	/**
	 * Writes out XML for the given <code>NodeViz</code> instance.
	 * The target specifies the name of the signature, set or relation
	 * it applies to. Use <code>UNIVERSAL_VIZ</code> if it is a general node
	 * setting.
	 * @param n the <code>NodeViz</code> instance to be written into XML
	 * @param target the name of the Alloy element to which the setting applies
	 * @return an <code>XMLElement</code> instance containing the XML tags 
	 */
	private XMLElement writeNodeXML(NodeViz n, String target) {
		XMLElement ret = new XMLElement();
		ret.setName("node");
		ret.setAttribute("target", target);
		writeAttributeXML(ret, VizResolver.VISIBLE,
				writeBoolean(n.isVisible()));
		writeAttributeXML(ret, VizResolver.ATTRIBUTE,
				writeBoolean(n.showInAttr()));
		writeAttributeXML(ret, VizResolver.SAME_RANK,
				writeBoolean(n.isSameRank()));
		writeAttributeXML(ret, VizResolver.SHOW_LABEL,
				writeBoolean(n.showLabel()));
		writeAttributeXML(ret, VizResolver.SELECTED,
				writeBoolean(n.isSelected()));
		writeAttributeXML(ret, VizResolver.HIDE_UNCONNECTED,
				writeBoolean(n.hideUnconnected()));
		writeAttributeXML(ret, VizResolver.NUMBER_ATOMS,
				writeBoolean(n.numberAtoms()));
		writeAttributeXML(ret, VizResolver.LABEL, n.getLabel());
		writeAttributeXML(ret, VizResolver.COLOR,
				n.getColor());
		writeAttributeXML(ret, VizResolver.STYLE,
				n.getStyle());
		writeAttributeXML(ret, VizResolver.SHAPE,
				n.getShape());
		return ret;
	}

	/**
	 * Writes out XML for the given <code>EdgeViz</code> instance.
	 * The target specifies the name of the signature, set or relation
	 * it applies to. Use <code>UNIVERSAL_VIZ</code> if it is a general edge
	 * setting.
	 * @param n the <code>EdgeViz</code> instance to be written into XML
	 * @param target the name of the Alloy element to which the setting applies
	 * @return an <code>XMLElement</code> instance containing the XML tags 
	 */
	private XMLElement writeEdgeXML(EdgeViz e, String target, String type) {
		XMLElement ret = new XMLElement();
		ret.setName("edge");
		ret.setAttribute("target", target);
		if (type != null) ret.setAttribute("type", type);
		writeAttributeXML(ret, VizResolver.VISIBLE,
				writeBoolean(e.isVisible()));
		writeAttributeXML(ret, VizResolver.MERGE,
				writeBoolean(e.mergeArrows()));
		writeAttributeXML(ret, VizResolver.SAME_RANK,
				writeBoolean(e.isSameRank()));
		writeAttributeXML(ret, VizResolver.LAYOUT_BACK,
				writeBoolean(e.layoutBack()));
		writeAttributeXML(ret, VizResolver.SELECTED,
				writeBoolean(e.isSelected()));
		writeAttributeXML(ret, VizResolver.ATTRIBUTE,
				writeBoolean(e.isAttribute()));
		writeAttributeXML(ret, VizResolver.LABEL, e.getLabel());
		writeAttributeXML(ret, VizResolver.COLOR,
				e.getColor());
		writeAttributeXML(ret, VizResolver.STYLE,
				e.getStyle());
		return ret;
	}
	
	/**
	 * Returns a string representation of the boolean values used
	 * in the palette XML (null specifies inherited values.)
	 * @param b the boolean value to be parsed into string
	 * @return a string representation of the given boolean value
	 */
	private String writeBoolean(Boolean b) {
		if (b == null) return TRISTATE_INHERIT;
		else if (b.booleanValue() == true)
			return TRISTATE_TRUE;
		return TRISTATE_FALSE;
	}
	
	/**
	 * Returns an XMLElement instance that contains the projected types.
	 * @param v the <code>View</code> instance to be parsed 
	 * @return an XMLElement instance that with "project" tag that contains
	 *  the list of projected tags
	 */
	private XMLElement writeProjectionXML(View v) {
		String _projectedTypeNames = _typesToString(new ArrayList(
				v.getModelView().getProjectionFrame().getProjectedTypes()),
				true);
		XMLElement ret = new XMLElement();
		ret.setName("project");
		ret.setContent(_projectedTypeNames);
		return ret;
	}
	
	/**
	 * Returns an XMLElement instance that contains the global parameters of
	 * a view. This consists of the font name, font size, node theme,
	 * edge theme and orientation.
	 * @param v the View instance that contains those global parameters 
	 * @return an XMLElement instance that with "global" tag that contains
	 *  the aforementioned settings as attribute tags
	 */
	private XMLElement writeGlobalXML(View v) {
		GeneralView gv = v.getGeneralView();
		XMLElement ret = new XMLElement();
		ret.setName("global");
		writeAttributeXML(ret, "font name", gv.getGeneralFontName());
		writeAttributeXML(ret, "font size",
				Integer.toString(gv.getGeneralFontSize()));
		writeAttributeXML(ret, "node theme",
				gv.getNodePalette().toString());
		writeAttributeXML(ret, "edge theme",
				gv.getEdgePalette().toString());
		writeAttributeXML(ret, "orientation",
				gv.getGeneralOrientation().toString());
		return ret;
	}
	
	/**
	 * Adds an attribute tag with the given name/value pair to the specified
	 * XMLElement tag
	 * @param parent the name of the XMLElement object to be modified
	 * @param name the name of the attribute
	 * @param value the value of the key specified by <code>name</code>
	 * @effect adds a child XMLElement tag to <code>parent</code> if
	 *  <code>value</code> is not null
	 */
	private void writeAttributeXML(XMLElement parent, String name,
			Object value) {
		if (value != null) {
			XMLElement att = new XMLElement();
			att.setName("attribute");
			att.setAttribute("value", value);
			att.setAttribute("name", name);
			parent.addChild(att);
		}
	}
	
	/**
	 * Reads a file and calls <code>readXM(String)</code>.
	 * @param f the file to be read
	 * @return a ViewPalette object that corresponds to the specified settings
	 * @see readXML(String)
	 */
	public ViewPalette readXML(File f) throws XMLParseException, 
	PaletteXMLParseException, IOException {
		return readXML(fileToString(f));
	}
	/**
	 * Processes a string that contains an XML document and returns
	 * a <code>ViewPalette</code> object that corresponds to the specified
	 * settings. This XML data must match the solution in the
	 * <code>Model</code>object with which <code>PaletteXMLParser</code> has
	 * been instantiated with.
	 * @param doc the string that contains the XML document
	 * @return a ViewPalette object that corresponds to the specified settings
	 * @throws PaletteXMLParseException if the XML does not correspond to a
	 *  well-formed Alloy palette object that maches the specified
	 *  <code>Model</code> object
	 * @throws XMLParseException if the XML is not well-formed 
	 */
	public ViewPalette readXML(String doc)
	throws PaletteXMLParseException, XMLParseException {
		XMLElement xml = new XMLElement();
		xml.parseString(doc);
		
		// parse top-level tag "palette"
		XMLTag _paletteTag = new XMLTag(xml, "palette");
		String _paletteName = _paletteTag.getName();
		PaletteXMLParserDbg.chk(_paletteName,
				"Palette tag is missing its name");
		ViewPalette ret = new ViewPalette(_paletteName);
		
		// parse views
		Iterator i = _paletteTag.getChildren().iterator();
		while (i.hasNext()) {
			XMLElement _next = (XMLElement)i.next();
			View _view = readViewXML(_next.toString());
			ret.addView(_view);
			String _isDefault = (String)(_next.getAttribute("default"));
			if (_isDefault != null && _isDefault.equals("true"))
				ret.setCurrentView(_view);
		}
		if (ret.getAllViews().size() == 0) {
			throw new PaletteXMLParseException("The file contains no views");
		} else if (ret.getAllViews().size() == 1) {
			ret.setCurrentView((View)(ret.getAllViews().get(0)));
		}
		return ret;
	}
	
	/**
	 * Processes a string that contains an XML document and returns
	 * a <code>View</code> object that corresponds to the specified settings.
	 * This XML data must match the solution in the <code>Model</code> object
	 * with which <code>PaletteXMLParser</code> has been instantiated.
	 * @param viewXML the string that contains the XML document
	 * @param _current the existing <code>View</code> to fall back to
	 * @return a <code>View</code> object corresponding to the XML document
	 * @throws PaletteXMLParseException if the XML does not correspond to a
	 *  well-formed Alloy View object
	 * @throws XMLParseException if the XML is not well-formed 
	 */
	/* Further note:
	 * 	Here is the set of conventions followed when a palette is read:
	 * 
	 * 0) In each view tag, the first subtag must be <global>. Then
	 *  <project> tag may or may not appear after that, and the rest
	 *  must be either <node> or <edge>.
	 * 
	 * 1) Projected types: if the XML document contains <project> tag,
	 *  the returned View will use the information in this tag, and ignore
	 *  all currently projected types. If it doesn't, it will keep the
	 *  currently projected types.
	 * 
	 * 2) Description of previously unknown types: If the XML describes
	 *  types that are not in the model, they are ignored.
	 *  
	 * 3) Description of already existing types: If the XML describes
	 *  types that are in the model, the returned View will use the information
	 *  in the XML, and supply any missing attributes from the current View
	 *  
	 * 4) Missing description of existing types: The current attributes
	 *  are kept in whole.
	 *  
	 * 5) Global parameters: the XML document must contain <global> tag.
	 *  Any missing attributes in this tag are set to default.
	 *  
	 * 6) If more than two set of node or edge tags are supplied for the same
	 *  the behavior is not predicted.
	 */
	public View readViewXML(String viewXML)
	throws PaletteXMLParseException,
	XMLParseException {
		XMLElement t = new XMLElement();
		t.parseString(viewXML);
		PaletteXMLParserDbg.chk("view".equals(t.getName()),
				"Expected view tag, encountered " + t.getName());
		String _viewName = (String)(t.getAttribute("name"));
		if (_viewName == null) throw new PaletteXMLParseException("View is"
				+ " missing its name");
		View ret = new View(_viewName);
		GeneralView gv = ret.getGeneralView();
		ModelView mv = ret.getModelView();
		VizMap _vizmap = mv.getVizMap();
		
		// global parameters
		NodeViz _generalNode = null;
		EdgeViz _generalEdge = null;
		resetThemeData();
				
		// intermediate data structures
		Map<AlloyNodeElement, NodeViz> _nodeVizMap =
			new HashMap<AlloyNodeElement, NodeViz>();		
		Map<AlloyRelation, EdgeViz> _edgeVizMap =
			new HashMap<AlloyRelation, EdgeViz>();
		Set _unprocessedNodes = new HashSet(_vizModel.getTypes());
		_unprocessedNodes.addAll(_vizModel.getSets());
		Set _unprocessedEdges = new HashSet(
				_vizModel.getRelations());
		String _projectedTypeNames = null;
		
		// process global tag
		Iterator i = t.getChildren().iterator();
		if (i.hasNext()) {
			XMLElement x = (XMLElement)(i.next());
			populateGlobalViewSettings(gv, new XMLTag(x, "global"));
		} else throw new PaletteXMLParseException(
			"View tag does not contain a global subtag");
		// process projection and other graph elements
		boolean _foundProjection = false;
		while (i.hasNext()) {
			XMLElement x = (XMLElement)(i.next());
			XMLTag elementTag = new XMLTag(x);
			if (!_foundProjection) { // process projected types
				_foundProjection = true;
				if (elementTag.getTagName().equals("project")) {
					_projectedTypeNames = elementTag.getContent();
					if (_projectedTypeNames == null && _current != null) {
						// keep old projected types

					} else { // overwrite with new ones
						List _projTypes = _stringToTypes(
								_projectedTypeNames);
						if (_projTypes != null) {
							for (Iterator j = _projTypes.iterator();
								j.hasNext(); mv.getProjectionFrame().projectOn(
										(AlloyType)(j.next())));
						} else {
							throw new PaletteXMLParseException(
									"Cannot resolve the projected type names: "
									+ _projectedTypeNames);
						}
					}
					// apply projection
					resetProjectedInstance(mv.getProjectionFrame());
					continue;
				} else
					resetDefaultProjection(mv.getProjectionFrame());
			}
			// process node or edge
			String _target = elementTag.getAttribute("target");
			if (_target == null) _target = UNIVERSAL_VIZ;
			Map<String, String> _currentElement =
				parseAttributes(elementTag);
			if (elementTag.getTagName().equals("node")) {
				AlloyNodeElement _match = _matchNodeName(_target);
				NodeViz _old = (_target.equals(UNIVERSAL_VIZ)) ?
					retrieveGeneralNodeViz() : retrieveNodeViz(_match);
				NodeViz _node = buildNode(_currentElement, _old);
				if (_target.equals(UNIVERSAL_VIZ)) {
					_generalNode = _node;
				} else if (_match == null) {
					// can't resolve node name; ignore: convention (2)
				} else {
					_nodeVizMap.put(_match, _node);
					_unprocessedNodes.remove(_match);
				}
			} else if (elementTag.getTagName().equals("edge")) {
				String _type = elementTag.getAttribute("type");
				AlloyRelation _match = _matchRelationName(_target, _type);
				EdgeViz _old = (_target.equals(UNIVERSAL_VIZ)) ?
						retrieveGeneralEdgeViz() : retrieveEdgeViz(_match);
				EdgeViz _edge = buildEdge(_currentElement, _old);
				if (_target.equals(UNIVERSAL_VIZ)) {
					_generalEdge = _edge;
				} else if (_match == null) {
					// can't resolve rel name; ignore: convention (2)
				} else {
					_edgeVizMap.put(_match, _edge);
					_unprocessedEdges.remove(_match);
				}
			} else 
				throw new PaletteXMLParseException("Found an unknown" +
						" tag in view: " + elementTag.getTagName());
		}
		// reset projection if necessary
		if (!_foundProjection)
			resetDefaultProjection(mv.getProjectionFrame());
		// sanity check to make sure that the global settings are defined
		if (_generalNode == null) {
			if (_current == null) {
				throw new PaletteXMLParseException(
					"No global settings defined for node elements");				
			} else
				_generalNode = adjustColor(
						_current.getGeneralView().getGeneralNodeViz());
		}
		if (_generalEdge == null) {
			if (_current == null) {
				throw new PaletteXMLParseException(
				"No global settings defined for edge elements");
			} else
				_generalEdge = adjustColor(
						_current.getGeneralView().getGeneralEdgeViz());
		}
		
		// process general node/edge settings
		gv.setGeneralNodeViz(_generalNode);
		gv.setGeneralEdgeViz(_generalEdge);
		
		// process model view
		VizMap _oldvizmap = (_current != null) ?
				_current.getModelView().getVizMap() : null;
		
		// process nodes
		i = _nodeVizMap.keySet().iterator();
		while (i.hasNext()) {
			AlloyNodeElement a = (AlloyNodeElement)(i.next());
			_vizmap.addNodeMapping(a, _nodeVizMap.get(a));
		}
		if (_unprocessedNodes.size() > 0) {
			Iterator j = _unprocessedNodes.iterator();
			while (j.hasNext()) {	
				AlloyNodeElement elt = (AlloyNodeElement)(j.next());
				if (_current == null || _oldvizmap.getNodeViz(elt) == null) {
					_vizmap.addNodeMapping(elt,
							new NodeViz(null, elt.getName(), null, null, null,
									null, null, null, null, null, null));
				} else { // get from old vizmap
					_vizmap.addNodeMapping(elt, adjustColor(
							_oldvizmap.getNodeViz(elt)));
				}
			}
		}
		
		// process edges
		i = _edgeVizMap.keySet().iterator();
		while (i.hasNext()) {
			AlloyRelation a = (AlloyRelation)(i.next());
			_vizmap.addEdgeMapping(a, _edgeVizMap.get(a));
		}
		if (_unprocessedEdges.size() > 0) {
			Iterator j = _unprocessedEdges.iterator();
			while (j.hasNext()) {
				AlloyRelation elt = (AlloyRelation)(j.next());
				if (_current == null || _oldvizmap.getEdgeViz(elt) == null) {
					_vizmap.addEdgeMapping(elt,
							new EdgeViz(null, elt.getName(), null, null, 0,
									null, null, null, true, null));
				} else { // get from old vizmap
					_vizmap.addEdgeMapping(elt, adjustColor(
							_oldvizmap.getEdgeViz(elt)));
				}
			}
		}
		return ret;
	}
	
	/**
	 * Returns a map of keys to values corresponding to an XML tag whose
	 * subtags are of the form <code>&lt;attribute name="key" value="value"&gt;
	 * </code>
	 * @param t the XML tag to be parsed
	 * @return a map of the keys to values in the attribute tags
	 * @throws PaletteXMLParseException if the children tags are not
	 *  attribute tags, or are missing the name or the value
	 */
	private Map<String, String> parseAttributes(XMLTag t)
	throws PaletteXMLParseException {
		Iterator i = t.getChildren().iterator();
		Map<String, String> ret = new HashMap<String, String>();
		while (i.hasNext()) {
			XMLTag att = new XMLTag((XMLElement)(i.next()), "attribute");
			String _attName = att.getAttribute("name");
			String _attValue = att.getAttribute("value");
			if (_attName == null) {
				throw new PaletteXMLParseException("Invalid attribute tag: "
						+ att.toString());
			}
			ret.put(_attName, _attValue);
		}
		return ret;
	}
	
	/**
	 * Builds an <code>EdgeViz</code> object based on the provided map
	 * and a default <code>EdgeViz</code> to inherit from.
	 * @param p the map that contains key/value pairs of properties
	 * @param _match the default <code>EdgeViz</code> to fall back onto
	 * @return a new <code>EdgeViz</code> object as specified in the
	 *  map, with missing properties inherited from <code>_match</code>
	 * @throws PaletteXMLParseException if the map contains inconsistent
	 *  properties
	 */
	private EdgeViz buildEdge(Map<String,String> p, EdgeViz _match)
	throws PaletteXMLParseException {
		Boolean visible_, attribute_, sameRank_;
		Boolean mergeArrows_, selected_, layoutBack_;
		String label_;
		DotColor color_;
		DotStyle style_;
		int weight_;
		populateEdgeVizMap(p, _match);
		visible_ = (Boolean)_resolve(p, VizResolver.VISIBLE, TRISTATE);
		attribute_ = (Boolean)_resolve(p, VizResolver.ATTRIBUTE, TRISTATE);
		sameRank_ = (Boolean)_resolve(p, VizResolver.SAME_RANK, TRISTATE);
		mergeArrows_ = (Boolean)_resolve(p, VizResolver.MERGE, TRISTATE);
		selected_ = (Boolean)_resolve(p, VizResolver.SELECTED, BOOLEAN);
		layoutBack_= (Boolean)_resolve(p, VizResolver.LAYOUT_BACK, TRISTATE);
		label_ = (String)_resolve(p, VizResolver.LABEL, STRING);
		weight_ = 0; // TODO
		color_ = (DotColor)_resolve(p, VizResolver.COLOR, DOTCOLOR_EDGE);
		style_ = (DotStyle)_resolve(p, VizResolver.STYLE, DOTSTYLE);
		// supply any missing information
		
		return new EdgeViz(visible_, label_, color_, style_, weight_,
				attribute_, sameRank_, mergeArrows_, selected_, layoutBack_);
	}

	/**
	 * Builds an <code>NodeViz</code> object based on the provided map
	 * and a default <code>NodeViz</code> to inherit from.
	 * @param p the map that contains key/value pairs of properties
	 * @param _match the default <code>NodeViz</code> to fall back onto
	 * @return a new <code>NodeViz</code> object as specified in the
	 *  map, with missing properties inherited from <code>_match</code>
	 * @throws PaletteXMLParseException if the map contains inconsistent
	 *  properties
	 */
	private NodeViz buildNode(Map<String,String> p, NodeViz _match)
	throws PaletteXMLParseException {
		Boolean visible_, sameRank_, showLabel_, selected_;
		Boolean showInAttr_, hideUnconnected_, numberAtoms_;
	    String label_;
	    DotColor color_;
	    DotShape shape_;
	    DotStyle style_;
	    // enter any missing information
	    populateNodeVizMap(p, _match);
	    visible_ = (Boolean)_resolve(p, VizResolver.VISIBLE, TRISTATE);
	    sameRank_ = (Boolean)_resolve(p, VizResolver.SAME_RANK, TRISTATE);
	    showLabel_ = (Boolean)_resolve(p, VizResolver.SHOW_LABEL, TRISTATE);
	    selected_ = (Boolean)_resolve(p, VizResolver.SELECTED, BOOLEAN);
	    showInAttr_ = (Boolean)_resolve(p, VizResolver.ATTRIBUTE, TRISTATE);
	    hideUnconnected_ = (Boolean)_resolve(p, VizResolver.HIDE_UNCONNECTED,
	    		TRISTATE);
	    numberAtoms_ = (Boolean)_resolve(p, VizResolver.NUMBER_ATOMS, TRISTATE);
	    label_ = (String)_resolve(p, VizResolver.LABEL, STRING);
	    color_ = (DotColor)_resolve(p, VizResolver.COLOR, DOTCOLOR_NODE);
		style_ = (DotStyle)_resolve(p, VizResolver.STYLE, DOTSTYLE);
		shape_ = (DotShape)_resolve(p, VizResolver.SHAPE, DOTSHAPE);
		return new NodeViz(visible_, label_, color_, shape_, style_,
				sameRank_, showLabel_, selected_, showInAttr_,
				hideUnconnected_, numberAtoms_);
	}
	
	/**
	 * Looks up the corresponding value of the given key <code>key</code>
	 * in the given map <code>m</code>, and converts the string description
	 * returned into the type specified by <code>valueType</code>.
	 * @param m the map that contains the string values
	 * @param key the key of the value being looked up
	 * @param valueType the resulting type
	 * @return the value of <code>key</code> in <code>m</code>, cast
	 *  appropriately into the type specified by <code>valueType</code>
	 * @throws PaletteXMLParseException if the value returned cannot
	 *  be converted into the specified type
	 */
	private Object _resolve(Map<String, String> m, String key,
			int valueType) throws PaletteXMLParseException {
		String value = m.get(key);
		if (value == null) return null;
		switch (valueType) {
		case BOOLEAN: 
			if (value.equals(TRISTATE_TRUE)) {
				return new Boolean(true);
			} else if (value.equals(TRISTATE_FALSE)) {
				return new Boolean(false);
			} else {
				throw new PaletteXMLParseException("Expected boolean value "
						+ "for attribute " + key + ", received " + value);
			}
		case TRISTATE:
			if (value.equals(TRISTATE_TRUE)) {
				return new Boolean(true);
			} else if (value.equals(TRISTATE_FALSE)) {
				return new Boolean(false);
			} else if (value.equals(TRISTATE_INHERIT)) {
				return null;
			} else {
				throw new PaletteXMLParseException("Expected boolean value "
						+ "for attribute " + key + ", received " + value);
			}
		case STRING:
			return value;
		case DOTCOLOR_NODE:
			List availableNodeColors = _colorResolver.getNodeColors(
					_newNodeTheme);
			for (int i = 0; i < availableNodeColors.size(); i++) {
				DotColor me = (DotColor)(availableNodeColors.get(i));
				if (me.toString().equals(value)) {
					return me;
				}
			}
			throw new PaletteXMLParseException("Cannot resolve the color name:"
					+ value);
		case DOTCOLOR_EDGE:
			List availableEdgeColors = _colorResolver.getNodeColors(
					_newEdgeTheme);
			for (int i = 0; i < availableEdgeColors.size(); i++) {
				DotColor me = (DotColor)(availableEdgeColors.get(i));
				if (me.toString().equals(value)) {
					return me;
				}
			}
			throw new PaletteXMLParseException("Cannot resolve the color name:"
					+ value);
		case DOTSTYLE:
			List availableStyles = DotStyle.getValidValues();
			for (int i = 0; i < availableStyles.size(); i++) {
				DotStyle me = (DotStyle)(availableStyles.get(i));
				if (me.toString().equals(value))
					return me;
			}
			throw new PaletteXMLParseException("Cannot resolve the style name:"
					+ value);
		case DOTSHAPE:
			List availableShapes = DotShape.getValidValues();
			for (int i = 0; i < availableShapes.size(); i++) {
				DotShape me = (DotShape)(availableShapes.get(i));
				if (me.toString().equals(value))
					return me;
			}
			throw new PaletteXMLParseException("Cannot resolve the shape name:"
					+ value);
		case INTEGER:
			return new Integer(Integer.parseInt(value));
		case DOTORIENTATION:
			List availableOrientations = DotOrientation.getValidValues();
			for (int i = 0; i < availableOrientations.size(); i++) {
				DotOrientation me = (DotOrientation)(availableOrientations.
						get(i));
				if (me.toString().equals(value))
					return me;
			}
			throw new PaletteXMLParseException("Cannot resolve the " +
					"orientation type:"	+ value);
		case DOTPALETTE:
			List availablePalettes = DotPalette.getValidValues();
			for (int i = 0; i < availablePalettes.size(); i++) {
				DotPalette me = (DotPalette)(availablePalettes.get(i));
				if (me.toString().equals(value))
					return me;
			}
			throw new PaletteXMLParseException("Cannot resolve the " +
					"palette name:"	+ value);
		default:
			PaletteXMLParserDbg.chk(false, "Invalid value type specified");
		}
		return null;
	}

	/**
	 * Returns the <code>AlloyRelation</code> object that matches the given
	 * name and type.
	 * @param _relname the name of the <code>AlloyRelation</code>
	 * @param _reltype a string that contains the type information:
	 *  a concatenation of the type names delimited by a whitespace
	 * @requires the relation name and type names must be fully qualified
	 * @return the corresponding <code>AlloyRelation</code> if one is found;
	 *  null if none is found, or if multiple matches are found
	 */
	private AlloyRelation _matchRelationName(String _relname, String _reltype)
	{
		if (_reltype == null || _relname == null ||
				!(_relname.contains(AlloyXMLParser._SEPARATOR))) return null;
		String _modulename = _relname.substring(0, _relname.lastIndexOf(
				AlloyXMLParser._SEPARATOR));
		AlloyModule am = _vizModel.getModuleByName(_modulename);
		if (am == null) return null;
		Iterator i = am.getRelations().iterator(); 
		boolean _matchAlreadyFound = false;
		
		AlloyRelation ret = null;
		while (i.hasNext()) {
			AlloyRelation t = (AlloyRelation)(i.next());
			String _name = t.getName().contains(AlloyXMLParser._SEPARATOR) ?
					t.getName() : (_modulename + AlloyXMLParser._SEPARATOR +
							t.getName());
			String _type = _typesToString(t.getTypes(), false);
			if (_relname.equals(_name) && _reltype.equals(_type)) {
				if (_matchAlreadyFound) return null;
				_matchAlreadyFound = true;
				ret = t;
			}
		}
		return ret;
	}
	
	//-------------------------------------------------------------------------
	// general helpers
	
	/**
	 * Concatenates the names of the <code>AlloyType</code> objects in the
	 * given list, separating them by a whitespace.
	 * @param _types the list that contains <code>AlloyType</code> objects
	 * @param useUnprojectedModel specifies whether to use the current
	 *  solution model, or to use the unprojected version of it
	 * @return a string obtained by concatenating the names, null if
	 *  any of the <code>AlloyType</code> objects cannot be resolved
	 */
	private String _typesToString(List _types, boolean useUnprojectedModel) {
		StringBuffer ret = new StringBuffer();
		for (int i = 0; i < _types.size(); i++) {
			String s = _getQualifiedTypeName((AlloyType)(_types.get(i)),
					useUnprojectedModel);
			if (s == null) return null;
			if (i != 0) ret.append(" ");
			ret.append(_getQualifiedTypeName((AlloyType)(_types.get(i)),
					useUnprojectedModel));
		}
		return ret.toString();
	}
	

	/**
	 * Maps a string produced by <code>_typesToString(List)</code> back
	 * into a list of <code>AlloyType</code> objects.
	 * @param a string to be parsed
	 * @return a list of <code>AlloyType</code> objects, or null if
	 *  any of the elements cannot be resolved
	 */
	private List _stringToTypes(String s) {
		List ret = new ArrayList();
		if (s == null) return ret;
		StringTokenizer sk = new StringTokenizer(s, " \n", false);
		while (sk.hasMoreTokens()) {
			String _next = sk.nextToken();
			if (_next == null || _next.length() == 0) continue;
			AlloyNodeElement a = _matchNodeName(_next);
			if (a == null) return null;
			ret.add(a);
		}
		return ret;
	}
	
	/**
	 * Returns the qualified name for the given AlloyType object.
	 * @param t the <code>AlloyType</code> object
	 * @param useUnprojectedModel specifies whether to use the default
	 *  solution model, or its unprojected version  
	 * @return the fully qualified name of <code>t</code>, or null
	 *  if <code>t</code> cannot be resolved in the <code>VizInstance</code>
	 *  object that the parser has been instantiated with
	 */
	private String _getQualifiedTypeName(AlloyType t,
			boolean useUnprojectedModel) {
		String name = t.getName();
		AlloyModule m = useUnprojectedModel ? _vizState.getUnprojectedModel()
				.getModuleOfType(t) : _vizModel.getModuleOfType(t);
		if (m == null) return null;
		if (name.contains(AlloyXMLParser._SEPARATOR)) return name;
		else return m.getName() + AlloyXMLParser._SEPARATOR + name;
	}
	

	/**
	 * Returns the <code>AlloyType</code> object that matches the given name.
	 * @param name the name of the <code>AlloyType</code> to be resolved
	 * @requires the name of the type must be fully qualified
	 * @return the corresponding <code>AlloyType</code> if one is found;
	 *  null if none is found, or if multiple matches are found
	 */
	private AlloyType _resolveAlloyType(String name) {
		if (name == null ||
				!(name.contains(AlloyXMLParser._SEPARATOR))) return null;
		String _modulename = name.substring(0, name.lastIndexOf(
				AlloyXMLParser._SEPARATOR));
		AlloyModule am = _vizModel.getModuleByName(_modulename);
		if (am == null) return null;
		boolean _matchAlreadyFound = false;
		Iterator i = am.getTypes().iterator();
		AlloyType ret = null;
		while (i.hasNext()) {
			AlloyType t = (AlloyType)(i.next());
			String _match = t.getName().contains(AlloyXMLParser._SEPARATOR) ?
					t.getName() : (_modulename + AlloyXMLParser._SEPARATOR +
							t.getName());
			if (name.equals(_match)) {
				if (_matchAlreadyFound) return null;
				_matchAlreadyFound = true;
				ret = t;
			}
		}
		return ret;
	}
	
	/**
	 * Returns the <code>AlloySet</code> object that matches the given name.
	 * @param name the name of the <code>AlloySet</code> to be resolved
	 * @requires the name of the set must be fully qualified
	 * @return the corresponding <code>AlloySet</code> if one is found;
	 *  null if none is found, or if multiple matches are found
	 */
	private AlloySet _resolveAlloySet(String name) {
		if (name == null ||
				!(name.contains(AlloyXMLParser._SEPARATOR))) return null;
		String _modulename = name.substring(0, name.lastIndexOf(
				AlloyXMLParser._SEPARATOR));
		AlloyModule am = _vizModel.getModuleByName(_modulename);
		if (am == null) return null;
		boolean _matchAlreadyFound = false;
		Iterator i = am.getSets().iterator();
		AlloySet ret = null;
		while (i.hasNext()) {
			AlloySet t = (AlloySet)(i.next());
			String _match = t.getName().contains(AlloyXMLParser._SEPARATOR) ?
					t.getName() : (_modulename + AlloyXMLParser._SEPARATOR +
							t.getName());
			if (name.equals(_match)) {
				if (_matchAlreadyFound) return null;
				_matchAlreadyFound = true;
				ret = t;
			}
		}
		return ret;
	}
	
	/**
	 * Returns the <code>AlloyNodeElement</code> object of given name
	 * @param _nodename the name of the object to be resolved
	 * @requires <code>_nodename</code> must be fully qualified
	 * @return the corresponding <code>AlloyNodeElement</code> if one
	 *  is found; null if none is found, or if multiple matches are found
	 */
	private AlloyNodeElement _matchNodeName(String _nodename) {
		AlloyType typeret = _resolveAlloyType(_nodename);
		AlloySet setret = _resolveAlloySet(_nodename);
		if (typeret != null) {
			if (setret != null) return null;
			return typeret;
		} else {
			if (setret != null) return setret;
			return null;
		}
	}
	
	/**
	 * Given a NodeViz object from the old view, it returns a copy of
	 * this NodeViz object, with the color adjusted to reflect the
	 * new color theme (i.e. oldNodeTheme ---> newNodeTheme) If a matching
	 * color for the new theme is not found, just stick with the old theme
	 */
	private NodeViz adjustColor(NodeViz old) {
		if (_oldNodeTheme == _newNodeTheme) {
			return old.copy();
		}
		DotColor oldC = old.getColor();
		if (oldC == null) return old.copy();
		DotColor newC = _colorResolver.findBestNodeMatch(_oldNodeTheme,
				_newNodeTheme, oldC);
		if (newC == null) return old.copy();
		return new NodeViz(old.isVisible(), old.getLabel(), newC,
				old.getShape(), old.getStyle(), old.isSameRank(),
				old.showLabel(), old.isSelected(), old.showInAttr(),
				old.hideUnconnected(), old.numberAtoms());
	}
	private EdgeViz adjustColor(EdgeViz old) {
		if (_oldEdgeTheme == _newEdgeTheme) {
			return old.copy();
		}
		DotColor oldC = old.getColor();
		if (oldC == null) return old.copy();
		DotColor newC = _colorResolver.findBestNodeMatch(_oldNodeTheme,
				_newNodeTheme, oldC);
		if (newC == null) return old.copy();
		return new EdgeViz(old.isVisible(), old.getLabel(),
				newC, old.getStyle(), old.getWeight(), old.isAttribute(),
				old.isSameRank(), old.mergeArrows(), old.isSelected(),
				old.layoutBack());
	}
	
	
	
	/**
	 * Retrieves the default node attributes associated with
	 * the default <code>View</code> object of this parser.
	 * @return the default node attributes, or null if the default
	 *  view does not exist
	 */
	private NodeViz retrieveGeneralNodeViz() {
		if (_current != null) {
			return _current.getGeneralView().getGeneralNodeViz();
		} else
			return (new GeneralView()).getGeneralNodeViz().copy();
	}
	
	/**
	 * Retrieves the node attributes associated with the given
	 * <code>AlloyNodeElement</code> object in the default <code>View</code>
	 * of this parser.
	 * @param elt the <code>AlloyNodeElement</code> being queried
	 * @return the associated node attributes, or null if the default
	 *  view does not exist or the attributes cannot be found
	 */
	private NodeViz retrieveNodeViz(AlloyNodeElement elt) {
		if (_current != null && elt != null) {
			return _current.getModelView().getVizMap().getNodeViz(elt);
		} else
			return null;
	}
	
	/**
	 * Retrieves the default edge attributes associated with
	 * the default <code>View</code> object of this parser.
	 * @return the default edge attributes, or null if the default
	 *  view does not exist
	 */
	private EdgeViz retrieveGeneralEdgeViz() {
		if (_current != null) {
			return _current.getGeneralView().getGeneralEdgeViz();
		} else
			return (new GeneralView()).getGeneralEdgeViz().copy();
	}
	
	/**
	 * Retrieves the edge attributes associated with the given
	 * <code>AlloyRelation</code> object in the default <code>View</code>
	 * of this parser.
	 * @param elt the <code>AlloyRelation</code> being queried
	 * @return the associated edge attributes, or null if the default
	 *  view does not exist or the attributes cannot be found
	 */
	private EdgeViz retrieveEdgeViz(AlloyRelation elt) {
		if (_current != null && elt != null) {
			return _current.getModelView().getVizMap().getEdgeViz(elt);
		} else
			return null;
	}
	
	/**
	 * Returns the string representation of the argument, as used in the
	 * parser.
	 * @param o the object to be turned into string
	 * @return the string representation of the object as used in the parser,
	 * 	or null if the object type cannot be resolved
	 */
	private String _resolveBack(Object o) {
		if (o instanceof Boolean) {
			if (o == null) return TRISTATE_INHERIT;
			else if (((Boolean)o).equals(Boolean.TRUE)) return TRISTATE_TRUE;
			else return TRISTATE_FALSE;
		} else if (o instanceof String) {
			return (String)o;
		} else if (o instanceof DotProperty) {
			return ((DotProperty)o).toString();
		} else if (o instanceof Integer) {
			return ((Integer)o).toString();
		}
		return null;
	}
	
	/**
	 * Populates the given map with attributes of the given
	 * <code>NodeViz</code> object, if those attributes are missing
	 * in the map.
	 * @param v the map to be populated
	 * @param old the <code>NodeViz</code> object to be used as the template
	 */
	private void populateNodeVizMap(Map<String,String> v, NodeViz old) {
		if (old == null) return;
	    if (v.get(VizResolver.VISIBLE) == null)
	    	v.put(VizResolver.VISIBLE, _resolveBack(old.isVisible()));
	    if (v.get(VizResolver.SAME_RANK) == null)
	    	v.put(VizResolver.SAME_RANK, _resolveBack(old.isSameRank()));
	    if (v.get(VizResolver.SHOW_LABEL) == null)
	    	v.put(VizResolver.SHOW_LABEL, _resolveBack(old.showLabel()));
	    if (v.get(VizResolver.SELECTED) == null)
	    	v.put(VizResolver.SELECTED, _resolveBack(old.isSelected()));
	    if (v.get(VizResolver.ATTRIBUTE) == null)
	    	v.put(VizResolver.ATTRIBUTE, _resolveBack(old.isSelected()));
	    if (v.get(VizResolver.HIDE_UNCONNECTED) == null)
	    	v.put(VizResolver.HIDE_UNCONNECTED,
	    			_resolveBack(old.hideUnconnected()));
	    if (v.get(VizResolver.NUMBER_ATOMS) == null)
	    	v.put(VizResolver.NUMBER_ATOMS, _resolveBack(old.numberAtoms()));
	    if (v.get(VizResolver.LABEL) == null)
	    	v.put(VizResolver.LABEL, _resolveBack(old.getLabel()));
	    if (v.get(VizResolver.COLOR) == null)
	    	v.put(VizResolver.COLOR, _resolveBack(old.getColor()));
	    if (v.get(VizResolver.STYLE) == null)
	    	v.put(VizResolver.STYLE, _resolveBack(old.getStyle()));
	    if (v.get(VizResolver.SHAPE) == null)
	    	v.put(VizResolver.SHAPE, _resolveBack(old.getShape()));
	}
	
	/**
	 * Populates the given map with attributes of the given
	 * <code>EdgeViz</code> object, if those attributes are missing
	 * in the map.
	 * @param v the map to be populated
	 * @param old the <code>EdgeViz</code> object to be used as the template
	 */
	private void populateEdgeVizMap(Map<String,String> v, EdgeViz old) {
		if (old == null) return;
	    if (v.get(VizResolver.VISIBLE) == null)
	    	v.put(VizResolver.VISIBLE, _resolveBack(old.isVisible()));
	    if (v.get(VizResolver.SAME_RANK) == null)
	    	v.put(VizResolver.SAME_RANK, _resolveBack(old.isSameRank()));
	    if (v.get(VizResolver.SELECTED) == null)
	    	v.put(VizResolver.SELECTED, _resolveBack(old.isSelected()));
	    if (v.get(VizResolver.ATTRIBUTE) == null)
	    	v.put(VizResolver.ATTRIBUTE, _resolveBack(old.isAttribute()));
	    if (v.get(VizResolver.MERGE) == null)
	    	v.put(VizResolver.MERGE, _resolveBack(old.mergeArrows()));
	    if (v.get(VizResolver.LAYOUT_BACK) == null)
	    	v.put(VizResolver.LAYOUT_BACK, _resolveBack(old.layoutBack()));
	    if (v.get(VizResolver.LABEL) == null)
	    	v.put(VizResolver.LABEL, _resolveBack(old.getLabel()));
	    if (v.get(VizResolver.COLOR) == null)
	    	v.put(VizResolver.COLOR, _resolveBack(old.getColor()));
	    if (v.get(VizResolver.STYLE) == null)
	    	v.put(VizResolver.STYLE, _resolveBack(old.getStyle()));
	}
	
	/**
	 * Populates the given <code>GeneralView</code> object with settings
	 * from a <global> tag
	 * @param g the <code>GeneralView</code> object to be populated
	 * @param x the XML tag that contains the global setting
	 */
	private void populateGlobalViewSettings(GeneralView g, XMLTag x)
	throws PaletteXMLParseException {
		String fontName = null;
		Integer fontSize = null;
		DotOrientation orientation = null;
		DotPalette edgePalette = null;
		DotPalette nodePalette = null;
		Map<String, String> _globalOp = 
			parseAttributes(x);
		fontName = (String)_resolve(_globalOp, "font name", STRING);
		fontSize = ((Integer)_resolve(_globalOp, "font size",
				INTEGER));
		orientation = (DotOrientation)_resolve(_globalOp,
				"orientation", DOTORIENTATION);
	    nodePalette = (DotPalette)_resolve(_globalOp, "node theme",
	    		DOTPALETTE);
	    edgePalette = (DotPalette)_resolve(_globalOp, "edge theme",
	    		DOTPALETTE);
	    if (edgePalette != null) {
	    	g.setEdgePalette(edgePalette);
	    	_newEdgeTheme = edgePalette;
	    }
		if (nodePalette != null) { 
			g.setNodePalette(nodePalette);
			_newNodeTheme = nodePalette;
		}
		if (fontName != null) g.setGeneralFontName(fontName);
		if (fontSize != null) g.setGeneralFontSize(fontSize.intValue());
		if (orientation != null) g.setGeneralOrientation(orientation);
	}
	
	/**
	 * A simple helper to parse a file into string
	 */
	public static String fileToString(File f) {
		try {
			StringBuffer sb = new StringBuffer();
			BufferedReader br = new BufferedReader(new FileReader(f));
			String line;
			while ((line = br.readLine()) != null) {
				sb.append(line);
				sb.append(System.getProperty("line.separator"));
			}
			return sb.toString();
		} catch (IOException e) {
			return null;
		}
	}
}
