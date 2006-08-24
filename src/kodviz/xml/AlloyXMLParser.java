package kodviz.xml;

import java.io.File;
import java.io.FileNotFoundException;
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

import kodviz.alloyviz.AlloyAtom;
import kodviz.alloyviz.AlloyModule;
import kodviz.alloyviz.AlloyRelation;
import kodviz.alloyviz.AlloySet;
import kodviz.alloyviz.AlloyTuple;
import kodviz.alloyviz.AlloyType;
import kodviz.alloyviz.Model;
import kodviz.alloyviz.TypeStructure;
import kodviz.alloyviz.VizInstance;

/**
 * processes an XML document that contains an instance of a solution for an
 * Alloy model. Refer to alloyxml.xsd for the exact XML Schema.
 * 
 * Some expected conventions in XML tags:
 * 
 * 1) The module name is always fully qualified.
 * 2) Signature name is always NOT qualified. However, parent signature names
 * can either be fully qualified or not qualified at all.
 * 
 * @author jbaek
 */

/*
 * The main method of the class is parseAlloyXML. What happens is the
 * following:
 * 
 * For each module, the relevant signature, sets, relation names are read,
 * along with the atom names, tuples, etc. However, the corresponding
 * ADT structures in Alloy, such as AlloyAtom, AlloyType, AlloyModule are
 * not created. The reason is that their constructors require information
 * that might not have been parsed yet. (For more information,
 * refer to their constructors.)
 * 
 * After all modules have been thus "processed," in parseAlloyXML,
 * all the information that has been read (stored as strings, mostly)
 * is then converted into Alloy ADT.
 */
@SuppressWarnings("unchecked")
public class AlloyXMLParser {

	// --------------- Alloy ADT
	private TypeStructure _ts;
	
	// --------------- intermediate data structures
	/* All names used in intermediate data structures listed below are
	 * fully qualified, with the exception of atom names.
	 * 
	 * relations are indexed by numbers. This is somewhat weird, but
	 * because relations can have the same names within the same module,
	 * if they come from a union type.
	 */
	private Map<String, AlloyModule> _moduleNames; // tracks module names
	private Map<String, AlloyType> _typeNames; // tracks sig names
	// tracks atom names, but it is not populated till the very end
	private Map<String, AlloyAtom> _atomNames;
	// maps an atom name to its type and vice versa
	private Map<String, AlloyType> _atomtotype;
	private Map<AlloyType, List<String>> _typetoatom;
	// maps a relation index to the name of the relation
	private Map<Integer, String> _relindextoname;
	// maps relation index to a set of tuples (list of atom names)
	private Map<Integer, Set<List>> _reltotuple;
	// maps relation index to a set of types in the tuples
	private Map<Integer, List<String>> _reltotype;
	// maps relation index to its owner module name
	private Map<Integer, String> _reltomodule;
	// maps module name to the sigs defined in that module
	private Map<String, Set<AlloyType>> _moduletotype;
	// maps a type name to the set of its subtypes for deferred operations
	private Map<String, Set<AlloyType>> _deferredSuperType;
	private int _currentRelationNumber = 0;
	// maps a subset name to the set of atoms in that subset
	private Map<String, Set<String>> _subsettoatom;
	private Map<String, String> _subsettomodule;
	private String _rootModuleName;
	
	public static String _SEPARATOR = "/";
	public static final String UNIV_SIG_NAME = "alloy/lang/univ/univ";
	public static final String UNIV_MODULE_NAME = "alloy/lang/univ";
	
	/*
	 * Contracts: 
	 * _reltotuple.keyset() must equal to _reltotype.keyset(); they both track
	 * the existing set of relations.
	 * _moduleNames.keyset() equals _moduletotype.keyset()
	 */
	
	/*
	 * internal data structure; wraps around an XML tag. It is used to
	 * facilitate tag validation.
	 */ 
	private static class AlloyTag {
		String name;
		private XMLElement source;
		/**
		 * Constructs a new AlloyTag instance, and checks if the XML element
		 * matches the provided tag type.
		 * @param x the XML Element to be wrapped
		 * @param expected_tag the expected top-level tag
		 * @throws AlloyXMLParseException if the top-level tag does not match
		 * the provided tag name, or if the XML element does not have
		 * attribute "name."
		 */
		AlloyTag(XMLElement x, String expected_tag)
		throws AlloyXMLParseException {
			name = _verifyAndExtractAttrib(x, "name");
			AlloyTag._verifyTopLevelTag(x, expected_tag);
			source = x;
		}
		/**
		 * Returns the children tags
		 * @return a vector containing the children tags. Each element of the
		 *  vector is of the original XML parser type.
		 */
		Vector getChildren() { return source.getChildren(); }
		/**
		 * Returns the specified attribute.
		 * @param attribute the name of the attribute
		 * @return the value of the attribute if it exists, null otherwise
		 */
		Object getAttribute(String attribute) {
			return source.getAttribute(attribute);
		}
		/**
		 * Verifies that the given XML tag has the specified top-level tag.
		 * @param e the XMLElement representing the XML tag to be checked
		 * @param expected_name the expected name of the tag
		 * @throws AlloyXMLParseException if <code>e.getName()</code> is not
		 *  equal to <code>expected_name</code> 
		 */
		private static void _verifyTopLevelTag(XMLElement e,
				String expected_name) throws AlloyXMLParseException {
			if (e.getName() == null || !e.getName().equals(expected_name))
				throw new AlloyXMLParseException("Expected a(n) " +
						expected_name + " tag, found " + e.getName() +
						" instead.");
		}
	}
	
	/**
	 * Constructs a new instance of AlloyXMLParser and initializes
	 * the intermediate data structures.
	 */
	public AlloyXMLParser() {
		_moduleNames = new HashMap<String, AlloyModule>();
		_typeNames = new HashMap<String, AlloyType>();
		_atomNames = new HashMap<String, AlloyAtom>();
		_atomtotype = new HashMap<String, AlloyType>();
		_typetoatom = new HashMap<AlloyType, List<String>>();
		_moduletotype = new HashMap<String, Set<AlloyType>>();
		_relindextoname = new HashMap<Integer, String>();
		_reltomodule = new HashMap<Integer, String>();
		_reltotuple = new HashMap<Integer, Set<List>>();
		_reltotype = new HashMap<Integer, List<String>>();
		_deferredSuperType = new HashMap<String, Set<AlloyType>>();
		_currentRelationNumber = 0;
		_subsettoatom = new HashMap<String, Set<String>>();
		_subsettomodule = new HashMap<String, String>();
	}
	
	/**
	 * Prepares the current instance of AlloyXMLParser to process
	 * another Alloy XML document; clears all intermediate data
	 * structures.
	 */
	private void clear() {
		_ts = new TypeStructure();
		_moduleNames.clear();
		_typeNames.clear();
		_atomNames.clear();
		_atomtotype.clear();
		_moduletotype.clear();
		_relindextoname.clear();
		_reltomodule.clear();
		_reltotuple.clear();
		_reltotype.clear();			
		_typetoatom.clear();
		_deferredSuperType.clear();
		_currentRelationNumber = 0;
		_subsettoatom.clear();
		_subsettomodule.clear();
	}
	
	/**
	 * Produces and returns a VizInstance object corresponding to the
	 * solution in the specified text (which is parsed as XML document)
	 * 
	 * @param t the string object containing XML tags 
	 * @return a VizInstance object representing the solution in the string
	 */
	public static VizInstance readXMLString(String t) {
		VizInstance ret = null;
		try {
			XMLElement xml = new XMLElement();
			xml.parseString(t);
			AlloyXMLParser x = new AlloyXMLParser();
			ret = x.parseAlloyXML(xml);
		} catch (XMLParseException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		} catch (AlloyXMLParseException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		return ret;
	}
	
	/**
	 * Produces and returns a VizInstance object corresponding to the
	 * solution in the specified Alloy XML file.
	 * 
	 * @param filename the name of the local XML file to be read 
	 * @return a VizInstance object representing the solution in the XML file
	 */
	public static VizInstance readXMLfile(File f) {
		VizInstance ret = null;
		if (f == null) return ret;
		try {
			FileReader reader = new FileReader(f);
			XMLElement xml = new XMLElement();
			xml.parseFromReader(reader);
			AlloyXMLParser x = new AlloyXMLParser();
			ret = x.parseAlloyXML(xml);
		} catch (FileNotFoundException e) { // TODO throw new exceptions
			System.out.println(e.getMessage());
			e.printStackTrace();
		} catch (IOException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		} catch (XMLParseException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		} catch (AlloyXMLParseException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		return ret;
	}
	
	/**
	 * Verifies that the given attribute exists in an XML tag, and returns
	 * the value of the said attribute.
	 * @param e the XMLElement object holding the XML tag to be processed
	 * @param attrib_name the name of the attribute
	 * @return the value of the said attribute, if it exists and is not empty
	 * @throws AlloyXMLParseException if the said attribute does not exist,
	 *  or the value is empty.
	 */
	private static String _verifyAndExtractAttrib(XMLElement e,
			String attrib_name) throws AlloyXMLParseException {
		String val = (String)(e.getAttribute(attrib_name));
		if (val == null || val.equals(""))
			throw new AlloyXMLParseException("Failed to find a(n) " +
					attrib_name + " attribute inside " + e.getName() +
					" tag.");
		return val;
	}
	
	/**
	 * Processes an XML tag containing an Alloy model instance, and returns
	 * a VizInstance object for the visualizer.
	 * 
	 * @param x the XMLElement corresponding to the Alloy XML document
	 * @return a VizInstance object representing the Alloy model instance
	 * @effect reinitializes all maps in this instance of AlloyXMLParser,
	 *  and populates it.
	 */
	private VizInstance parseAlloyXML(XMLElement x)
	throws AlloyXMLParseException, AlloyXMLParserError {
		// clears all arrays and data structures
		clear();
		AlloyTag root = new AlloyTag(x, "solution");
		_rootModuleName = root.name;
		Iterator modIte = root.getChildren().iterator();
		List modulelist = new ArrayList(); // list of modules parsed
		// run through children tags of the top-level tag
		while (modIte.hasNext()) {
			XMLElement mod = (XMLElement)(modIte.next());
			if (mod.getName().equals("skolem")) { // found skolem tag
				AlloyTag at = new AlloyTag(mod, "skolem");
				parseAlloySkolem(at);
			} else { // expecting a module tag
				AlloyTag at = new AlloyTag(mod, "module");
				AlloyModule am = parseAlloyModule(at);
				if (am != null) modulelist.add(am); 
			}
		}
		// check to see if the root module exists
		if (_moduleNames.get(_rootModuleName) == null)
			throw new AlloyXMLParseException("Cannot find the main module: "
					+ root.name);
		
		// check for unresolved supertypes
		if (_deferredSuperType.keySet().size() > 0)
			throw new AlloyXMLParseException("Found unresolved supertypes: "
					+ _deferredSuperType.keySet());
		
		// create alloy viz structures
		Model mymodel = new Model(modulelist, _ts, root.name);
		VizInstance myinstance = new VizInstance(mymodel);
		
		_atomNames.clear();
		Iterator typeIte = _typetoatom.keySet().iterator();
		while (typeIte.hasNext()) {
			AlloyType at = (AlloyType)(typeIte.next());
			Iterator atomIte = _typetoatom.get(at).iterator();
			for (int i = 0; atomIte.hasNext(); i++) {
				String name = (String)(atomIte.next());
				AlloyAtom aa = new AlloyAtom(name, at, i);
				myinstance.addAtom(aa);
				_atomNames.put(name, aa);
			}
		}

		// form and add relations
		Iterator relIte = _relindextoname.keySet().iterator();
		while (relIte.hasNext()) {
			Integer _relationIndex = (Integer)(relIte.next());
			String _relationName = _relindextoname.get(_relationIndex);
			List _typeNameList = _mapStringsToTypes(_reltotype.get(
					_relationIndex));
			AlloyRelation ar = new AlloyRelation(_retrieveShortName(
					_relationName),	_typeNameList);
			AlloyXMLParserDbg.chk(_moduleNames.containsKey(
					_reltomodule.get(_relationIndex)));
			myinstance.addRelation(ar, _reltomodule.get(_relationIndex));
			// now form add tuples
			Iterator tupIte = _reltotuple.get(_relationIndex).iterator();
			while (tupIte.hasNext()) {
				List _newTuple = (List)(tupIte.next());
				myinstance.addTupleToRelation(new AlloyTuple(
						_mapStringsToAtoms(_newTuple)), ar);
			}
		}
		
		// form and add sets
		Iterator setIte = _subsettoatom.keySet().iterator();
		while (setIte.hasNext()) {
			String _setName = (String)(setIte.next());
			AlloySet as = new AlloySet(_retrieveShortName(_setName),
					_resolveSignatureName(UNIV_SIG_NAME));
			myinstance.addSet(as, _subsettomodule.get(_setName));
			Iterator atomIte = _subsettoatom.get(_setName).iterator();
			while (atomIte.hasNext()) {
				AlloyAtom a = _atomNames.get((String)(atomIte.next()));
				if (a == null) throw new AlloyXMLParserError();
				myinstance.addAtomToSet(a, as);
			}
		}
		return myinstance;
	}
	

	/**
	 * Processes an AlloyTag instance that contains an XML tag corresponding
	 * to a module, and creates an appropriate AlloyModule object.
	 * @param x the AlloyTag wrapper around an XML tag
	 * @return an AlloyModule object
	 * @throws AlloyXMLParseException if the tag is not a well-formed Alloy
	 *  XML tag that corresponds to a module
	 * @effect populates the internal data structures of AlloyXMLParser as it
	 *  recursively walks through the children tags
	 * @throws AlloyXMLParseException if the XML element does not represent a
	 *  well-formed Alloy module
	 */
	private AlloyModule parseAlloyModule(AlloyTag x)
	throws AlloyXMLParseException {
		String module_name = x.name;
		Iterator ite = x.getChildren().iterator();
		//if (!ite.hasNext()) // empty module
		//	return null;
		// check if this is a duplicate module
		if (_moduleNames.keySet().contains(module_name))
			throw new AlloyXMLParseException("Duplicate module name found!");
		else { // update internal structures
			_moduleNames.put(module_name, null);
			_moduletotype.put(module_name, new HashSet<AlloyType>());
		}

		// walks through the children tags
		while (ite.hasNext()) {
			XMLElement elm = (XMLElement)(ite.next());
			AlloyTag at = new AlloyTag(elm, "sig");
			parseAlloySig(at, module_name);
		}
		List _types = new ArrayList(_moduletotype.get(module_name));
		AlloyModule ret = new AlloyModule(module_name, _types,
				new ArrayList(), new ArrayList());
		_moduleNames.put(module_name, ret);
		return ret;
	}
	
	/**
	 * Processes an AlloyTag instance that contains an XML tag corresponding
	 * to a signature, and creates an appropriate AlloySig object.
	 * @param x the AlloyTag wrapper around an XML tag
	 * @return an AlloySig object
	 * @throws AlloyXMLParseException if the tag is not a well-formed Alloy
	 *  XML tag that corresponds to a signature
	 * @effect populates the internal data structures of AlloyXMLParser as it
	 *  recursively walks through the children tags
	 * @throws AlloyXMLParseException if the XML element does not represent a
	 *  well-formed Alloy signature 
	 */
	private AlloyType parseAlloySig(AlloyTag x, String ownerModule)
	throws AlloyXMLParseException, AlloyXMLParserError {
		// set up signature names
		String sig_name = x.name;
		String full_name = x.name;
		if (!sig_name.contains(_SEPARATOR))
			full_name = ownerModule + _SEPARATOR + sig_name;
		String parent_sig_name = (String)(x.getAttribute("extends"));
		AlloyType _new = null;
		
		// check if the signature is already defined
		if (_typeNames.containsKey(full_name))
			throw new AlloyXMLParseException("Duplicate signature found!");
		
		// take a peek at the children tags to see if this is a subset.
		// otherwise go ahead and create sig.
		XMLElement first_child;
		boolean _isSubset = false;
		if (x.getChildren().size() > 0 &&
				(first_child = (XMLElement)(x.getChildren().get(0)))
				!= null && first_child.getName().equals("in")) {
			_isSubset = true;
			_subsettoatom.put(full_name, new HashSet<String>());
			_subsettomodule.put(full_name, ownerModule);
		} else {
			_new = new AlloyType(sig_name);
			// update internal structures
			AlloyXMLParserDbg.chk(_moduletotype.get(ownerModule));
			_typeNames.put(full_name, _new);
			_moduletotype.get(ownerModule).add(_new);
			_typetoatom.put(_new, new ArrayList<String>());
			_ts.addType(_new);
	
			// check for deferred subtyping
			if (_deferredSuperType.get(full_name) != null) {
				Iterator subtypes = _deferredSuperType.get(
						full_name).iterator();
				while (subtypes.hasNext()) {
					_ts.addSuper(_new, (AlloyType)(subtypes.next()));
				}
				_deferredSuperType.remove(full_name);
			}
			
			// If the supertype of this new sig has not been defined, defer it
			if (parent_sig_name != null && !parent_sig_name.equals("")) {
				AlloyType parent_sig = _resolveSignatureName(parent_sig_name);
				if (parent_sig != null) {
					_ts.addSuper(parent_sig, _new);
				} else { // supertype not declared yet--add to deferred task
					if (_deferredSuperType.get(parent_sig_name) == null)
						_deferredSuperType.put(parent_sig_name,
								new HashSet<AlloyType>());
					(_deferredSuperType.get(parent_sig_name)).add(_new);
				}
			}
		}
		
		// now parse atoms and fields from children tags
		Iterator ite = x.getChildren().iterator();
		boolean _processed_subset = false;
		while (ite.hasNext()) {
			XMLElement elm = (XMLElement)(ite.next());
			if (elm.getName().equals("in")) { // found subset
				if (_processed_subset)
					throw new AlloyXMLParseException("'in' tags must be " +
							"defined in the beginning of " + x.name);
				/*AlloyTag at =*/ new AlloyTag(elm, "in");
			} else if (elm.getName().equals("atom")) { // found atom
				AlloyTag at = new AlloyTag(elm, "atom");
				parseAlloyAtom(at, _new);
				if (_isSubset) {
					_subsettoatom.get(full_name).add(at.name);
				}
				_processed_subset = true;
			} else if (elm.getName().equals("field")) { // found relation
				AlloyTag at = new AlloyTag(elm, "field");
				parseAlloyRelation(at, _isSubset ? UNIV_SIG_NAME :
					full_name, _isSubset? UNIV_MODULE_NAME : ownerModule);
				_processed_subset = true;
			} else { // error?
				throw new AlloyXMLParseException("Can't recognize the tag "
						+ elm.getName() + "inside a sig tag.");
			}
		}
		return _new;
	}
	
	/**
	 * Processes an AlloyTag wrapping an XML element corresponding to an atom.
	 * @param x the AlloyTag wrapping the XML element
	 * @param a the type of the given atom; if null,
	 *  <code>UNIV_SIG_NAME</code> is assumed, and the atom is ignored.
	 * @effect updates the internal data structure <code>_atomtotype</code>
	 * and <code>_typetoatom</code>. If the atom already exists, update it
	 * if necessary.
	 */
	private void parseAlloyAtom(AlloyTag x, AlloyType a) {
		// Check for existing atom.
		if (a == null) return;
		AlloyType existing_type = _atomtotype.get(x.name);
		if (existing_type != null) {
			if (_isSuperType(a, existing_type)) {
				return; // already subtyped
			} else {
				// overwrite
				_atomtotype.put(x.name, a);
				_typetoatom.get(existing_type).remove(x.name);
				_typetoatom.get(a).add(x.name);
			}
		} else { // new atom
			_atomtotype.put(x.name, a);
			_typetoatom.get(a).add(x.name);
		}
	}
	
	/**
	 * Processes an AlloyTag wrapping an XML element corresponding to an
	 * Alloy relation.
	 * @param x the AlloyTag wrapping the XML element
	 * @param ownerType the name of the type that occurs as the first element
	 *  in the tuple
	 * @param ownerModule the module that contains this relation
	 * @effect updates the internal data structures to reflect the existence
	 *  of the particular relation contained in the AlloyTag object
	 * @throws AlloyXMLParseException if the XML element does not represent a
	 *  well-formed AlloyRelation object
	 */
	private void parseAlloyRelation(AlloyTag x, String ownerType,
			String ownerModule) throws AlloyXMLParseException {
		int arity = Integer.parseInt((String)(x.getAttribute("arity")));
		if (arity < 1) {
			throw new AlloyXMLParseException("Invalid arity used in " + x.name);
		}

		Iterator ite = x.getChildren().iterator();
		boolean _type_decl_found = false;
		String full_name = ownerModule + _SEPARATOR + x.name;
		Integer _currentIndex = null;
		
		while (ite.hasNext()) {
			XMLElement elm = (XMLElement)(ite.next());
			if (elm.getName().equals("type")) { // found type declaration
				_type_decl_found = true;
				String content = elm.getContent();
				List<String>_type = _parseRelationTypeDeclaration(content);
				_type.add(0, ownerType);
				if (_type.size() != arity) {
					System.out.println(_type);
					throw new AlloyXMLParseException("Declared arity (" +
							arity + ") does not match the number of types" +
							" listed in " + x.name + "(" + _type.size() + ")");
				}
				_currentIndex = new Integer(++_currentRelationNumber);
				_relindextoname.put(_currentIndex, full_name);
				_reltomodule.put(_currentIndex, ownerModule);
				_reltotype.put(_currentIndex, _type);
				_reltotuple.put(_currentIndex, new HashSet<List>());
			} else if (elm.getName().equals("tuple")) { // found tuple
				if (!_type_decl_found)
					throw new AlloyXMLParseException("Tuples declared before" +
							"a proper type declaration in relation " + x.name);
				Iterator subite = elm.getChildren().iterator();
				List atmlist = new ArrayList();
				while (subite.hasNext()) {
					XMLElement subelm = (XMLElement)(subite.next());
					atmlist.add((new AlloyTag(subelm, "atom")).name);
				}
				if (atmlist.size() != arity)
					throw new AlloyXMLParseException("Declared arity does " +
							"not match the number of atoms listed in " +
							x.name);
				_reltotuple.get(_currentIndex).add(atmlist);
			} else { // error
				throw new AlloyXMLParseException("Can't recognize the tag " +
						elm.getName() + "inside a field tag.");
			}
		}
		if (!_type_decl_found) { // type not found
			throw new AlloyXMLParseException("No type declaration found " +
					"for " + x.name);
		}
	}
	
	/**
	 * Processes an AlloyTag wrapping an XML element corresponding to an
	 * skolem constant
	 * @param x the AlloyTag wrapping the XML element
	 * @effect updates the internal data structures to reflect the existence
	 *  of the particular skolem constant. A single-arity skolem constant
	 *  is added as a set, and the rest as relations.
	 * @throws AlloyXMLParseException if the XML element does not represent a
	 *  well-formed AlloyRelation object
	 */
	private void parseAlloySkolem(AlloyTag x)
	throws AlloyXMLParseException {
		String _name = x.name; // skolem name
		Iterator tupIte = x.getChildren().iterator();
		int arity = -1;
		Set tuplelist = new HashSet();
		while (tupIte.hasNext()) {
			XMLElement elm = (XMLElement)(tupIte.next());
			if (!elm.getName().equals("tuple"))
				throw new AlloyXMLParseException("Encountered unknown tag in"
						+ " a skolem constant: " + elm.getName());
			Iterator subite = elm.getChildren().iterator();
			List<String> atmlist = new ArrayList<String>();
			while (subite.hasNext()) {
				XMLElement subelm = (XMLElement)(subite.next());
				atmlist.add((new AlloyTag(subelm, "atom")).name);
			}
			if (arity > 0 && arity != atmlist.size()) {
				throw new AlloyXMLParseException("Inconsistent arity in " +
						"skolem constant " + _name);
			}
			arity = atmlist.size();
			tuplelist.add(atmlist);
		}
		if (arity == -1) return;
		if (arity == 1) { // treat as sets
			_subsettomodule.put(x.name, _rootModuleName);
			tupIte = tuplelist.iterator();
			Set<String> ret = new HashSet<String>();
			while (tupIte.hasNext()) {
				ret.add((String)(((List)(tupIte.next())).get(0)));
			}
			_subsettoatom.put(x.name, ret);
		} else {
			Integer i = new Integer(++_currentRelationNumber);
			_relindextoname.put(i, _name);
			_reltomodule.put(i, _rootModuleName);
			List<String> _type = new ArrayList<String>();
			for (int j = 0; j < arity; j++, _type.add(UNIV_SIG_NAME));
			_reltotype.put(i, _type);
			_reltotuple.put(i, tuplelist);
		}
		
	}
	
	/**
	 * Checks whether a given AlloyType is a supertype of another.
	 * @param a an AlloyType 
	 * @param b an AlloyType
	 * @return true if <code>a</code> is a supertype of <code>b</code>
	 *  according to the internal data structures; false otherwise
	 */ 
	private boolean _isSuperType(AlloyType a, AlloyType b) {
		if (a == null || b == null) return false;
		Set sub_a = _ts.getSubTypes(a);
		if (sub_a != null && sub_a.contains(b)) return true;
		return false;
	}
	
	/**
	 * Returns a non-qualified name without the path information.
	 * @param _fullName the string containing the fully qualified name
	 * @return a substring of <code>_fullName</code> that comes after
	 *  the last occurrence of the path separator.
	 */
	public static String _retrieveShortName(String _fullName) {
		if (_fullName == null) return null;
		int lastIndex = _fullName.lastIndexOf(_SEPARATOR);
		if (lastIndex == -1) return _fullName;
		return _fullName.substring(lastIndex + 1);
	}
	
	/**
	 * Parses a string that contains a declaration of a relation type, and
	 * returns a list of type names in the correct order.
	 * @param str the string to be parsed
	 * @return a list of the type names in the declaration
	 */
	private List<String> _parseRelationTypeDeclaration(String str)
	throws AlloyXMLParseException {
		String[] _keywords = {"set", "lone", "one", "some"};
		StringTokenizer x = new StringTokenizer(str, " ()", false);
		List<String> _typeList = new ArrayList<String>();
		while (x.hasMoreTokens()) {
			String tmp = x.nextToken();
			boolean _ignore = false;
			if (tmp.contains("->")) continue;
			for (int i = 0; i < _keywords.length; i++)
				if (tmp.equals(_keywords[i])) { _ignore = true; break; }
			if (!_ignore) _typeList.add(tmp);
		}
		return _typeList;
	}
	
	/**
	 * Takes a list of atom names, and returns a list containing the
	 * corresponding AlloyAtom objects in the same order. This requires
	 * that the internal structure <code>_atomNames</code> has already
	 * been populated from an XML document.
	 * @param l the list of atom names
	 * @return a list of corresponding AlloyAtoms
	 */
	@SuppressWarnings("unchecked")
	private List _mapStringsToAtoms(List<String> l)
	throws AlloyXMLParseException {
		List atms = new ArrayList();
		for (int i = 0; i < l.size(); i++) {
			if (_atomNames.get(l.get(i)) != null)
				atms.add(_atomNames.get(l.get(i)));
			else {
				// atom name not resolved
				throw new AlloyXMLParseException("Atom " + l.get(i) +
						" cannot be resolved");
			}
		}
		return atms;
	}
	
	/**
	 * Takes a list of signature names, and returns a list containing the
	 * corresponding AlloyType objects in the same order. This requires
	 * that the internal structure <code>_typeNames</code> has already
	 * been populated from an XML document.
	 * @param l the list of signature names
	 * @return a list of corresponding AlloyTypes
	 */
	@SuppressWarnings("unchecked")
	private List _mapStringsToTypes(List<String> l)
	throws AlloyXMLParseException {
		List atms = new ArrayList();
		for (int i = 0; i < l.size(); i++) {
			AlloyType _type = _resolveSignatureName(l.get(i));
			if (_type == null) {
				throw new AlloyXMLParseException("Type " + l.get(i) +
						" cannot be resolved");
			} else
				atms.add(_type);
		}
		return atms;
	}
	
	/**
	 * Returns the existing AlloyType instance whose name matches the
	 * first parameter. Return null if no match is found.
	 * @param signature the name of the Alloy signature
	 * @return an existing AlloyType instance that matches the name
	 * @throws AlloyXMLParseException if more than one match is found
	 */
	private AlloyType _resolveSignatureName(String signature)
	throws AlloyXMLParseException {
		if (signature == null) return null;
		if (signature.equals(_retrieveShortName(signature))) {
			Iterator typeIte = _typeNames.keySet().iterator();
			String lastMatch = null;
			while (typeIte.hasNext()) {
				String t = (String)(typeIte.next());
				if (signature.equals(_retrieveShortName(t))) {
					if (lastMatch != null) {
						throw new AlloyXMLParseException(
								"Cannot resolve type name: " + signature +
								" matches more than one type");
					} else lastMatch = t;
				}
			}
			typeIte = _subsettoatom.keySet().iterator();
			while (typeIte.hasNext()) {
				String t = (String)(typeIte.next());
				if (signature.equals(_retrieveShortName(t))) {
					if (lastMatch != null) {
						throw new AlloyXMLParseException(
								"Cannot resolve type name: " + signature +
								" matches more than one type");
					} else lastMatch = UNIV_SIG_NAME;
				}
			}
			return _typeNames.get(lastMatch);
		} else {
			AlloyType ret = _typeNames.get(signature);
			if (_subsettoatom.keySet().contains(signature))
				ret = _typeNames.get(UNIV_SIG_NAME);
			return ret;
		}
	}
}
