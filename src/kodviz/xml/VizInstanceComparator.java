package kodviz.xml;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kodviz.alloyviz.AlloyModule;
import kodviz.alloyviz.AlloySet;
import kodviz.alloyviz.AlloyType;
import kodviz.alloyviz.Model;
import kodviz.alloyviz.VizInstance;

/**
 * This class was written to test the return values of the AlloyXMLParser,
 * and is not meant to be used anywhere else.
 * @author jbaek
 */

@SuppressWarnings("unchecked")
public class VizInstanceComparator {
	
	protected VizInstanceComparator() {}
	
	/**
	 * Compare two <code>VizInstance</code> objects to see if they are of the
	 * same solution of the same model. This is not a comprehensive test; if
	 * they are the same, it will return true; if they are not, it might
	 * still return true. :-) This methods assumes that the two parameters
	 * are well-formed objects of class <code>VizInstance</code>.
	 * @param a
	 * @param b
	 * @return true if the two VizInstance objects are deemed to be the same
	 */
	protected static boolean compare(VizInstance a, VizInstance b) {
		Model _a = a.getModel();
		Model _b = b.getModel();
		
		Map _moduleMap;
		Map _setMap;
		Map _typeMap = new HashMap();
		
		System.out.println("Starting comparison...");
		
		// Compare modules
		System.out.println(" Comparing module names (" + _a.getModules().size() + ")");
		MapGenerator _moduleMapGenerator = new MapGenerator() {
			public String extractKey(Object o) {
				return ((AlloyModule)o).getName();
			}};
		_moduleMap = _moduleMapGenerator.compareAndMap(_a.getModules(),
				_b.getModules());
		if (_moduleMap == null) {
			System.out.println("  Error: Module names do not match.");
			return false;
		}
		
		// Compare types
		System.out.println(" Comparing types (" + a.getAllTypes().size() + ")");
		Iterator i = _moduleMap.keySet().iterator();
		while (i.hasNext()) {
			final AlloyModule m1 = (AlloyModule)(i.next());
			final AlloyModule m2 = (AlloyModule)(_moduleMap.get(m1));
			MapGenerator _typeMapGenerator = new MapGenerator() {
				public String extractKey(Object o) {
					String _name = ((AlloyType)o).getName();
					if (_name.contains(AlloyXMLParser._SEPARATOR))
						return _name;
					else
						return m1.getName() + AlloyXMLParser._SEPARATOR + _name; 
				}};
			Map _tmpMap = _typeMapGenerator.compareAndMap(m1.getTypes(),
					m2.getTypes());
			if (_tmpMap == null) {
				System.out.println("  Error: Type names in module " +
						m1.getName() + " do not match.");
				return false;
			}
			_typeMap.putAll(_tmpMap);
			System.out.println("  Types in module " + m1.getName() +
					" matched...");
		}
		
		// Compare type hierarchy
		i = _typeMap.keySet().iterator();
		while (i.hasNext()) {
			AlloyType t1 = (AlloyType)(i.next());
			AlloyType t2 = (AlloyType)(_typeMap.get(t1));
			// compare atom numbers
			if (!(a.getAtomsOfType(t1).size()==b.getAtomsOfType(t2).size())) {
				System.out.println("  Error: Type " + t1.getName() +
						" has inconsistent number of atoms.");
				return false;
			} else
				System.out.println("  Number of atoms of type " + t1.getName()
						+ " matched...");
			AlloyType t1_super = _a.getSuperForType(t1);
			if (t1_super == null && _b.getSuperForType(t2) != null ||
					t1_super != null &&
					!_typeMap.get(t1_super).equals(_b.getSuperForType(t2))) {
					System.out.println("  Error: Type hierarchy of " +
							t1.getName() + " does not match");
					return false;
			} else System.out.println("  Type hierarchy of " + t1.getName()
					+ " matched...");
				
		}

		// Compare set names
		System.out.println(" Comparing sets (" + a.getAllSets().size() + ")");
		MapGenerator _setMapGenerator = new MapGenerator() {
			public String extractKey(Object o) {
				return ((AlloySet)o).getName();
			}};
		_setMap = _setMapGenerator.compareAndMap(_a.getSets(),
				_b.getSets());
		if (_setMap == null) {
			System.out.println("  Error: Set names do not match.");
			return false;
		}		
		// compare set element numbers
		i = _setMap.keySet().iterator();
		while (i.hasNext()) {
			AlloySet s1 = (AlloySet)(i.next());
			AlloySet s2 = (AlloySet)(_setMap.get(s1));
			if (a.getAtomsInSet(s1).size() != b.getAtomsInSet(s2).size()) {
				System.out.println("  Error: Set " + s1.getName() + 
						" has an inconsistent number of elements.");
				return false;
			}
		}
		return true;
	}
	
	private static class MapGenerator {
		public Map compareAndMap(List a, List b) {
			Iterator i = a.iterator();
			Map<String, Object> _tmpMap = new HashMap<String, Object>();
			Map<Object, Object> ret = new HashMap<Object, Object>();
			while (i.hasNext()) {
				Object _next = i.next();
				_tmpMap.put(extractKey(_next), _next);
			}
			i = b.iterator();
			while (i.hasNext()) {
				Object _next = i.next();
				if (!_tmpMap.keySet().contains(extractKey(_next))) {
					System.out.println("!!!!!Cannot find " + extractKey(_next));
					return null;
				}
				ret.put(_tmpMap.get(extractKey(_next)), _next);
				_tmpMap.remove(extractKey(_next));
			}
			if (!_tmpMap.isEmpty()) {
				System.out.println("!!!!!Cannot resolve " + _tmpMap.keySet().iterator().next());
				return null;
			}
			return ret;
		}
		// need to override this
		public String extractKey(Object _a) {
			return (new Integer(_a.hashCode())).toString();
		}
		// need to override this
		public boolean isEqual(Object _a, Object _b) {
			return _a.equals(_b);
		}
		
	}
}
