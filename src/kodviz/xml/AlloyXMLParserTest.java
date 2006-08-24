package kodviz.xml;

import java.io.File;
import java.util.Iterator;

import junit.framework.TestCase;
import kodviz.alloyviz.AlloyRelation;
import kodviz.alloyviz.AlloySet;
import kodviz.alloyviz.VizInstance;

public class AlloyXMLParserTest extends TestCase {
	public void testFarmerXML() {
		File f = new File("examples/farmer.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(f);
		assertEquals(3, v.getModel().getModules().size());
		assertEquals(13, v.getAllAtoms().size());
		assertEquals(7, v.getAllRelations().size());
		assertEquals(8, v.getAllTypes().size());
	}
	
	public void testUnionType() {
		File f = new File("examples/uniontype.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(f);				
		Iterator i = v.getAllRelations().iterator();
		int count = 0;
		while (i.hasNext()) {
			if (((AlloyRelation)(i.next())).getName().equals("fc")) {
				count++;
			}
		}
		assertEquals(2, count);
		assertEquals(2, v.getModel().getModules().size());
		assertEquals(4, v.getAllRelations().size());
		assertEquals(3, v.getAllAtoms().size());
		assertEquals(4, v.getAllTypes().size());
	}

	public void testSubsetSig() {
		File f = new File("examples/subset_sig.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(f);
		assertEquals(1, v.getAllSets().size());
		AlloySet z = ((AlloySet)(v.getAllSets().get(0)));
		assertEquals(2, v.getModel().getModules().size());
		assertEquals("Z", z.getName());
		assertEquals("univ", z.getType().getName());
	}
	
	public void testSkolemTest() {
		File f = new File("examples/skolem_test.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(f);
		assertEquals(1, v.getAllSets().size());
		assertEquals(2, v.getAllRelations().size());
		boolean _found_solve_a = false;
		boolean _found_solve_b = false;
		Iterator i = v.getAllRelations().iterator();
		while (i.hasNext()) {
			AlloyRelation r = (AlloyRelation)(i.next());
			if (r.getName().equals("solve_a")) {
				assertEquals(2, r.getArity());
				_found_solve_a = true;
			}
		}
		i = v.getAllSets().iterator();
		while (i.hasNext()) {
			AlloySet s = (AlloySet)(i.next());
			if (s.getName().equals("solve_b")) {
				assertEquals(1, v.getAtomsInSet(s).size());
				_found_solve_b = true;
			}
		}
		assertEquals(true, _found_solve_a);
		assertEquals(true, _found_solve_b);
	}
	
	public void testDerivedTypeTest() {
		File f = new File("examples/opt_spantree.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(f);
		assertEquals(6, v.getModel().getModules().size());
		assertEquals(7, v.getAllTypes().size());
	}
}
