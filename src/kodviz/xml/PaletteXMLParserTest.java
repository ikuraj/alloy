package kodviz.xml;

import java.io.File;
import java.util.Iterator;
import java.util.Set;

import junit.framework.TestCase;
import kodviz.alloyviz.AlloyNodeElement;
import kodviz.alloyviz.AlloyRelation;
import kodviz.alloyviz.ViewPalette;
import kodviz.alloyviz.VizInstance;
import kodviz.alloyviz.VizMap;
import kodviz.alloyviz.VizState;

public class PaletteXMLParserTest extends TestCase {
	
	@SuppressWarnings("unchecked")
	public void testMergeXML() {
		File inst = new File("examples/farmer.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(inst);
		try {
			String oldPal =	PaletteXMLParser.fileToString(new File(
					"examples/farmer-overwrite1.pal"));
			String newPal = PaletteXMLParser.fileToString(new File(
					"examples/farmer-overwrite2.pal"));
			PaletteXMLParser p = new PaletteXMLParser(
					new VizState(v.getModel(), v), null);
			ViewPalette vp = p.readXML(PaletteXMLParser.reconcilePalettes_Alloy4(newPal,oldPal));
			assertEquals(1, vp.getAllViews().size());
			VizMap myVizMap = vp.getCurrentView().getModelView().getVizMap();
			Iterator i = myVizMap.getNodes().iterator();
			while (i.hasNext()) {
				AlloyNodeElement elt = (AlloyNodeElement)(i.next());
				if (elt.getName().equals("Chicken")) {
					assertEquals(null,
							myVizMap.getNodeViz(elt).getLabel());
					assertEquals("House", 
							myVizMap.getNodeViz(elt).getShape().toString());
				} else if (elt.getName().equals("Fox")) {
					assertEquals("Circle", 
							myVizMap.getNodeViz(elt).getShape().toString());
				} else if (elt.getName().equals("Farmer")) {
					assertEquals("House", 
							myVizMap.getNodeViz(elt).getShape().toString());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			assertTrue(false);
		}
	}
	public void testFarmerEmptyView() {
		File inst = new File("examples/farmer.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(inst);
		File f = new File("examples/empty.pal");
		try {
			// ViewPalette old = new ViewPalette("Old");
			VizState vs = new VizState(v.getModel(), v);
			PaletteXMLParser p = new PaletteXMLParser(
					new VizState(v.getModel(), v),
					vs.getView());
			
			ViewPalette ret = p.readXML(f);
			assertEquals(ret.getCurrentView().getName(), "empty");
			// check that the old types still linger
			VizMap oldVizMap = vs.getView().getModelView().getVizMap();
			VizMap newVizMap = ret.getCurrentView().getModelView().getVizMap();
			Set oldNodes = oldVizMap.getNodes();
			Set newNodes = newVizMap.getNodes();
			Set oldEdges = oldVizMap.getEdges();
			Set newEdges = newVizMap.getEdges();
			
			assertEquals(oldNodes, newNodes);
			Iterator i = oldNodes.iterator();
			while (i.hasNext()) {
				AlloyNodeElement elt = (AlloyNodeElement)(i.next());
				assertEquals(oldVizMap.getNodeViz(elt),
						newVizMap.getNodeViz(elt));
			}
			assertEquals(oldEdges, newEdges);
			i = oldEdges.iterator();
			while (i.hasNext()) {
				AlloyRelation elt = (AlloyRelation)(i.next());
				assertEquals(oldVizMap.getEdgeViz(elt),
						newVizMap.getEdgeViz(elt));
			}
		} catch (Exception e) {
			e.printStackTrace();
			assertTrue(false);
		}
	}
	public void testFarmerPartialView() {
		File inst = new File("examples/farmer.xml");
		VizInstance v = AlloyXMLParser.readXMLfile(inst);
		File f1 = new File("examples/farmer-partial.pal");
		File f2 = new File("examples/farmer-partial2.pal");
		File f3 = new File("examples/farmer-partial3.pal");
		try {
			// ViewPalette old = new ViewPalette("Old");
			VizState vs = new VizState(v.getModel(), v);
			PaletteXMLParser p = new PaletteXMLParser(
					new VizState(v.getModel(), v),
					vs.getView());
			
			ViewPalette ret1 = p.readXML(f1);
			assertEquals(ret1.getCurrentView().getName(), "partial");
			// check that the old types still linger
			VizMap oldVizMap = vs.getView().getModelView().getVizMap();
			VizMap newVizMap = ret1.getCurrentView().getModelView().getVizMap();
			Set oldNodes = oldVizMap.getNodes();
			Set newNodes = newVizMap.getNodes();
			
			
			assertEquals(oldNodes, newNodes);
			Iterator i = oldNodes.iterator();
			while (i.hasNext()) {
				AlloyNodeElement elt = (AlloyNodeElement)(i.next());
				if (!elt.getName().equals("Chicken")) {
					assertEquals(oldVizMap.getNodeViz(elt),
							newVizMap.getNodeViz(elt));
				} else
					assertEquals("customlabel",
							newVizMap.getNodeViz(elt).getLabel());
			}
			assertEquals(1,
					ret1.getCurrentView().getModelView().getProjectionFrame().
					getProjectedTypes().size());
			
			// load another partial palette
			p.setDefaultView(ret1.getCurrentView());
			ViewPalette ret2 = p.readXML(f2);
			newVizMap = ret2.getCurrentView().getModelView().getVizMap();
			newNodes = newVizMap.getNodes();
			
			assertEquals(oldNodes, newNodes);
			i = oldNodes.iterator();
			while (i.hasNext()) {
				AlloyNodeElement elt = (AlloyNodeElement)(i.next());
				if (!elt.getName().equals("Chicken")) {
					assertEquals(oldVizMap.getNodeViz(elt),
							newVizMap.getNodeViz(elt));
				} else {
					assertEquals("customlabel",
							newVizMap.getNodeViz(elt).getLabel());
					assertEquals("House", 
							newVizMap.getNodeViz(elt).getShape().toString());
				}
			}
			assertEquals(1,
					ret2.getCurrentView().getModelView().getProjectionFrame().
					getProjectedTypes().size());
			
//			 load yet another partial palette
			p.setDefaultView(ret2.getCurrentView());
			ViewPalette ret3 = p.readXML(f3);
			newVizMap = ret3.getCurrentView().getModelView().getVizMap();
			newNodes = newVizMap.getNodes();
			
			assertEquals(oldNodes, newNodes);
			i = oldNodes.iterator();
			while (i.hasNext()) {
				AlloyNodeElement elt = (AlloyNodeElement)(i.next());
				if (!elt.getName().equals("Chicken")) {
					assertEquals(oldVizMap.getNodeViz(elt),
							newVizMap.getNodeViz(elt));
				} else {
					assertEquals("customlabel",
							newVizMap.getNodeViz(elt).getLabel());
					assertEquals("House", 
							newVizMap.getNodeViz(elt).getShape().toString());
				}
			}
			assertEquals(2,
					ret3.getCurrentView().getModelView().getProjectionFrame().
					getProjectedTypes().size());
		} catch (Exception e) {
			e.printStackTrace();
			assertTrue(false);
		}
	}
}