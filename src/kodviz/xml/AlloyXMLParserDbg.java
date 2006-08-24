package kodviz.xml;


public class AlloyXMLParserDbg {
	public static void chk(Object o) throws AlloyXMLParserError {
		chk(o != null);
	}
	public static void chk(boolean b) throws AlloyXMLParserError {
		if (!b) {
			throw new AlloyXMLParserError();
		}
	}
}
