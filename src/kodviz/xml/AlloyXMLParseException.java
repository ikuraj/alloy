package kodviz.xml;

public class AlloyXMLParseException extends Exception
{
	private static final long serialVersionUID = 1L;

	public AlloyXMLParseException() {
		super("Detected a malformed element");
	}
	
	public AlloyXMLParseException(String msg) {
		super(msg);
	}
}
