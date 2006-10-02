package edu.mit.csail.sdg.alloy4.util;

/**
 * This defines a generic MessageHandler interface.
 * 
 * @author Felix Chang
 */
public interface MessageHandler {
	
	/**
	 * Callers should call this method to send a message to a MessageHandler.
	 * The meaning of the argument and the meaning of the return results
	 * are dependent on the specific implementation.
	 */
    public Object handleMessage(String message);    
}
