/*
 * @(#)ValueHandlerImpl.java	1.44 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
/*
 * Licensed Materials - Property of IBM
 * RMI-IIOP v1.0
 * Copyright IBM Corp. 1998 1999  All Rights Reserved
 *
 * US Government Users Restricted Rights - Use, duplication or
 * disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */

package com.sun.corba.se.internal.io;

import javax.rmi.CORBA.Util;
import javax.rmi.PortableRemoteObject;

import java.util.Hashtable;
import java.util.Stack;
import java.io.IOException;
import java.util.EmptyStackException;

import com.sun.corba.se.internal.util.Utility;
import com.sun.corba.se.internal.io.IIOPInputStream;
import com.sun.corba.se.internal.io.IIOPOutputStream;
import com.sun.corba.se.internal.util.MinorCodes;
import com.sun.corba.se.internal.util.RepositoryId;
import com.sun.corba.se.internal.util.Utility;

import org.omg.CORBA.TCKind;

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.portable.IndirectionException;
import com.sun.org.omg.SendingContext.CodeBase;
import com.sun.org.omg.SendingContext.CodeBaseHelper;

import java.security.AccessController;
import java.security.PrivilegedAction;
 
import com.sun.corba.se.internal.io.IIOPInputStream.ActiveRecursionManager;

public class ValueHandlerImpl implements javax.rmi.CORBA.ValueHandler {

    public static final short kRemoteType = 0;
    public static final short kAbstractType = 1;
    public static final short kValueType = 2;

    private Hashtable inputStreamPairs = null;
    private Hashtable outputStreamPairs = null;
    private CodeBase codeBase = null;
    private static boolean libraryManagerLoaded = false;
    private boolean useHashtables = true;
    private boolean isInputStream = true;
    private IIOPOutputStream outputStreamBridge = null;
    private IIOPInputStream inputStreamBridge = null;

    public ValueHandlerImpl(){
	if (!libraryManagerLoaded) {
        com.sun.corba.se.internal.io.LibraryManager.load();
	    libraryManagerLoaded = true;
        }
    }

    public ValueHandlerImpl(boolean isInputStream) {
	this();
	useHashtables = false;
	this.isInputStream = isInputStream;
    }

    /**
     * Writes the value to the stream using java semantics.
     * @param out The stream to write the value to
     * @param value The value to be written to the stream
     **/
    public void writeValue(org.omg.CORBA.portable.OutputStream _out, java.io.Serializable value) {
	org.omg.CORBA_2_3.portable.OutputStream out =
	    (org.omg.CORBA_2_3.portable.OutputStream) _out;

	if (!useHashtables)
	    {
		if (outputStreamBridge == null)
		    {
			outputStreamBridge = createOutputStream();
			outputStreamBridge.setOrbStream(out);
		    }
		try {
		    outputStreamBridge.increaseRecursionDepth();
		    writeValueInternal(outputStreamBridge, out, value);
		}
		finally
		    {
			outputStreamBridge.decreaseRecursionDepth();
		    }
		return;
	    }
		
        IIOPOutputStream jdkToOrbOutputStreamBridge = null;

	if (outputStreamPairs == null)
	    outputStreamPairs = new Hashtable();
		
	    jdkToOrbOutputStreamBridge = (IIOPOutputStream)outputStreamPairs.get(_out);
	    if (jdkToOrbOutputStreamBridge == null)
		{
		jdkToOrbOutputStreamBridge = createOutputStream();
		    jdkToOrbOutputStreamBridge.setOrbStream(out);
		    outputStreamPairs.put(_out, jdkToOrbOutputStreamBridge);
		}

        try {
	    jdkToOrbOutputStreamBridge.increaseRecursionDepth();
	    writeValueInternal(jdkToOrbOutputStreamBridge, out, value);
		}
        finally
	    {
		if (jdkToOrbOutputStreamBridge.decreaseRecursionDepth() == 0)
		    {
			outputStreamPairs.remove(_out);
		    }
	    }
    }

    private void writeValueInternal(IIOPOutputStream bridge,
				    org.omg.CORBA_2_3.portable.OutputStream out,
				    java.io.Serializable value)
    {
	Class clazz = value.getClass();

        if (clazz.isArray())
            write_Array(out, value, clazz.getComponentType());
        else
            bridge.simpleWriteObject(value);
    }

    /**
     * Reads a value from the stream using java semantics.
     * @param in The stream to read the value from
     * @param clazz The type of the value to be read in
     * @param sender The sending context runtime
     **/
    public java.io.Serializable readValue(org.omg.CORBA.portable.InputStream _in,
					  int offset, 
					  java.lang.Class clazz, 
					  String repositoryID,
					  org.omg.SendingContext.RunTime _sender)
    {
        // Must use narrow rather than a direct cast to a com.sun
        // class.  Fix for bug 4379539.
	CodeBase sender = CodeBaseHelper.narrow(_sender);

	org.omg.CORBA_2_3.portable.InputStream in = 
	    (org.omg.CORBA_2_3.portable.InputStream) _in;

	if (!useHashtables)
	    {
		if (inputStreamBridge == null)
		    {
			inputStreamBridge = createInputStream();
		        inputStreamBridge.setOrbStream(in);
		        inputStreamBridge.setSender(sender); //d11638
	                // backward compatability 4365188
		        inputStreamBridge.setValueHandler(this); 
		    }
			
		java.io.Serializable result = null;
    	try {
		    inputStreamBridge.increaseRecursionDepth();
		    result = (java.io.Serializable) readValueInternal(inputStreamBridge, in, offset, clazz, repositoryID, sender);
		}
		finally
		    {
			if (inputStreamBridge.decreaseRecursionDepth() == 0)
			    {
                                // Indirections are resolved immediately since
                                // the change to the active recursion manager,
                                // so this will never happen.
                                //
				//if (!inputStreamBridge.recursionManager.isEmpty())
				//    throw new org.omg.CORBA.MARSHAL("Dangling recursive references");
			    }
		    }
		return result;
	    }
            
        IIOPInputStream jdkToOrbInputStreamBridge = null;
        if (inputStreamPairs == null)
            inputStreamPairs = new Hashtable();
		
	    jdkToOrbInputStreamBridge = (IIOPInputStream)inputStreamPairs.get(_in);
	    if (jdkToOrbInputStreamBridge == null)
		{
		jdkToOrbInputStreamBridge = createInputStream();
		    jdkToOrbInputStreamBridge.setOrbStream(in);
		    jdkToOrbInputStreamBridge.setSender(sender); //d11638
	            // backward compatability 4365188
		    jdkToOrbInputStreamBridge.setValueHandler(this); 
		    inputStreamPairs.put(_in, jdkToOrbInputStreamBridge);
		}

	java.io.Serializable result = null;
		
    	try {
			
	    jdkToOrbInputStreamBridge.increaseRecursionDepth();
	    result = (java.io.Serializable) readValueInternal(jdkToOrbInputStreamBridge, in, offset, clazz, repositoryID, sender);
			}
        finally
		    {
		if (jdkToOrbInputStreamBridge.decreaseRecursionDepth() == 0)
			    {
			inputStreamPairs.remove(_in);
			    }
			
		    }

	    return result;
	}

    private java.io.Serializable readValueInternal(IIOPInputStream bridge,
						  org.omg.CORBA_2_3.portable.InputStream in,
						  int offset,
						  java.lang.Class clazz,
						  String repositoryID,
						  com.sun.org.omg.SendingContext.CodeBase sender)
	    {
	java.io.Serializable result = null;
		
	if (clazz == null) {
	    // clazz == null indicates an FVD situation for a nonexistant class
	    if (isArray(repositoryID)){
		read_Array(bridge, in, null, sender, offset);
	    } else {
		bridge.simpleSkipObject(repositoryID, sender);
	    }
	    return result;
	}
		
        if (clazz.isArray())
		    {
		result = (java.io.Serializable)read_Array(bridge, in, clazz, sender, offset);
	    } else
		{
		    result = (java.io.Serializable)bridge.simpleReadObject(clazz, repositoryID, sender, offset);
		    }

	if (result != null)
	    {
		// bridge.recursionManager.handleRecursions(offset, result);
	    }

	return result;
    }

    /**
     * Returns the repository ID for the given RMI value Class.
     * @param clz The class to return a repository ID for.
     * @return the repository ID of the Class.
     **/
    public java.lang.String getRMIRepositoryID(java.lang.Class clz) {
	return RepositoryId.createForJavaType(clz);
    }

    /**
     * Indicates whether the given Class performs custom or
     * default marshaling.
     * @param clz The class to test for custom marshaling.
     * @return True if the class performs custom marshaling, false
     * if it does not.
     **/
    public boolean isCustomMarshaled(java.lang.Class clz) {
	return ObjectStreamClass.lookup(clz).isCustomMarshaled();
    }

    /**
     * Returns the CodeBase for this ValueHandler.  This is used by
     * the ORB runtime.  The server sends the service context containing
     * the IOR for this CodeBase on the first GIOP reply.  The clients
     * do the same on the first GIOP request.
     * @return the SendingContext.CodeBase of this ValueHandler.
     **/
    public org.omg.SendingContext.RunTime getRunTimeCodeBase() {
	if (codeBase != null)
	    return codeBase;
	else {
	    codeBase = new FVDCodeBaseImpl();

	    // backward compatability 4365188
            // set the valueHandler so that correct/incorrect RepositoryID
            // calculations can be done based on the ORB version
            FVDCodeBaseImpl fvdImpl = (FVDCodeBaseImpl) codeBase;
            fvdImpl.setValueHandler(this);
	    return codeBase;
	}
    }


    // methods supported for backward compatability so that the appropriate
    // Rep-id calculations take place based on the ORB version

    /**
     *  Returns a boolean of whether or not RepositoryId indicates
     *  FullValueDescriptor.
     *  used for backward compatability
     */

     public boolean useFullValueDescription(Class clazz, String repositoryID)
	throws IOException
     {
	return RepositoryId.useFullValueDescription(clazz, repositoryID);
     }

     public String getClassName(String id)
     {
        RepositoryId repID = RepositoryId.cache.getId(id);
        return repID.getClassName();
     }

     public Class getClassFromType(String id)
        throws ClassNotFoundException
     {
        RepositoryId repId = RepositoryId.cache.getId(id);
        return repId.getClassFromType();
     }

     public Class getAnyClassFromType(String id)
        throws ClassNotFoundException
     {
        RepositoryId repId = RepositoryId.cache.getId(id);
        return repId.getAnyClassFromType();
     }

     public String createForAnyType(Class cl)
     {
        return RepositoryId.createForAnyType(cl);
     }

     public String getDefinedInId(String id)
     {
        RepositoryId repId = RepositoryId.cache.getId(id);
        return repId.getDefinedInId();
     }

     public String getUnqualifiedName(String id)
     {
        RepositoryId repId = RepositoryId.cache.getId(id);
        return repId.getUnqualifiedName();
     }

     public String getSerialVersionUID(String id)
     {
        RepositoryId repId = RepositoryId.cache.getId(id);
        return repId.getSerialVersionUID();
     }


     public boolean isAbstractBase(Class clazz)
     {
        return RepositoryId.isAbstractBase(clazz);
     }

     public boolean isSequence(String id)
     {
        RepositoryId repId = RepositoryId.cache.getId(id);
        return repId.isSequence();
     }

    /**
     * If the value contains a writeReplace method then the result
     * is returned.  Otherwise, the value itself is returned.
     * @return the true value to marshal on the wire.
     **/
    public java.io.Serializable writeReplace(java.io.Serializable value) {
	return ObjectStreamClass.lookup(value.getClass()).writeReplace(value);
    }

    /**
     * Encapsulates writing of Java char arrays so that the 1.3 subclass
     * can override it without exposing internals across packages.  This
     * is a fix for bug 4367783.
     */
    protected void writeCharArray(org.omg.CORBA_2_3.portable.OutputStream out,
                                char[] array,
                                int offset,
                                int length)
    {
        out.write_wchar_array(array, offset, length);
    }

    private void write_Array(org.omg.CORBA_2_3.portable.OutputStream out, java.io.Serializable obj, Class type) {

        int i, length;

        if (type.isPrimitive()) {
            if (type == Integer.TYPE) {
		int[] array = (int[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_long_array(array, 0, length);
            } else if (type == Byte.TYPE) {
		byte[] array = (byte[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_octet_array(array, 0, length);
            } else if (type == Long.TYPE) {
		long[] array = (long[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_longlong_array(array, 0, length);
            } else if (type == Float.TYPE) {
		float[] array = (float[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_float_array(array, 0, length);
            } else if (type == Double.TYPE) {
		double[] array = (double[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_double_array(array, 0, length);
            } else if (type == Short.TYPE) {
		short[] array = (short[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_short_array(array, 0, length);
            } else if (type == Character.TYPE) {
		char[] array = (char[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		writeCharArray(out, array, 0, length);
            } else if (type == Boolean.TYPE) {
		boolean[] array = (boolean[])((Object)obj);
		length = array.length;
		out.write_ulong(length);
		out.write_boolean_array(array, 0, length);
            } else {
		throw new Error("Invalid primitive type : " + obj.getClass().getName());
            }
        } else if (type == java.lang.Object.class) {
            Object[] array = (Object[])((Object)obj);
            length = array.length;
            out.write_ulong(length);
	    //FD            for (i = 0; i < length; i++) {               
            i = 0;
            for (; i < length; i++) {               
		Util.writeAny(out, array[i]);
	    }
        } else {
            Object[] array = (Object[])((Object)obj);
            length = array.length;
            out.write_ulong(length);
	    int callType = kValueType;
			
	    if (type.isInterface()) { 
		String className = type.getName();
				
		if (java.rmi.Remote.class.isAssignableFrom(type)) {
					
		    // RMI Object reference...
					
		    callType = kRemoteType;
					
		} else if (org.omg.CORBA.Object.class.isAssignableFrom(type)){
					
		    // IDL Object reference...
		    callType = kRemoteType;
					
		} else if (RepositoryId.isAbstractBase(type)) {
		    // IDL Abstract Object reference...
		    callType = kAbstractType;
					
		} else if (ObjectStreamClassCorbaExt.isAbstractInterface(type)) {
					
		        callType = kAbstractType;
	        }
				
	    }
			
	    //FD	    for (i = 0; i < length; i++) {
	    i = 0;
	    for (; i < length; i++) {
				
		switch (callType) {
		case kRemoteType: 
		    Util.writeRemoteObject(out, array[i]);
		    break;
		case kAbstractType: 
		    Util.writeAbstractObject(out,array[i]);
		    break;
		case kValueType:
		    try{
			out.write_value((java.io.Serializable)array[i]);
		    }
		    catch(ClassCastException cce){
			if (array[i] instanceof java.io.Serializable)
			    throw cce;
			else {
			    Utility.throwNotSerializableForCorba(array[i].getClass().getName());
								
			}
		    }
		    break;
		}
	    }
        }
    }

    /**
     * Encapsulates reading of Java char arrays so that the 1.3 subclass
     * can override it without exposing internals across packages.  This
     * is a fix for bug 4367783.
     */
    protected void readCharArray(org.omg.CORBA_2_3.portable.InputStream in,
                                 char[] array,
                                 int offset,
                                 int length)
    {  
        in.read_wchar_array(array, offset, length);
    }

    private java.lang.Object read_Array(IIOPInputStream bridge, 
                                        org.omg.CORBA_2_3.portable.InputStream in,
					Class sequence, 
                                        com.sun.org.omg.SendingContext.CodeBase sender,
                                        int offset) {
		
    	try {
	    // Read length of coming array
            int length = in.read_ulong();
            int i;
            Class type = null;

	    if (sequence == null) {
		//FD		for (i = 0; i < length; i++)
		i = 0;
		for (; i < length; i++)
		    in.read_value();

		return null;
	    }
			
	    type = sequence.getComponentType();

            if (type.isPrimitive()) {
                if (type == Integer.TYPE) {
		    int[] array = new int[length];
		    in.read_long_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Byte.TYPE) {
		    byte[] array = new byte[length];
		    in.read_octet_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Long.TYPE) {
		    long[] array = new long[length];
		    in.read_longlong_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Float.TYPE) {
		    float[] array = new float[length];
		    in.read_float_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Double.TYPE) {
		    double[] array = new double[length];
		    in.read_double_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Short.TYPE) {
		    short[] array = new short[length];
		    in.read_short_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Character.TYPE) {
		    char[] array = new char[length];
		    readCharArray(in, array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else if (type == Boolean.TYPE) {
		    boolean[] array = new boolean[length];
		    in.read_boolean_array(array, 0, length);
		    return ((java.io.Serializable)((Object)array));
                } else {
		    throw new Error("Invalid primitive type : " + sequence.getName());
                }
            } else if (type == java.lang.Object.class) {
		Object[] array = (Object[])java.lang.reflect.Array.newInstance(type, length);
                // Store this object and its beginning position
                // since there might be indirections to it while
                // it's been unmarshalled.
                bridge.activeRecursionMgr.addObject(offset, array);

		//FD                for (i = 0; i < length; i++) {
                i = 0;
                for (; i < length; i++) {
                    Object objectValue = null;
                    try {
                        objectValue = Util.readAny(in);
                    }
                    catch(IndirectionException cdrie) {
                        try {
                            // The CDR stream had never seen the given offset before,
                            // so check the recursion manager (it will throw an
                            // IOException if it doesn't have a reference, either).
                            objectValue = bridge.activeRecursionMgr.getObject(cdrie.offset);
                        } catch (IOException ie) {
                            // Translate to a MARSHAL exception since
                            // ValueHandlers aren't allowed to throw
                            // IOExceptions
                            throw new MARSHAL("Invalid indirection to offset "
                                              + cdrie.offset,
                                              MinorCodes.INVALID_INDIRECTION,
                                              CompletionStatus.COMPLETED_NO);
                        }
                    }
                    
                    array[i] = objectValue;
                }
                return ((java.io.Serializable)((Object)array));
            
            } else {

		Object[] array = (Object[])java.lang.reflect.Array.newInstance(type, length);
                // Store this object and its beginning position
                // since there might be indirections to it while
                // it's been unmarshalled.				
                bridge.activeRecursionMgr.addObject(offset, array);

				// Decide what method call to make based on the type. If
				// it is a type for which we need to load a stub, convert
				// the type to the correct stub type.
				
                int callType = kValueType;
                boolean narrow = false;
				
                if (type.isInterface()) { 
                    boolean loadStubClass = false;
                    // String className = type.getName();
                        
                    if (java.rmi.Remote.class.isAssignableFrom(type)) {
						
                        // RMI Object reference...
                        
                        // className = Utility.stubName(className);
                        callType = kRemoteType;
                        
                        // for better performance, load the stub class once
                        // instead of for each element of the array
                        loadStubClass = true;
                        
                    } else if (org.omg.CORBA.Object.class.isAssignableFrom(type)){
                        
                        // IDL Object reference...
                        
                        // className = Utility.idlStubName(className);
                        callType = kRemoteType;
                        loadStubClass = true;
                        
                    } else if (RepositoryId.isAbstractBase(type)) {
                        // IDL Abstract Object reference...
                        
                        // className = Utility.idlStubName(className);
                        callType = kAbstractType;
                        loadStubClass = true;
                    } else if (ObjectStreamClassCorbaExt.isAbstractInterface(type)) {
                        
                        // RMI Abstract Object reference...
                        
                        // type = null;
                        callType = kAbstractType;
                        
                    }
                    
                    if (loadStubClass) {
                        try {
                            String codebase = Util.getCodebase(type);
                            String repID = RepositoryId.createForAnyType(type);
                            type = Utility.loadStubClass(repID, codebase, type); //d11638
                        } catch (ClassNotFoundException e) {
                            narrow = true;
                        }
                    } else {
                        narrow = true;
                    }
                }
                
		//FD                for (i = 0; i < length; i++) {
                i = 0;
                for (; i < length; i++) {
                    
                    try {
                        switch (callType) {
                        case kRemoteType: 
                            if (!narrow)
                                array[i] = (Object)in.read_Object(type); 
                            else {
                                array[i] = Utility.readObjectAndNarrow(in, type);
                                
                            }
                            break;
                        case kAbstractType: 
                            if (!narrow)
                                array[i] = (Object)in.read_abstract_interface(type); 
                            else {
                                array[i] = Utility.readAbstractAndNarrow(in, type);
                            }
                            break;
                        case kValueType:
                            array[i] = (Object)in.read_value(type);
                            break;
                        }
                    }
                    catch(IndirectionException cdrie) {
                        // The CDR stream had never seen the given offset before,
                        // so check the recursion manager (it will throw an
                        // IOException if it doesn't have a reference, either).
                        try {
                            array[i] = bridge.activeRecursionMgr.getObject(cdrie.offset);
                        } catch (IOException ioe) {
                            // Translate to a MARSHAL exception since
                            // ValueHandlers aren't allowed to throw
                            // IOExceptions
                            throw new MARSHAL("Invalid indirection to offset "
                                              + cdrie.offset,
                                              MinorCodes.INVALID_INDIRECTION,
                                              CompletionStatus.COMPLETED_NO);
                        }
                    }
                    
                }
                
                return ((java.io.Serializable)((Object)array));
                
	    }
			
        } finally {
            // We've completed deserializing this object.  Any
            // future indirections will be handled correctly at the
            // CDR level.  The ActiveRecursionManager only deals with
            // objects currently being deserialized.
            bridge.activeRecursionMgr.removeObject(offset);
        }
    }

    private boolean isArray(String repId){
	return RepositoryId.cache.getId(repId).isSequence();
    }

    protected String getOutputStreamClassName() {
        return "com.sun.corba.se.internal.io.IIOPOutputStream";
    }

    private com.sun.corba.se.internal.io.IIOPOutputStream createOutputStream() {
        return (com.sun.corba.se.internal.io.IIOPOutputStream)AccessController.doPrivileged(new StreamFactory(getOutputStreamClassName()));
    }

    protected String getInputStreamClassName() {
        return "com.sun.corba.se.internal.io.IIOPInputStream";
    }

    private com.sun.corba.se.internal.io.IIOPInputStream createInputStream() {
        return (com.sun.corba.se.internal.io.IIOPInputStream)AccessController.doPrivileged(new StreamFactory(getInputStreamClassName()));
    }

    /**
     * Instantiates a class of the given name using the system ClassLoader
     * as part of a PrivilegedAction.
     *
     * It's private final so hopefully people can't grab it outside of
     * this class.
     *
     * If you're worried that someone could subclass ValueHandlerImpl,
     * install his own streams, and snoop what's on the wire:
     * Someone can do that only if he's allowed to use the feature
     * of installing his own javax.rmi.CORBA.Util delegate (via a
     * JVM property or orb.properties file, read the first time the
     * Util class is used).  If he can do that, he can snoop
     * anything on the wire, anyway, without abusing the
     * StreamFactory class.
     */
    private static final class StreamFactory implements PrivilegedAction {
        private String className;

        public StreamFactory (String _className) {
            className = _className;
        }

        public Object run() {
            try {
                // Note: We must use the system ClassLoader here
                // since we want to load classes outside of the
                // core JDK when running J2EE Pure ORB and
                // talking to Kestrel.
                ClassLoader cl = Thread.currentThread().getContextClassLoader();
                if (cl == null)
                    cl = ClassLoader.getSystemClassLoader();

                Class streamClass = cl.loadClass(className);

                // Since the ClassLoader should cache the class, this isn't
                // as expensive as it looks.
                return streamClass.newInstance();

            } catch(Throwable t) {
                throw new InternalError("Error loading "
                                        + className
                                        + ": "
                                        + t.getClass().getName()
                                        + ": "
                                        + t.getMessage());
            }
        }
    }

    /**
     * Our JDK 1.3 and JDK 1.3.1 behavior subclasses override this.
     * The correct behavior is for a Java char to map to a CORBA wchar,
     * but our older code mapped it to a CORBA char.
     */
    protected TCKind getJavaCharTCKind() {
        return TCKind.tk_wchar;
    }
}

