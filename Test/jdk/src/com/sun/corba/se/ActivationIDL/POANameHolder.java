package com.sun.corba.se.ActivationIDL;


/**
* com/sun/corba/se/ActivationIDL/POANameHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from ../../../../../../src/share/classes/com/sun/corba/se/ActivationIDL/activation.idl
* Tuesday, May 9, 2006 1:28:33 PM PDT
*/

public final class POANameHolder implements org.omg.CORBA.portable.Streamable
{
  public String value[] = null;

  public POANameHolder ()
  {
  }

  public POANameHolder (String[] initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = com.sun.corba.se.ActivationIDL.POANameHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    com.sun.corba.se.ActivationIDL.POANameHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return com.sun.corba.se.ActivationIDL.POANameHelper.type ();
  }

}