package com.sun.corba.se.ActivationIDL;

/**
* com/sun/corba/se/ActivationIDL/RepositoryHolder.java .
* Generated by the IDL-to-Java compiler (portable), version "3.1"
* from ../../../../../../src/share/classes/com/sun/corba/se/ActivationIDL/activation.idl
* Tuesday, May 9, 2006 1:28:34 PM PDT
*/

public final class RepositoryHolder implements org.omg.CORBA.portable.Streamable
{
  public com.sun.corba.se.ActivationIDL.Repository value = null;

  public RepositoryHolder ()
  {
  }

  public RepositoryHolder (com.sun.corba.se.ActivationIDL.Repository initialValue)
  {
    value = initialValue;
  }

  public void _read (org.omg.CORBA.portable.InputStream i)
  {
    value = com.sun.corba.se.ActivationIDL.RepositoryHelper.read (i);
  }

  public void _write (org.omg.CORBA.portable.OutputStream o)
  {
    com.sun.corba.se.ActivationIDL.RepositoryHelper.write (o, value);
  }

  public org.omg.CORBA.TypeCode _type ()
  {
    return com.sun.corba.se.ActivationIDL.RepositoryHelper.type ();
  }

}