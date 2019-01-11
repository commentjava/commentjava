/*
 * @(#)Direct-X-Buffer.java	1.45 03/04/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

// -- This file was mechanically generated: Do not edit! -- //

package java.nio;

import sun.misc.Cleaner;
import sun.misc.Unsafe;
import sun.nio.ch.DirectBuffer;
import sun.nio.ch.FileChannelImpl;


class DirectIntBufferRS



    extends DirectIntBufferS

    implements DirectBuffer
{






















































































































    // For duplicates and slices
    //
    DirectIntBufferRS(DirectBuffer db,	        // package-private
			       int mark, int pos, int lim, int cap,
			       int off)
    {








	super(db, mark, pos, lim, cap, off);

    }

    public IntBuffer slice() {
	int pos = this.position();
	int lim = this.limit();
	assert (pos <= lim);
	int rem = (pos <= lim ? lim - pos : 0);
	int off = (pos << 2);
	return new DirectIntBufferRS(this, -1, 0, rem, rem, off);
    }

    public IntBuffer duplicate() {
	return new DirectIntBufferRS(this,
					      this.markValue(),
					      this.position(),
					      this.limit(),
					      this.capacity(),
					      0);
    }

    public IntBuffer asReadOnlyBuffer() {








	return duplicate();

    }


















































    public IntBuffer put(int x) {




	throw new ReadOnlyBufferException();

    }

    public IntBuffer put(int i, int x) {




	throw new ReadOnlyBufferException();

    }

    public IntBuffer put(IntBuffer src) {




































	throw new ReadOnlyBufferException();

    }

    public IntBuffer put(int[] src, int offset, int length) {






















	throw new ReadOnlyBufferException();

    }
    
    public IntBuffer compact() {











	throw new ReadOnlyBufferException();

    }

    public boolean isDirect() {
	return true;
    }

    public boolean isReadOnly() {
	return true;
    }











































    public ByteOrder order() {

	return ((ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN)
		? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);





    }


























}